#' Generate text embeddings
#'
#' Function \code{er_embed} generates embeddings for text inputs.
#'
#' @param text a \code{character} vector of texts.
#' @param api a \code{character} string specifying the embedding API. One of \code{c("huggingface","openai","cohere")}. Default is \code{"huggingface"}.
#' @param model a \code{character} string specifying the embedding model. Must match the model names in the corresponding APIs. See, \href{https://huggingface.co/models}{huggingface.co/models}, \href{https://platform.openai.com/docs/models/embeddings}{platform.openai.com/docs/models/embeddings}, \href{https://cohere.com/embeddings}{cohere.com/embeddings}. Defaults to \code{"sentence-transformers/all-mpnet-base-v2"} for \code{api = "huggingface"}, to \code{"text-embedding-ada-002"} for \code{api = "openai"}, and to \code{"embed-english-v3.0"} for \code{api = "cohere"}.
#' @param type a \code{character} string specifying the type of Cohere embeddings. One of \code{c("search_document","search_query","classification","clustering")}. Default is \code{"clustering"}. See \href{https://docs.cohere.com/reference/embed}{https://docs.cohere.com/reference/embed}.
#' @param verbose a \code{logical} specifying whether to show messages.
#'
#' @return The function returns a \code{matrix} containing the text embeddings with \code{length(text)} rows and as many columns as there are embedding dimensions.
#'
#' @references Wulff, D. U., Aeschbach, S., Hussain, Z., & Mata, R. (2024). embeddeR. In preparation.
#'
#' @examples
#'#'\dontrun{
#' # run embedding
#' er_embed(neo$text)
#'}
#' @export

er_embed <- function(text, api = "huggingface", model = NULL, type = NULL, verbose = FALSE) {

  # run tests
  if(!class(text) == "character") stop('Argument text must be a "character" vector.')
  if(!api[1] %in% c("huggingface","openai","cohere")) stop('Argument text must be one of c("huggingface","openai", "cohere").')
  if(!is.null(model) && !is.character(model[1])) stop('Argument model must be of type character.')
  if(!is.null(type) && !type[1] %in% c("search_document","search_query","classification","clustering")) stop('Argument type must be one of c("search_document","search_query","classification","clustering").')
  if(!is.logical(verbose)) stop('Argument verbose must be of type logical.')

  # set to unique
  unique_text = unique(text)

  # check if tokens exist
  if(is.null(suppressMessages(er_get_tokens()))) stop("No API tokens exist. Set at least one token with er_set_tokens(). See ?er_set_tokens")

  # HUGGINGFACE -----

  if(api[1] == "huggingface"){

    # does token exist
    if(!"huggingface" %in% suppressMessages(er_get_tokens())$api) stop("Token of huggingface is missing. Set using er_set_tokens().")

    # check uni_text length
    counts = stringr::str_count(unique_text, "[:alpha:]+")
    larger = counts > (512 * .75)
    if(any(larger)){
      warning(paste0(sum(larger), " texts are likely longer than 512 tokens. Hugging face models (e.g., BERT) work best with fewer than 512 tokens."))
    }

    # default model
    if(is.null(model)) model = "sentence-transformers/all-mpnet-base-v2"

    # post
    if(verbose) cat("\nRunning huggingface (model:",model,")", sep = "")
    api = glue::glue("https://api-inference.huggingface.co/pipeline/feature-extraction/{model}")
    token = options()$huggingface_token
    post = httr::POST(url = api,
                      httr::add_headers(Authorization = glue::glue("Bearer {token}"),
                                        "Content-Type" = "application/json"),
                      body = jsonlite::toJSON(unique_text))

    if(post$status_code == 503){

      # handle error
      error = httr::content(post, as = "text", encoding = "UTF-8") %>% jsonlite::fromJSON()
      time = error$estimated
      message(paste0(error$error,glue::glue(". Wait {time} seconds.")))

      # sleep
      Sys.sleep(time + 5)

      # rerun
      post = httr::POST(url = api,
                        httr::add_headers(Authorization = glue::glue("Bearer {token}"),
                                          "Content-Type" = "application/json"),
                        body = jsonlite::toJSON(unique_text))
    }


    if(post$status_code == 200){

      # extract embedding
      emb_raw = httr::content(post, as = "text", encoding = "UTF-8") %>% jsonlite::fromJSON()

      # handle multi-token embeddings
      if(class(emb_raw)[1] == "list"){
        emb = sapply(emb_raw, function(x) x[1,1,]) %>% t()
        } else {
        emb = emb_raw
        }
      }
    rownames(emb) = unique_text

    if(!post$status_code %in% c(200, 503)) stop(paste0("Error:", post$status_code))

    }

  # OPENAI -----

  if(api[1] == "openai"){

    # does token exist
    if(!"openai" %in% suppressMessages(er_get_tokens())$api) stop("Token of openai is missing. Set using er_set_tokens().")

    # set model
    if(is.null(model)) model = "text-embedding-ada-002"

    # do post
    if(verbose) cat("\nRunning openai (model:",model,")", sep = "")
    api = "https://api.openai.com/v1/embeddings"
    token = options()$openai_token
    text_json = jsonlite::toJSON(unique_text)
    input = paste0('{"input": ',text_json,', "model": "',model,'", "encoding_format": "float"}')
    post = httr::POST(url = api,
                      httr::add_headers(Authorization = glue::glue("Bearer {token}"),
                                        "Content-Type" = "application/json"),
                      body = input)

    # get result
    emb = httr::content(post, as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON() %>%
      `[[`("data") %>%
      `[[`("embedding") %>%
      do.call(what = rbind)

    # names
    rownames(emb) = unique_text

  }

  # COHERE -----

  if(api[1] == "cohere"){

    # does token exist
    if(!"cohere" %in% suppressMessages(er_get_tokens())$api) stop("Token of cohere is missing. Set using er_set_tokens().")

    # check text length
    counts = stringr::str_count(unique_text, "[:alpha:]+")
    larger = counts > (512 * .75)
    if(any(larger)){
      warning(paste0(sum(larger), " texts are likely longer than 512 tokens. Cohere models work best with fewer than 512 tokens."))
      }

    # set model
    if(is.null(model)) model = "embed-english-v3.0"
    if(is.null(type)) type = "clustering"

    # post
    api = "https://api.cohere.ai/v1/embed"
    token = options()$cohere_token

    # split to obey 96 text limit
    if(length(unique_text) > 96) {
      ind = seq(0, ceiling(length(unique_text) / 96) - .00001, length = length(unique_text)) %>% floor()
      texts = split(unique_text, ind)
      } else {
      texts = unique_text
      }

    # run through
    if(verbose) cat("\nRunning cohere (model:",model,", type:", type,")", sep = "")
    emb = lapply(unique_text, function(text){

    # do post
    text_json = jsonlite::toJSON(text)
    input = paste0('{"texts": ',text_json,', "model": "', model,'", "input_type": "', type, '", "truncate": "END"}')
    post = httr::POST(url = api,
                      httr::add_headers(authorization = glue::glue("Bearer {token}"),
                                        "content-type" = "application/json",
                                        accept = "application/json"),
                      body = input)

    # get result
    emb = httr::content(post, as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON() %>%
      `[[`("embeddings")

    # names
    rownames(emb) = text

    # out
    emb
    }) %>% do.call(what = rbind)


  }


  # expand and class
  emb = emb[text,]
  if(!"embedR" %in% class(emb)) {
    class(emb) = c("embedR", class(emb))
    }

  # out
  emb
  }




