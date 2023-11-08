#' Infer category labels
#'
#' \code{er_infer_labels} infers category labels using generative large language models.
#'
#' The models recommended for label inferences, including the default models, are not free for use and using them can result in significant costs. Costs will depend on the size of input texts and the number of labels inferred. The default Hugging Face model, \code{meta-llama/Llama-2-70b-chat-hf}, requires a PRO subscription at a monthly price. The OpenAI models, including the default \code{gpt-4} model, incur costs based on the number of tokens in the input and output.
#'
#' To obtain the best possible labels it is recommended to adjust the prompt arguments \code{role}, \code{system}, and \code{instruct}.
#'
#' @param labels a \code{list} of character vectors.
#' @param api a \code{character} string specifying the api One of \code{c("huggingface","openai","cohere")}. Default is \code{"openai"}.
#' @param model a \code{character} string specifying the model label. Must match the model names on the corresponding APIs. See, \href{https://huggingface.co/models}{huggingface.co/models} and \href{https://platform.openai.com/docs/models/embeddings}{platform.openai.com/docs/models/embeddings}. Defaults to \code{"meta-llama/Llama-2-70b-chat-hf"} for \code{api = "huggingface"} and to \code{"gpt-4"} for \code{api = "openai"}.
#' @param role a \code{character} string specifying the systems role in place of \code{role} in the general system instruction to the model. Default is \code{"assistant"}.
#' @param system a \code{character} string specifying the general system instruction to the model. Default is \code{"You are a helpful {role} who provides short, specific, and accurate category labels."}.
#' @param instruct a \code{character} string specifying the instruction for the model. Must contain the placeholder \code{"{examples}"}. Default is \code{"Generate a specific and accurate one or two word category label that captures the common meaning of the following examples: {examples}. Place '@' before and after the category label."}.
#' @param attempts an optional \code{integer} specifying the number of API request attempts for a given set of input labels.
#' @param verbose a \code{logical} specifying whether to show messages.'
#'
#' @return The function returns a \code{character} vector of category labels.
#'
#' @references Wulff, D. U., Aeschbach, S., Hussain, Z., & Mata, R. (2024). embeddeR. psyArXiv
#'
#' @examples
#'\dontrun{
#' # get labeled results
#' result <- er_embed(neo$text) %>%
#'   er_group() %>%
#'   er_project() %>%
#'   er_frame() %>%
#'   dplyr::mutate(group_labels = er_infer_labels(group_texts))
#'}
#' @export

er_infer_labels <- function(labels,
                            api = "huggingface",
                            model = NULL,
                            role = "assistant",
                            system = NULL,
                            instruct = NULL,
                            attempts = 50,
                            verbose = FALSE){


  # run tests
  if(!class(labels) == "list") stop('Argument labels must be a list.')
  if(any(sapply(labels, class) != "character")) stop('Argument labels must contain character vectors.')
  if(!api[1] %in% c("huggingface","openai")) stop('Argument text must be one of "huggingface" or "openai"')
  if(!is.null(model) && !is.character(model)) stop("Argument model must be a character string.")
  if(!is.null(model) && !is.character(role)) stop("Argument role must be a character string.")
  if(!is.null(model) && !is.character(instruct)) stop("Argument instruct must be a character string.")
  if(!is.null(model) && !is.character(system)) stop("Argument system must be a character string.")
  if(!is.logical(verbose)) stop('Argument verbose must be of type logical.')

  # container
  generated_labels = character(length(labels))

  # fix quotation marks
  labels = lapply(labels, function(x) stringr::str_replace_all(x, '"', "'"))

  # set instruct & system
  if(is.null(instruct)){
    instruct = "Generate a specific and accurate one- or two- word category label that summarizes the following examples: {examples}, The best label can be among the examples. Place '@' before and after the label."
    }
  if(is.null(system)){
    system = "You are a helpful {role}."
    }

  # HUGGINGFACE -------
  if(verbose) cat("\nInferring with Hugging Face")
  if(api == "huggingface"){

    # does token exist
    if(!"huggingface" %in% suppressMessages(er_get_tokens())$api) stop("Token of huggingface is missing. Set using er_set_tokens().")

    # setup progress bar
    bar = progress::progress_bar$new(format = "Inferring labels [:bar] :percent eta: :eta", total = 100, clear = FALSE, width= 60)
    bar$tick(0)

    # iterate through labels
    for(i in 1:length(labels)){

      # do attempts
      for(j in 1:attempts){

        # break if attempts reached
        if(j == attempts) stop("Too many errors. See messages printed in console.")

        # setup prompt
        examples = paste0('"',labels[[i]],'"') %>% paste0(collapse = ", ")
        instruct_i = glue::glue(instruct)
        system_i = glue::glue(system)
        prompt = glue::glue("<s>[INST] <<SYS>>{system_i}<</SYS>> {instruct_i} [/INST]")

        # setup api
        if(is.null(model)) model = "meta-llama/Llama-2-70b-chat-hf"
        api = glue::glue("https://api-inference.huggingface.co/pipeline/text-generation/{model}")
        token = options()$huggingface_token

        # run post
        post = httr::POST(url = api,
                          httr::add_headers(Authorization = glue::glue("Bearer {token}"),
                                            "Content-Type" = "application/json"),
                          body = jsonlite::toJSON(prompt))

        # 429 = overloaded

        if(post$status_code == 200){

          # get label
          generated_labels[i] = httr::content(post, as = "text", encoding = "UTF-8") %>%
            stringr::str_remove("'@'") %>%
            stringr::str_extract("@[^@]+@") %>%
            stringr::str_remove_all("@") %>%
            stringr::str_trim()

          # break inner loop
          break

          } else {

          # show error
          message(httr::content(post, as = "text", encoding = "UTF-8"))
          }
        }

      bar$update(i/length(labels))

      }
    }


  # OPENAI -------
  if(verbose) cat("\nInferring with OpenAI")
  if(api == "openai"){

    # does token exist
    if(!"openai" %in% suppressMessages(er_get_tokens())$api) stop("Token of openai is missing. Set using er_set_tokens().")

    # setup progress bar
    bar = progress::progress_bar$new(format = "Inferring labels [:bar] :percent eta: :eta", total = 100, clear = FALSE, width= 60)
    bar$tick(0)

    # iterate through labels
    for(i in 1:length(labels)){

      # do attempts
      for(j in 1:attempts){

        # break if attempts reached
        if(j == attempts) stop("Too many errors. See messages printed in console.")

        # setup prompt
        if(is.null(model)) model = "gpt-4"

        # generate prompt
        examples = paste0("\'",labels[[i]],"\'") %>% paste0(collapse = ", ")
        instruct_i = glue::glue(instruct)
        system_i = glue::glue(system)
        prompt = '{"model": "MODEL","messages": [{"role": "system","content": "SYSTEM"},{"role": "user","content": "INSTRUCT"}]}'
        prompt = stringr::str_replace(prompt, "MODEL", model)
        prompt = stringr::str_replace(prompt, "SYSTEM", system_i)
        prompt = stringr::str_replace(prompt, "INSTRUCT", instruct_i)

        # setup api
        api = glue::glue("https://api.openai.com/v1/chat/completions")
        token = options()$openai_token

        # run post
        post = httr::POST(url = api,
                          httr::add_headers(Authorization = glue::glue("Bearer {token}"),
                                            "Content-Type" = "application/json",
                                            accept = "application/json"),
                          body = prompt)

        if(post$status_code == 429) stop("Insufficient quota. You exceeded your OpenAI funds.")
        if(post$status_code == 200){

          # get label
          generated_labels[i] = httr::content(post, as = "text", encoding = "UTF-8") %>%
            jsonlite::fromJSON() %>%
            `[[`("choices") %>% `[[`("message") %>%
            unlist() %>% `[`("content") %>% #print() %>%
            stringr::str_remove_all("@") %>%
            stringr::str_trim()

          # break inner loop
          break

        } else {

          # show error
          message(httr::content(post, as = "text", encoding = "UTF-8"))
        }
      }

      bar$update(i/length(labels))

    }
  }


  # out
  generated_labels
  }

