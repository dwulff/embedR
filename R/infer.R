#' Reduce embedding or projection
#'
#' \code{to_tibble} transforms an embedding projections to a tibble ready for \code{ggplot2}.
#'
#' Note that the default Hugging Face model (meta-llama/Llama-2-70b-chat-hf) requires a PRO subscription token and that extensive of the gpt-4 model can be expensive.
#'
#' @param labels a \code{list} of character vectors.
#' @param api a \code{character} string specifying the api One of \code{c("huggingface","openai","cohere")}. Default is \code{"openai"}.
#' @param model a \code{character} string specifying the model label. Must match the model names on the corresponding APIs. See, \href{https://huggingface.co/models}{huggingface.co/models}, \href{https://platform.openai.com/docs/models/embeddings}{platform.openai.com/docs/models/embeddings}, \href{https://cohere.com/embeddings}{cohere.com/embeddings}. Defaults to \code{"meta-llama/Llama-2-70b-chat-hf"} for \code{api = "huggingface"} and to \code{"gpt-4"} for \code{api = "openai"}.
#' @param role a \code{character} string specifying the systems role in place of \code{role} in the general system instruction to the model. Default is \code{"assistant"}.
#' @param system a \code{character} string specifying the general system instruction to the model. Default is \code{"You are a helpful and honest \code{role}, who provides specific and accurate category labels based on examples."}.
#' @param instruct a \code{character} string specifying the instruction for the model. Must contain the placeholder \code{"{examples}"}. Default is \code{Generate a specific and accurate category label for the following examples: {examples}. Strictly respond with a single word.}.
#' @param verbose a \code{logical} specifying whether to show messages.'
#'
#' @return The function returns a \code{character} vector of category labels.
#'
#' @references Wulff, D. U., Aeschbach, S., & Mata, R. (2024). embeddeR. psyArXiv
#'
#' @examples
#'
#' # get embedding
#' projection <- embed(neo$text) %>%
#'   project() %>%
#'   to_tibble()
#'
#' @export

infer_labels <- function(labels,
                         api = "huggingface",
                         model = NULL,
                         role = "assistant",
                         instruct = NULL,
                         system = NULL,
                         verbose = FALSE){


  # run tests
  if(!class(labels) == "list") stop('Argument labels must be a list.')
  if(any(sapply(labels, class) != "character")) stop('Argument labels must contain character vectors.')
  if(!api[1] %in% c("huggingface","openai")) stop('Argument text must be one of "huggingface" or "openai"')

  # container
  generated_labels = character(length(labels))

  # HUGGINGFACE -------
  if(verbose) message("Running Hugging Face")
  if(api == "huggingface"){

    # does token exist
    if(!"huggingface" %in% suppressMessages(get_tokens())$api) stop("Token of huggingface is missing. Set using set_tokens().")

    # setup progress bar
    bar = progress::progress_bar$new(format = "  generating labels [:bar] :percent eta: :eta", total = 100, clear = FALSE, width= 60)
    bar$tick(0)

    # iterate through labels
    for(i in 1:length(labels)){

      n_attempts = 11
      for(j in 1:n_attempts){

        # break if n_attempts reached
        if(j == n_attempts) stop("Too many errors. See messages printed in console.")

        # setup prompt
        examples = paste0('"',labels[[i]],'"') %>% paste0(collapse = ", ")
        instruct = glue::glue("Generate a specific and accurate one or two word category label that captures the common meaning of the following examples: {examples}. Place '@' before and after the category label.")
        system = glue::glue("You are a helpful {role} who provides short, specific, and accurate category labels.")
        prompt = glue::glue("<s>[INST] <<SYS>>{system}<</SYS>> {instruct} [/INST]")

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


  # HUGGINGFACE -------
  if(verbose) message("Running OpenAI")
  if(api == "openai"){

    # does token exist
    if(!"openai" %in% suppressMessages(get_tokens())$api) stop("Token of openai is missing. Set using set_tokens().")

    # setup progress bar
    bar = progress::progress_bar$new(format = "  generating labels [:bar] :percent eta: :eta", total = 100, clear = FALSE, width= 60)
    bar$tick(0)

    # iterate through labels
    for(i in 1:length(labels)){

      n_attempts = 11
      for(j in 1:n_attempts){

        # break if n_attempts reached
        if(j == n_attempts) stop("Too many errors. See messages printed in console.")

        # setup prompt
        if(is.null(model)) model = "gpt-4"

        # generate prompt
        examples = paste0("\'",labels[[i]],"\'") %>% paste0(collapse = ", ")
        instruct = glue::glue('Generate a specific and accurate one or two word category label that captures the common meaning of the following examples: {examples}. Place \'@\' before and after the category label.')
        system = glue::glue("You are a helpful {role} who provides short, specific, and accurate category labels.")
        prompt = '{"model": "MODEL","messages": [{"role": "system","content": "SYSTEM"},{"role": "user","content": "INSTRUCT"}]}'
        prompt = stringr::str_replace(prompt, "MODEL", model)
        prompt = stringr::str_replace(prompt, "SYSTEM", system)
        prompt = stringr::str_replace(prompt, "INSTRUCT", instruct)

        # setup api
        api = glue::glue("https://api.openai.com/v1/chat/completions")
        token = options()$openai_token

        # run post
        post = httr::POST(url = api,
                          httr::add_headers(Authorization = glue::glue("Bearer {token}"),
                                            "Content-Type" = "application/json",
                                            accept = "application/json"),
                          body = prompt)

        if(post$status_code == 200){

          # get label
          generated_labels[i] = httr::content(post, as = "text", encoding = "UTF-8") %>%
            jsonlite::fromJSON() %>%
            `[[`("choices") %>% `[[`("message") %>%
            unlist() %>% `[`("content") %>%
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

