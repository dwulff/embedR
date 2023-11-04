#' Set API tokens
#'
#' \code{set_tokens} sets access tokens of embedding APIs.
#'
#' To obtain API tokens users must register with the respective services:
#'
#' \itemize{
#'  \item{"Hugging Face"}{Sign up at \href{https://huggingface.co/inference-api}{huggingface.co/inference-api}. Then go to profile and click on settings. Generate access token in the \emph{Access Tokens} sub-menu.}
#'  \item{"OpenAI"}{Sign up at \href{https://openai.com/blog/openai-api}{openai.com/blog/openai-api}. Select API. Click on Personal (top-right) and select \emph{View API keys}. Then \emph{Create new secret key}.}
#'  \item{"Cohere"}{Sign up at \href{https://cohere.com/}{cohere.com}. Select \emph{API keys} in your personal dashboard. Either \emph{Create Trial key} (free of charge, but rate limited) or \emph{Create Production key} (costly, but unlimited). }
#' }
#'
#' \emph{Hugging Face} and \emph{Cohere} both offer free-of-charge rate-limited API use.
#'
#'
#' @param ... one or more api-token (name-value) pairs (e.g., huggingface = "TOKEN"). Names can be one or more of \code{c("huggingface","openai","cohere")}.
#' @param hard a \code{logical} specifying whether tokens should be overwritten.
#'
#' @return The function returns a \code{matrix} containing the similarities of all pairs of the embedding vectors. The \code{matrix} has \code{nrow(embedding)} rows and columns.
#'
#' @references Wulff, D. U., Aeschbach, S., & Mata, R. (2024). embeddeR. psyArXiv
#'
#' @examples
#'
#' # get hugging face token
#' set_tokens("huggingface" = "TOKEN")
#'
#' @export

set_tokens <- function(..., hard = FALSE){

  # gather input
  input = dplyr::enexprs(...)

  # tests
  if(any(names(input) == "")) stop('Input must one or more name-value pairs (e.g., huggingface = "TOKEN")')
  if(!all(names(input) %in% c("huggingface","openai","cohere"))) stop('Names (APIs) must be one or more of huggingface, openai, and cohere.')
  if(any(sapply(input, class) != "character")) stop("Values (tokens) must be of type character.")

  # check if exist
  exist = names(input)[paste0(names(input), "_token") %in% names(options())]
  if(length(exist) > 0){
    if(hard) {
      warning(paste0(paste0(exist, collapse=" and "),ifelse(length(exist)==1," has "," have "), "been replaced."))
      } else {
      stop(paste0(paste0(exist, collapse=" and "), " already ", ifelse(length(exist)==1,"exists","exist"),". Set argument hard = TRUE to overwrite."))
      }
    }

  # set tokens
  names(input) = paste0(names(input),"_token")
  do.call(options, input)
  }


#' Get API tokens
#'
#' \code{get_tokens} lists existing access tokens of embedding APIs.
#'
#' @return The function returns a \code{tibble} showing existing access tokens.
#'
#' @references Wulff, D. U., Aeschbach, S., & Mata, R. (2024). embeddeR. psyArXiv
#'
#' @examples
#'
#' # get embedding
#' embedding <- embed(neo$text)
#'
#' # compute similarity
#' compare_vectors(embedding)
#'
#' @export

get_tokens <- function(){

  # set names
  token_names = paste0(c("huggingface","openai","cohere"), "_token")

  # get tokens
  exist = token_names[token_names %in% names(options())]
  if(length(exist) > 0){
    tibble::tibble(api = stringr::str_replace(exist, "_token", ""),
                   token = options()[exist] %>% unlist())
    } else {
    message("No embedding API tokens found!")
    }

  }





