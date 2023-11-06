#' Set API tokens
#'
#' Function \code{er_set_tokens} sets access tokens of embedding and inference APIs.
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
#' @param ... one or more api-token (name-value) pairs (e.g., huggingface = "TOKEN"). Names can be one or more of \code{c("huggingface","openai","cohere")}.
#' @param hard a \code{logical} specifying whether existing tokens should be overwritten.
#'
#' @references Wulff, D. U., Aeschbach, S., Hussain, Z., & Mata, R. (2024). embeddeR. In preparation.
#'
#' @examples
#'\dontrun{
#' # set hugging face token
#' set_tokens("huggingface" = "TOKEN")
#'}
#' @export

er_set_tokens <- function(..., hard = FALSE){

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
#' Function \code{er_get_tokens} lists existing access tokens of embedding and inference APIs.
#'
#' @return The function returns a \code{tibble} showing previously defined access tokens.
#'
#' @references Wulff, D. U., Aeschbach, S., Hussain, Z., & Mata, R. (2024). embeddeR. In preparation.
#'
#' @examples
#'
#' # retrieve access tokens
#' er_get_tokens()
#'
#' @export

er_get_tokens <- function(){

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





