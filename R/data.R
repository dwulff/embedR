#' AI association data
#'
#' Excerpt data from Scharowski, Mata, and Wulff (2024) containing laypeople associations of artificial intelligence.
#'
#' @format A \code{tibble} of 2,500 AI-associations with the following columns:
#' \describe{
#'  \item{id}{Participant id}
#'  \item{trial}{Index of response. Every participant produced five associations.}
#'  \item{text}{Text of responses.}
#' }
#'
#' @references Scharowski, S., Mata, R., Wulff, D. U. (2024). Laypeople's associations with AI. In preparation.
"ai"

#' NEO personality items
#'
#' Personality items from the 300-item NEO personality questionnaire obtained from \href{https://ipip.ori.org/}{IPIP}.
#'
#' @format A \code{tibble} of 300 personality items with the following columns:
#' \describe{
#'  \item{instrument}{Abbreviated name of personality questionnaire.}
#'  \item{label}{Scale label. Scales each aggregate ten items}
#'  \item{key}{Direction of item. Used in scale construction.}
#'  \item{text}{Text of personality item.}
#' }
#'
#' @references Wulff, D. U., & Mata, R. (2023, October 12). Automated jingle–jangle detection: Using embeddings to tackle taxonomic incommensurability. https://doi.org/10.31234/osf.io/9h7aw
#' @source \url{https://ipip.ori.org/}
"neo"
