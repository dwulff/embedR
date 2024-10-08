#' Embed and analyze text
#'
#' The \code{embedR} package is an open-source R package to generate and analyze text embeddings. It gives access to state-of-the-art open and paid APIs from \href{https://huggingface.co/inference-api}{Hugging Face}, \href{https://openai.com/blog/openai-api}{OpenAI}, and \href{https://cohere.com/}{Cohere} to gnerate text embeddings and offers methods to group, project, relabel, and visualize them. The following provides an overview of the package's functions:
#'
#' @section Tokens:
#'
#'   \code{\link{er_set_tokens}} sets access tokens for the APIs of Hugging Face, OpenAI, and Cohere.
#'
#'   \code{\link{er_get_tokens}} shows tokens that have been set during the current session.
#'
#' @section Embed:
#'
#'   \code{\link{er_embed}} generates state-of-the-art text embeddings using the APIs from \href{https://huggingface.co/inference-api}{Hugging Face}, \href{https://openai.com/blog/openai-api}{OpenAI}, and \href{https://cohere.com/}{Cohere}.
#'
#' @section Process:
#'
#'   \code{\link{er_group}} groups identical or highly similar embedding vectors to produce group-based embeddings.
#'
#'   \code{\link{er_project}} projects embeddings into smaller dimensional spaces using MDS, UMAP, or PaCMAP.
#'
#' @section Analyze:
#'
#'   \code{\link{er_compare_vectors}} computes a similarity matrix containing the similarities of all pairs of embedding vectors.
#'
#'   \code{\link{er_compare_embeddings}} computes the representational similarity of pairs of embeddings.
#'
#'   \code{\link{er_cluster}} clusters the embedding vectors into larger groups using hierarchical clustering, dbscan, or louvain clustering.
#'
#' @section Helper:
#'
#'   \code{\link{er_frame}} generates a tibble from the embedding objects including potential attributes.
#'
#'   \code{\link{er_infer_labels}} uses state-of-the-art generative models from \href{https://huggingface.co/inference-api}{Hugging Face} and \href{https://openai.com/blog/openai-api}{OpenAI} to generate category labels for groups of texts.
#'
#' @section Visualize:
#'
#'  \code{\link{plot}} produces a 2D scatter plot of embedding vectors (typically after projection) with options for customization.
#'
#' @section Data:
#'
#'  \code{\link{neo}} data set containing 300 items of the personality questionnaire NEO.
#'
#'  \code{\link{ai}} data set containing 2,500 free associations of artificial intelligence provided by laypeople.
#'
#'
#' @examples
#' \dontrun{
#' # load package
#' library(embedR)
#'
#' # set api tokens
#' er_set_token("openai" = "TOKEN",
#'              "huggingface" = "TOKEN",
#'              "cohere" = "TOKEN")
#'
#' # generate embedding
#' embedding = neo$text %>%
#'
#'   # generate text embedding
#'   er_embed(api = "openai")
#'
#' # analyze embedding
#' result = embedding %>%
#'
#'   # group similar texts
#'   er_group(method = "fuzzy") %>%
#'
#'   # generate 2D projection
#'   er_project(method = "umap") %>%
#'
#'   # cluster projection
#'   er_cluster(method = "louvain") %>%
#'
#'   # produce data frame
#'   er_frame()
#'
#' # re-label text groups
#' result = embedding %>%
#'
#'   # relabel groups
#'   er_mutate(labels = label(group_texts,
#'                            api = "openai"))
#'
#' # visualize
#' result %>% plot()
#' }
#'
#' @docType package
#' @name embedR
#' @useDynLib embedR, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

