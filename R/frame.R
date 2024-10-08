#' Build data frame
#'
#' Function \code{er_frame} transforms the embedding and embedding attributes to a \link[tibble]{tibble} of class \code{embedR_tbl}.
#'
#' @param embedding a \code{numeric} matrix containing a text embedding.
#'
#' @return The function returns a \code{tibble} containing the embedding and its attributes.
#'
#' @references Wulff, D. U., Aeschbach, S., Hussain, Z., & Mata, R. (2024). embeddeR. psyArXiv
#'
#' @examples
#'\dontrun{
#' # embed, project, and frame
#' tbl <- er_embed(neo$text) %>%
#'   er_project() %>%
#'   er_frame()
#'}
#' @export

er_frame <- function(embedding){

  # run tests
  if(!any(class(embedding) == "matrix")) stop("Argument embedding must be a matrix.")
  if(mode(embedding) != "numeric") stop("Argument embedding must be a numeric matrix.")

  # warning
  if(ncol(embedding) > 2) warning("With ncol(embedding) > 2 the object can be large")

  # turn embedding to tibble
  if(is.null(colnames(embedding))) colnames(embedding) = paste0("dim_",1:ncol(embedding))

  # remove class for better transfer
  if("embedR" %in% class(embedding)) class(embedding) = class(embedding)[class(embedding) != "embedR"]
  embedding_tbl = tibble::as_tibble(embedding)

  # generate tibble
  out = tibble::tibble(text = rownames(embedding)) %>%
    dplyr::bind_cols(embedding_tbl)

  # generate and add attribute tibble
  attr_embed = names(attributes(embedding))
  attr_exist = attr_embed[!attr_embed %in% options()$embedR_mute_attr]

  if(length(attr_exist) > 0){

    # generate tibble
    attr_tbl = lapply(attr_exist, function(a){
      tbl = tibble::tibble(attr(embedding, a)[out$text])
      names(tbl) = a
      tbl}) %>%
      do.call(what = dplyr::bind_cols)

    # add tibble
    out = out %>% dplyr::bind_cols(attr_tbl)
  }

  # set class
  class(out) = c("embedR_tbl", class(out))

  # out
  out
}


#' Build data frame
#'
#' Function \code{er_unframe} transforms the embedding and embedding attributes to a \link[tibble]{tibble} of class \code{embedR_tbl}.
#'
#' @param frame a \code{data.frame} containing a text embedding.
#'
#' @return The function returns a \code{matrix} containing the embedding and its attributes.
#'
#' @references Wulff, D. U., Aeschbach, S., Hussain, Z., & Mata, R. (2024). embeddeR. psyArXiv
#'
#' @examples
#'\dontrun{
#' # embed, project, and frame
#' tbl <- er_embed(neo$text) %>%
#'   er_project() %>%
#'   er_frame()
#'}
#' @export

er_unframe <- function(embedding){





}
