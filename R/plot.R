#' Create data frame
#'
#' \code{frame} transforms an embedding projections to a data frame, specifically, a \code{tibble}.
#'
#' @param projection a \code{numeric} matrix containing a text embedding projection generated with \code{k = 2}.
#'
#' @return The function returns a \code{tibble} containing a projected embedding. The projection dimensions will be assigned to columns \code{x} and \code{y}.
#'
#' @references Wulff, D. U., Aeschbach, S., & Mata, R. (2024). embeddeR. psyArXiv
#'
#' @examples
#'
#' # embed, project, to tibble
#' projection <- embed(neo$text) %>%
#'   project() %>%
#'   to_tibble()
#'
#' @export

frame <- function(projection){

  # run tests
  if(!any(class(projection) == "matrix")) stop("Argument projection must be a matrix.")
  if(mode(projection) != "numeric") stop("Argument projection must be a numeric matrix.")
  if(ncol(projection) != 2) stop('Argument projection must have exactly two columns.')

  # generate tibble
  out = tibble::tibble(text = rownames(projection),
                       x = projection[,1],
                       y = projection[,2])

  # add attributes exist
  exist = names(attributes(projection))[!names(attributes(projection)) %in% c("dim", "dimnames")]
  for(i in 1:length(exist)){
    append = tibble::tibble(attributes(projection)[[exist[i]]][out$text])
    names(append) = exist[i]
    out = out %>% dplyr::bind_cols(append)
    }


  # out
  out
  }


#' Plot projection
#'
#' \code{project} projects the embedding into a two or more dimensional space.
#'
#' @param embedding a \code{numeric} matrix containing a text embedding.
#' @param method a \code{character} string specifying the type of projection. One of \code{c("mds","umap","pacmap")}. Default is \code{"mds"}.
#' @param k an \code{integer} determining the number of dimensions. Default is \code{2}.
#' @param ... additional parameters handed to the embedding method.
#' @param verbose a \code{logical} indicating whether to show messages.
#'
#' @return The function returns a \code{matrix} containing projected coordinates for each embedding vectors. The \code{matrix} has \code{nrow(embedding)} rows and \code{k} columns.
#'
#' @references Wulff, D. U., Aeschbach, S., & Mata, R. (2024). embeddeR. psyArXiv
#'
#' @examples
#'
#' # get embedding
#' embedding <- embed(neo$text)
#'
#' # project embedding
#' project(embedding)
#'
#' @export

plot <- function(embedding, method = "mds", k = 2, ..., verbose = FALSE){

  # run tests
  if(!any(class(embedding) == "matrix")) stop("Argument embedding must be a matrix.")
  if(mode(embedding) != "numeric") stop("Argument embedding must be a numeric matrix.")
  if(!method[1] %in% c("mds","umap","pacmap")) stop('Argument method must be one of c("mds","umap","pacmap").')
  if(k %% 1 != 0 | k > ncol(embedding)) stop('Argument k must be an integer between 1 and ncol(embedding).')

  # MDS -----
  if(method == "mds"){

    # message
    if(verbose) message("Running MDS.")

    # determine similarity
    similarity = compare_vectors(embedding, metric = "arccos")
    distances = as.dist(1-similarity)

    # run projection
    projection = stats::cmdscale(distances, k = k, ...)

  }

  # out
  rownames(projection) = rownames(embedding)
  projection
}
