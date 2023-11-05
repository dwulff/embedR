#' Cluster embedding or projection vectors
#'
#' \code{cluster} generates clusters of embedding vectors using standard clustering algorithms.
#'
#' @param embedding a \code{numeric} matrix containing a text embedding or embedding projection.
#' @param method a \code{character} string specifying the clustering method One of \code{c("hclust","dbscan","louvain")}. Default is \code{"hclust"}.
#' @param k an \code{integer} specifying the number of clusters for \code{method = "hclust"}.
#' @param eps a \code{numeric} specifying the point distance within clusters for \code{method = "dbscan"}.
#' @param metric a \code{character} string specifying the similarity function used for methods \code{c("hclust","louvain")}.
#' @param ... further arguments passed on to the clustering methods.
#' @param verbose a \code{logical} specifying whether to show messages.'
#'
#' @return The function returns a \code{matrix} containing the input embedding or projection and a new attribute \code{"cluster"}.
#'
#' @references Wulff, D. U., Aeschbach, S., & Mata, R. (2024). embeddeR. psyArXiv
#'
#' @examples
#'
#' # add clustering to embedding
#' projection <- embed(neo$text) %>%
#'   cluster()
#'
#' @export

cluster <- function(embedding, method = "hclust", k = NULL, eps = NULL, metric = "arccos", ..., verbose = FALSE){

  # run tests
  if(!any(class(embedding) == "matrix")) stop("Argument embedding must be a matrix.")
  if(mode(embedding) != "numeric") stop("Argument embedding must be a numeric matrix.")
  if(!method[1] %in% c("hclust","dbscan","louvain")) stop('Argument method must be one of c("hclust","dbscan","louvain").')
  if(!is.null(k) && !(k[1] > 0 & k[1] < nrow(embedding))) stop('Argument k must be an integer > 0 and smaller nrow(embedding).')
  if(!is.null(eps) && !(k[1] > 0)) stop('Argument eps must be a numeric > 0.')
  if(!metric[1] %in% c("cosine","arccos","pearson","spearman")) stop('Argument metric must be one of c("cosine","arccos","pearson","spearman").')
  if(!is.logical(verbose)) stop('Argument verbose must be of type logical.')


  # HCLUST -----

  if(method[1] == "hclust"){

    # test
    if(is.null(k)) stop("Method hclust requires argument k specifying the number of clusters.")

    # get similarities
    similarities = compare_vectors(embedding, metric = metric)
    if(any(similarities < 0)) stop("Negative similarities observed. Choose metric resulting in stricly positive similarities (e.g., arccos)")

    # get distances
    distances = as.dist(1 - similarities)

    # perform cluster
    cluster = stats::hclust(distances, ...)
    clustering = cutree(cluster, k = k)

  }


  # DBSCAN -----

  if(method[1] == "dbscan"){

    # test
    if(is.null(eps)) stop("Method dbscan requires argument eps specifying the point distance within clusters.")

    # perform clustering
    cluster = dbscan::dbscan(embedding, eps = eps, ...)
    clustering = cluster$cluster
    names(clustering) = rownames(embedding)

  }

  # LOUVAIN -----

  if(method[1] == "louvain"){

    # get similarities
    similarities = compare_vectors(embedding, metric = metric)
    if(any(similarities < 0)) stop("Negative similarities observed. Choose metric resulting in stricly positive similarities (e.g., arccos)")

    # construct graph
    graph = igraph::graph_from_adjacency_matrix(similarities, mode = "undirected", weighted = TRUE)

    # perform cluster
    cluster = igraph::cluster_louvain(graph)
    clustering = cluster$membership
    names(clustering) = rownames(embedding)

  }


  # write attribute
  embedding = embedding %>%
    magrittr::set_attr("cluster", clustering)

  # out
  embedding

  }



