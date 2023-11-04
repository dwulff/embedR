#' Run PaCMAP
#'
#' Run PaCMAP dimensionality reduction
#'
#' Function wraps around the PaCMAP Python module found at \href{https://github.com/YingfanWang/PaCMAP}{github.com/YingfanWang/PaCMAP}. Function adapted from \href{https://github.com/milescsmith/ReductionWrappers}{/github.com/milescsmith/ReductionWrappers}.
#'
#' PaCMAP (Pairwise Controlled Manifold Approximation) Maps high-dimensionaldataset to a low-dimensional embedding. For details see \href{https://www.jmlr.org/papers/volume22/20-1061/20-1061.pdf}{jmlr.org/papers/volume22/20-1061/20-1061.pdf}.
#'
#' @param embedding a \code{numeric} matrix containing a text embedding.
#' @param n_components an \code{integer} Dimensions of the embedded space. Default is \code{2}.
#' @param n_neighbors an \code{integer} specifying the number of neighbors considered for nearest neighbor pairs for local structure preservation. Default is \code{10}.
#' @param MN_ratio a \code{numeric} specifying the ratio of mid-near pairs to nearest-neighbor pairs  (e.g. \code{n_neighbors=10}, \code{MN_ratio=0.5} means 5 mid-near pairs). Mid-near pairs are used for global structure preservation. Default is \code{.5}.
#' @param FP_ratio a \code{numeric} specifying the ratio of further pairs to nearest-neighbor pairs (e.g. \code{n_neighbors=10}, \code{FP_ratio=2} means 20 further pairs). Further pairs are used for both local and global structure preservation. Default is \code{2}.
#' @param distance a \code{character} string specifying the distance metric. One of \code{c("euclidean", "manhattan", "angular", "hamming")}. Default is \code{"euclidean"}.
#' @param lr a \code{numeric} specifying the learning rate of the Adam optimizer for embedding. Default is \code{1}.
#' @param num_iters an \code{integer} specifying the number of iterations for the optimization of embedding. Values greater than 250 are recommended. Default is \code{450}.
#' @param verbose a \code{logical} specifying whether to show messages during initialization and fitting. Default is \code{FALSE}.
#' @param apply_pca: a \code{logical} specifying whether to apply PCA on the data before pair construction. Default is \code{FALSE}.
#'
#' @return The function returns a \code{matrix} containing projected coordinates for each embedding vectors. The \code{matrix} has \code{nrow(embedding)} rows and \code{n_components} columns.
#'

pacmap <- function(embedding,
                   n_components   = 2,
                   n_neighbors    = 10,
                   MN_ratio       = 0.5,
                   FP_ratio       = 2.0,
                   distance       = "euclidean",
                   lr             = 1.0,
                   num_iters      = 450,
                   verbose        = FALSE,
                   apply_pca      = TRUE){

  pacmap <-
    pacmap_module$PaCMAP(
      n_components   = as.integer(n_components),
      n_neighbors    = as.integer(n_neighbors),
      MN_ratio       = as.numeric(MN_ratio),
      FP_ratio       = as.numeric(FP_ratio),
      distance       = distance[1],
      lr             = as.numeric(lr),
      num_iters      = as.integer(num_iters),
      verbose        = verbose,
      apply_pca      = apply_pca
    )

  pacmap$fit_transform(embedding)
}
