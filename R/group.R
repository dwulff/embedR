#' Group embedding or projection
#'
#' \code{group} transforms an embedding projections to a tibble ready for \code{ggplot2}.
#'
#' @param embedding a \code{numeric} matrix containing a text embedding or embedding projection.
#' @param method a \code{character} string specifying the reduction method One of \code{c("identity","fuzzy")}. Default is \code{"identity"}.
#' @param threshold a \code{numeric} specifying the threshold for \code{method = fuzzy}.
#' @param verbose a \code{logical} specifying whether to show messages.'
#'
#' @return The function returns a \code{matrix} containing the embedding or projection. The \code{matrix} has \code{ncol(embedding)} dimensions and as many rows as determined by the condensing method. The \code{matrix} will gain the attribute \code{counts} containing the frequency table of each element in the original \code{embedding} or \code{projection}.
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

group <- function(embedding, method = "identity", threshold = .995, verbose = FALSE){

  # run tests
  if(!any(class(embedding) == "matrix")) stop("Argument embedding must be a matrix.")
  if(mode(embedding) != "numeric") stop("Argument embedding must be a numeric matrix.")
  if(!method[1] %in% c("identity","fuzzy")) stop('Argument method must be one of c("identity","fuzzy").')
  if(!is.numeric(threshold) | threshold > 1 | threshold < 0) stop('Argument threshold must be a scalar between 0 and 1.')

  # Identity -----
  if(method == "identity"){

    # determine frequencies
    frequencies = table(rownames(embedding)) %>% c()

    # create recduced embedding
    grouped_embedding = embedding[names(frequencies),] %>%
      magrittr::set_attr("frequencies", frequencies)

    }

  # Fuzzy -----
  if(method == "fuzzy"){

    # produce distance mat
    similarities = compare_vectors(embedding, metric = "arccos")

    # threshold similarities
    eps = 1/20**10
    similarity_cutoff = quantile(similarities[upper.tri(similarities)], threshold) - eps

    # cluster
    cluster = hclust(as.dist(1 - similarities),
                     method = "complete")

    # get clustering
    number_steps = sum(cluster$height <  (1 - similarity_cutoff))
    clustering = cutree(cluster, k = nrow(embedding) - number_steps)

    # determine cliques
    cliques = split(names(clustering), clustering)
    clique_minimum = sapply(cliques, function(x) similarities[x, x] %>% min)

    # reduced embedding
    grouped_embedding = matrix(nrow = length(cliques), ncol = ncol(embedding),
                               dimnames = list(paste0("group_",1:length(cliques)), NULL))
    for(i in 1:length(cliques)){
      grouped_embedding[i,] = colMeans(embedding[cliques[[i]],,drop=FALSE])
      }

    # add frequencies
    frequencies = lengths(cliques)
    names(frequencies) = rownames(grouped_embedding)
    grouped_embedding = grouped_embedding %>%
      magrittr::set_attr("group_size", frequencies)

    # add cliques
    names(cliques) = rownames(grouped_embedding)
    grouped_embedding = grouped_embedding %>%
      magrittr::set_attr("group_texts", cliques)

    # add clique minimums
    names(clique_minimum) = rownames(grouped_embedding)
    grouped_embedding = grouped_embedding %>%
      magrittr::set_attr("group_min_sim", clique_minimum)

  }


  # out
  grouped_embedding

  }

