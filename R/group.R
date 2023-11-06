#' Group embedding vectors
#'
#' Function \code{er_group} condenses the embedding by grouping identical or highly similar objects (rows).
#'
#' @param embedding a \code{numeric} matrix containing a text embedding.
#' @param method a \code{character} string specifying the grouping method. One of \code{c("identity","fuzzy")}. Default is \code{"identity"}.
#' @param threshold a \code{numeric} specifying the threshold for \code{method = "fuzzy"}. The threshold argument defines the quantile of the arccos similarity distribution that is used as the threshold for grouping embedding objects.
#' @param verbose a \code{logical} specifying whether to show messages.
#'
#' @return The function returns a \code{matrix} containing the grouped embedding. The \code{matrix} still has \code{ncol(embedding)} dimensions, but its rows have been reduced due to the grouping. With \code{method = "identity"}, the \code{matrix} gains the attribute \code{frequency} containing the frequency table of each element in the original \code{embedding}. With \code{method = "fuzzy"}, the \code{matrix} gains the new attributes \code{group_size}, which is analogue to \code{frequency}, \code{group_texts}, which contains the texts assigned to the group, and \code{group_min_sim}, which shows the minimum arccos similarity of texts in a group. Furthermore, with \code{method = "fuzzy"}, the \code{text} column will be replaced with generic group labels.
#'
#' @references Wulff, D. U., Aeschbach, S., Hussain, Z., & Mata, R. (2024). embeddeR. psyArXiv
#'
#' @examples
#'
#' # get and group embedding vectors
#' embedding <- er_embed(neo$text) %>%
#'   er_group()
#'
#' @export

er_group <- function(embedding, method = "identity", threshold = .95, verbose = FALSE){

  # run tests
  if(!any(class(embedding) == "matrix")) stop("Argument embedding must be a matrix.")
  if(mode(embedding) != "numeric") stop("Argument embedding must be a numeric matrix.")
  if(!method[1] %in% c("identity","fuzzy")) stop('Argument method must be one of c("identity","fuzzy").')
  if(!is.numeric(threshold) | threshold >= 1 | threshold <= 0) stop('Argument threshold must be a scalar between 0 and 1.')
  if(!is.logical(verbose)) stop('Argument verbose must be of type logical.')

  # Identity -----
  if(method == "identity"){

    # verbose
    if(verbose) cat("\nRunning identity grouping")

    # determine frequencies
    frequencies = table(rownames(embedding)) %>% c()

    # create recduced embedding
    grouped_embedding = embedding[names(frequencies),] %>%
      magrittr::set_attr("frequencies", frequencies)

    }

  # Fuzzy -----
  if(method == "fuzzy"){

    # verbose
    if(verbose) cat("\nRunning fuzzy grouping")

    # produce distance mat
    similarities = er_compare_vectors(embedding, metric = "arccos")

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


  # class
  if(!"embedR" %in% class(grouped_embedding)) {
    class(grouped_embedding) = c("embedR", class(grouped_embedding))
    }

  # out
  grouped_embedding

  }

