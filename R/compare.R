#' Compare embedding vectors
#'
#' Function \code{er_compare_vectors} computes similarities of embedding vectors.
#'
#' @param embedding a \code{numeric} matrix containing a text embedding.
#' @param metric a \code{character} string specifying the type of similarity. One of \code{c("cosine","arccos","pearson","spearman")}. Default is \code{"cosine"}.
#'
#' @return The function returns a \code{matrix} containing the similarities of all pairs of embedding vectors. The \code{matrix} has \code{nrow(embedding)} rows and columns.
#'
#' @references Wulff, D. U., Aeschbach, Hussain, Z., S., & Mata, R. (2024). embeddeR. In preparation.
#'
#' @examples
#'
#' # compute similarity matrix
#' embedding <- compare_vectors(embedding)
#'
#' @export

er_compare_vectors <- function(embedding, metric = "cosine"){

  if(!any(class(embedding) == "matrix")) stop("Argument embedding must be a matrix.")
  if(mode(embedding) != "numeric") stop("Argument embedding must be a numeric matrix.")
  if(!metric[1] %in% c("cosine","arccos","pearson","spearman")) stop('Argument metric must be one of c("cosine","arccos","pearson","spearman").')

  # get cosines
  if(metric[1] == "cosine"){
    dotprod = embedding %*% t(embedding)
    norm = sqrt(rowSums(embedding**2))
    out = dotprod / t(t(norm)) %*% t(norm)
    }

  # get arccos
  if(metric[1] == "arccos"){
    dotprod = embedding %*% t(embedding)
    norm = sqrt(rowSums(embedding**2))
    cosine = dotprod / t(t(norm)) %*% t(norm)
    cosine[cosine > 1] = 1
    cosine[cosine < -1] = -1
    out = 1-acos(cosine)/pi
    }

  # get correlations
  if(metric[1] == "pearson"){ out = cor(t(embedding)) }
  if(metric[1] == "spearman"){ out = cor(t(embedding), method = "spearman") }

  # out
  out

  }


#' Compare embeddings
#'
#' Function \code{er_compare_embeddings} computes the similarity between two or more embeddings.
#'
#' @param embeddings a \code{list} of embedding matrices.
#' @param metric a \code{character} string specifying the type of similarity used to produce similarity matrices from each embedding. One of \code{c("cosine","arccos","pearson","spearman")}. Default is \code{"cosine"}.
#' @param comparison_metric a \code{character} string specifying the type of similarity used to compare similarity matrices. One of \code{c("cosine","arccos","pearson","spearman")}. Default is \code{"spearman"}.
#'
#' @return The function returns a \code{tibble} containing for every pair of embeddings the representational similarities overall (based on the lower triangle) and per row (text).
#'
#' @references Wulff, D. U., Aeschbach, S., Hussain, Z., & Mata, R. (2024). embeddeR. In preparation.
#'
#' @examples
#'
#' # get embedding
#' embedding_1 <- embed(neo$text)
#' embedding_2 <- embed(neo$text, model = "distilbert-base-uncased")
#'
#' # compute similarity
#' compare_embeddings(list(embedding_1, embedding_2))
#'
#' @export

er_compare_embeddings <- function(embeddings,
                               metric = "cosine",
                               comparison_metric = "spearman"){

  if(class(embeddings)[1] != "list") stop("Argument embeddings must be a list.")
  if(any(sapply(embeddings, function(x) class(x)[1]) != "matrix")) stop("Argument embeddings contain matrices.")
  if(any(sapply(embeddings, mode) != "numeric")) stop("Argument embeddings must contain numeric matrices.")
  if(!metric[1] %in% c("cosine","arccos","pearson","spearman")) stop('Argument metric must be one of c("cosine","arccos","pearson","spearman").')
  if(!comparison_metric[1] %in% c("cosine","arccos","pearson","spearman")) stop('Argument comparison_metric must be one of c("cosine","arccos","pearson","spearman").')

  # get similarities
  similarities = lapply(embeddings, compare_vectors, metric = metric)
  if(is.null(names(similarities))) names(similarities) = paste0("emb_",1:length(similarities))

  # determine function
  if(comparison_metric[1] == "cosine") {
    fun = function(x, y) (x %*% y) / (sqrt(sum(x**2)) * sqrt(sum(y**2)))
    }
  if(comparison_metric[1] == "arccos") {
    fun = function(x, y) {
      cos = (x %*% y) / (sqrt(sum(x**2)) * sqrt(sum(y**2)))
      cos[cos > 1] = 1 ; cos[cos < -1] = -1
      1-acos(cos)/pi
      }
    }
  if(comparison_metric[1] == "pearson") {
    fun = function(x, y) cor(x, y)
    }
  if(comparison_metric[1] == "spearman") {
    fun = function(x, y) cor(x, y, method = "spearman")
    }

  # get similarities
  out = list()
  for(i in 1:(length(similarities)-1)){
    for(j in (i+1):length(similarities)){

    # extract similarities
    sim_i = similarities[[i]]
    sim_j = similarities[[j]]

    # get overall
    overall = fun(sim_i[lower.tri(sim_i)],
                  sim_j[lower.tri(sim_j)])

    # get row-wise
    rows = sapply(1:nrow(sim_i), function(k){
      fun(sim_i[k,-k], sim_j[k,-k])
      })

    out[[length(out) + 1]] =
      tibble::tibble(model_i = names(similarities)[i],
                     model_j = names(similarities)[j],
                     type = c("overall", rownames(similarities[[1]])),
                     similarity = c(overall, rows))
    }
  }


  # out
  out %>% do.call(what = rbind)

}







