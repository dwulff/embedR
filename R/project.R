#' Project embedding
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

project <- function(embedding, method = "mds", k = 2, ..., verbose = FALSE){

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

  # MDS -----
  if(method == "umap"){

    # message
    if(verbose) message("Running UMAP.")

    # get settings
    settings = umap::umap.defaults
    settings$n_components = k

    # gather input
    input = dplyr::enexprs(...)

    # overwrite settings
    matched = names(input)[names(input) %in% names(settings)]
    if(length(matched) > 0){
      for(i in 1:length(matched)){
        settings[[matched[i]]] = input[[matched[i]]]
        }
      if(length(matched) < length(input)) warning("Not all ... inputs could be matched to UMAP settings.")
      }

    # run umap
    projection = umap::umap(embedding, config = settings)$layout

  }


  # MDS -----
  if(method == "pacmap"){

    # install python
    if(!reticulate::py_available()){
      reticulate::install_python()
      }

    # gather input
    input = dplyr::enexprs(...)

    # check if environment provided otherwise create
    if(any(c("virtualenv","condaenv") %in% names(input))){
      if("virtualenv" %in% names(input)){
        if(reticulate::virtualenv_exists(input$virtualenv)){
          reticulate::use_virtualenv(input$virtualenv)
          environment = input$virtualenv
          } else {
          stop(paste0("Environment ",input$virtualenv," not found. Create using virtualenv_create()."))
          }
        } else if("condaenv" %in% names(input)){
        if(input$condaenv %in% reticulate::conda_list()$name){
          print("test")
          reticulate::use_condaenv(input$condaenv)
          environment = input$condaenv
          } else {
          stop(paste0("Environment ",input$condaenv," not found. Create using conda_create()."))
          }
        }
      } else {
        reticulate::virtualenv_create("pacmap")
        reticulate::use_virtualenv("pacmap")
        environment = "pacmap"
      }

    # make sure to install if not available
    if(!reticulate::py_module_available("pacmap")){
      reticulate::py_install(packages = "pacmap")
    }

    # run pacmac
    projection = pacmap(embedding, ...)
    }


  # inherit attributes
  exist = names(attributes(embedding))[!names(attributes(embedding)) %in% c("dim", "dimnames")]
  if(length(exist) > 0){
    for(i in 1:length(exist)){
      projection = projection %>%
        magrittr::set_attr(exist[i], attributes(embedding)[[exist[i]]])
      }
    }

  # adapt
  rownames(projection) = rownames(embedding)
  if(!"embedR" %in% class(projection)) {
    class(projection) = c("embedR", class(projection))
    }

  # out
  projection

  }





