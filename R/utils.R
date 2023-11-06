#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Print embedding object
#'
#' Generic function \code{print.embedR} shows a print of the embedding object with an abbreviated \code{matrix} and a \code{tibble} of its attributes.
#'
#' @param x an object of class embedR
#' @param n an \code{integer} specifying the number of rows to print. Default is \code{5}.
#' @param m an \code{integer} specifying the number of columns to print. Default is \code{5}.
#' @param ... further arguments passed to or from other methods.
#'
#' @method print embedR
#' @export

print.embedR = function(x, n = 5, m = 5,...){

  # CONSTRUCT ---

  # get emb list
  emb_list = paste0("\nEmbedding: ",nrow(x)," objects and ",ncol(x)," dimensions.")

  # check attributes
  attr_names = names(attributes(x))
  attr_extras = attr_names[!attr_names %in% options()$embedR_mute_attr]

  # construct attributes
  if(length(attr_extras) > 0){

    # attribute list
    attr_list = paste0("\nAttributes: ", paste0(attr_extras, collapse = ", "))

    # attribute tibble
    attr_tbl = lapply(attr_extras, function(a){
      tbl = tibble::tibble(attr(x, a))
      names(tbl) = a
      tbl}) %>%
      do.call(what = dplyr::bind_cols)

    } else {
    attr_list = NULL
    }

  # PRINT ---

  cat(cli::style_bold("\nembedR object\n"))
  cat(emb_list)
  cat(attr_list)
  cat(cli::style_underline(paste0("\n\n\nEmbedding\n\n"), sep = ""))
  print(x[1:n,1:min(m, ncol(x))])
  if(length(attr_extras) > 0){
    cat(cli::style_underline(paste0("\n\nAttributes\n\n"), sep = ""))
    print(attr_tbl, ...)
    }

  }

