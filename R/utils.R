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


#' Print embedding object
#'
#' Generic function \code{print.embedR} shows a print of the embedding object with an abbreviated \code{matrix} and a \code{tibble} of its attributes.
#'
#' @param col1,col2 color values as label, rgb or hex.
#' @param weight a numeric value specifying the amount of weight given to \code{col1} and \code{col2}
#' @param format a character string specifying the output type. One of \code{c("rgb","hex")}. Default is \code{"hex"}
#' @param ... further arguments passed to or from other methods.
#'

cmix = function(col1, col2, weight, format = 'hex'){

  if(ifelse(is.matrix(col1),nrow(col1),length(col1)) != length(weight) &
     length(col1) != 1){
    stop('Length of col1 must be either 1 or matching the length of weight')
  }
  if(ifelse(is.matrix(col2),nrow(col2),length(col2)) != length(weight) &
     length(col2) != 1){
    stop('Length of col1 must be either 1 or matching the length of weight')
  }
  if(length(weight) == 1){
    if(ifelse(is.matrix(col1),nrow(col1),length(col1)) !=
       ifelse(is.matrix(col2),nrow(col2),length(col2))){
      stop('If length of weight = 1, number of colors in col1 and col2 must match')
    }
  }

  nrows = max(c(ifelse(is.matrix(col1),nrow(col1),length(col1)),
                ifelse(is.matrix(col2),nrow(col2),length(col2)),
                length(weight)))

  if(is.character(col1)){
    if(length(col1) == 1){
      col1 = grDevices::col2rgb(col1)
      col1 = matrix(c(col1),ncol=3,nrow=nrows,byrow=T)
    } else {
      col1 = t(sapply(col1,grDevices::col2rgb))
    }
  } else{
    col1 = matrix(c(col1),ncol=3,nrow=nrows,byrow=F)
  }
  if(is.character(col2)){
    if(length(col2) == 1){
      col2 = grDevices::col2rgb(col2)
      col2 = matrix(c(col2),ncol=3,nrow=nrows,byrow=T)
    } else {
      col2 = t(sapply(col2,grDevices::col2rgb))
    }
  } else{
    col2 = matrix(c(col2),ncol=3,nrow=nrows,byrow=F)
  }


  col = col1 * (1-weight) + col2 * weight

  if(format == 'rgb') return(col)
  if(format == 'hex') return(grDevices::rgb(data.frame(col),maxColorValue = 255))
  if(!format %in% c('rgb','hex')) stop('Choose either "rgb" or "hex" as format')

  }

