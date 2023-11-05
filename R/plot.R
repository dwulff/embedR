#' Plot embedding
#'
#' \code{project} projects the embedding into a two or more dimensional space.
#'
#' @param data a \code{data.frame} created using \link[emebedR]{frame}.
#' @param dimensions a \code{integer} vector determining the dimensions plotted. Default is \code{1:2}.
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
#' @method plot embedR_tbl
#'
#' @export

plot.embedR_tbl <- function(data, dimensions = c(1, 2), label = "text", color = NULL, size = NULL,
                            pt_padding = 1, box_padding = 1, label_size = 3, label_filter = NULL){

  # run data
  if(!any(class(data) == "data.frame")) stop("Argument data must be a data.frame.")
  if(!is.numeric(dimensions) || length(dimensions) != 2 || !all(paste0("dim_",dimensions) %in% names(data))) stop('Argument dimensions must specify integer two indices of existing dimensions in data.')

  # define plot x and y
  x = paste0("dim_",dimensions[1])
  y = paste0("dim_",dimensions[2])

  # fix color
  if(is.numeric(data[[color]])) {
    data$color_var = factor(data[[color]])
    } else {
    data$color_var = data[[color]]
    }

  # define label set
  data_label = data
  if(!is.null(label_filter)){
    data_label = data_label %>%
      dplyr::group_by(var = eval(str2lang(label_filter))) %>%
      mutate({{label}} := ifelse(var, .[[{{label}}]], ""))
    }

  ggplot2::ggplot(data = data,
                  mapping = ggplot2::aes_string(x = x,
                                                y = y,
                                                col = "color_var",
                                                size = size,
                                                label = label)) +
    ggplot2::geom_point() +
    ggrepel::geom_text_repel(data = data_label,
                             point.padding = ggplot2::unit(pt_padding,"npc"),
                             box.padding = ggplot2::unit(box_padding,"npc"),
                             size = label_size,
                             min.segment.length = 0,
                             max.overlaps = 100) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_viridis_d(option = "E")


  }
