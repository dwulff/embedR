#' Plot embedding frame
#'
#' Generic function \code{plot} shows the points in a two-dimensional embedding or projection space with options for customization.
#'
#' @param data a \code{data.frame} created using \link[emebedR]{er_frame}.
#' @param dimensions an \code{integer} vector determining the dimensions plotted. Default is \code{c(1, 2)}.
#' @param color an optional \code{character} string specifying the column used to determine point (and label) color.
#' @param size an optional \code{character} string specifying the column used to determine point size.
#' @param label an optional \code{character} string specifying the column used to determine label texts.
#' @param label_size an optional \code{character} string specifying the column used to determine label size.
#' @param label_filter an optional \code{logical} comparison determining which label texts to show in the plot. Can be based on any columns in \code{data}.
#' @param pt_padding a \code{numeric} specifying the point padding.
#' @param box_padding a \code{numeric} specifying the label box padding.
#' @param viridis_set a \code{character} string determining the viridis color set. See \link[ggplot2]{scale_color_viridis_d}.
#' @param viridis_limits a \code{numeric} vector of length 2 determining the \code{begin} and \code{end} arguments of \link[ggplot2]{scale_color_viridis_d}.
#'
#' @return The function returns a \code{ggplot2} object.
#'
#' @references Wulff, D. U., Aeschbach, S., Hussain, Z., & Mata, R. (2024). embeddeR. In preparation.
#'
#' @examples
#'
#' # get embedding plot
#' neo$text %>%
#'   er_embed() %>%
#'   er_project() %>%
#'   er_frame()
#'   plot()
#'
#' @method plot embedR_tbl
#'
#' @export

plot.embedR_tbl <- function(data,
                            dimensions = c(1, 2),
                            size = NULL,
                            color = NULL,
                            label = NULL,
                            label_size = 3,
                            label_filter = NULL,
                            pt_padding = .05,
                            box_padding = .05,
                            viridis_set = "E",
                            viridis_limits = c(0,.9)){

  # run data
  if(!any(class(data) == "data.frame")) stop("Argument data must be a data.frame.")
  if(!is.numeric(dimensions) || length(dimensions) != 2 || !all(paste0("dim_",dimensions) %in% names(data))) stop('Argument dimensions must specify integer two indices of existing dimensions in data.')
  if(!is.numeric(label_size) || label_size < 0) stop("Argument label_size must be a positive numeric value.")
  if(!is.numeric(pt_padding) || pt_padding < 0) stop("Argument pt_padding must be a positive numeric value.")
  if(!is.numeric(box_padding) || box_padding < 0) stop("Argument box_padding must be a positive numeric value.")
  if(!viridis_set %in% c("A","B","C","D","E","F","G","H")) stop('Argument viridis_set must be one of c("A","B","C","D","E","F","G","H").')
  if(!is.numeric(viridis_limits) || length(viridis_limits) != 2 || any(viridis_limits > 1) || any(viridis_limits < 0)) stop('Argument viridis_limits must be a numeric vector of length two with values between 0 and 1')

  # define plot x and y
  x = paste0("dim_",dimensions[1])
  y = paste0("dim_",dimensions[2])

  # handle variables
  data = data %>% dplyr::mutate(color = !!dplyr::enquo(color),
                                size = !!dplyr::enquo(size),
                                label = !!dplyr::enquo(label))


  # adjust labels
  label_filter = dplyr::enquo(label_filter)
  if(rlang::expr_text(label_filter)!= "~NULL"){
    data = data %>% dplyr::filter(!(!!label_filter)) %>%
      dplyr::mutate(label = "") %>%
      dplyr::bind_rows(data_false = data %>% dplyr::filter(!!label_filter))
    }

  # generate plot
  p = ggplot2::ggplot(data = data,
                  mapping = ggplot2::aes_string(x = x, y = y ,
                                                color = "color",
                                                size = "size",
                                                label = "label")) +
    ggplot2::geom_point()

  # handle labels
  if(!is.null(label)){
    p = p + ggrepel::geom_text_repel(point.padding = ggplot2::unit(pt_padding,"npc"),
                                     box.padding = ggplot2::unit(box_padding,"npc"),
                                     size = label_size,
                                     min.segment.length = 0,
                                     max.overlaps = 1000)
    }

  # add colors
  if(!is.null(viridis_set) & "color" %in% names(data)){

    # check type
    fun = ifelse(is.numeric(data %>% pull(color)),
                 ggplot2::scale_color_viridis_c,
                 ggplot2::scale_color_viridis_d)

    # style colors
    p = p + fun(option = viridis_set,
                begin = viridis_limits[1],
                end = viridis_limits[2])
    }

  # out
  p + ggplot2::theme_minimal()

  }

#' Plot embedding
#'
#' Generic function \code{plot} shows the points in a two-dimensional embedding or projection space with options for customization.
#'
#' The function wraps around \link[emebedR]{er_frame} and \link[embedR]{plot.embedR_tbl}.
#'
#' @param embedding a \code{numeric} matrix containing a text embedding.
#' @inheritParams plot.embedR_tbl
#'
#' @return The function returns a \code{ggplot2} object.
#'
#' @references Wulff, D. U., Aeschbach, S., Hussain, Z., & Mata, R. (2024). embeddeR. In preparation.
#'
#' @examples
#'
#' # get embedding plot
#' neo$text %>%
#'   er_embed() %>%
#'   er_project() %>%
#'   plot()
#'
#' @method plot embedR
#'
#' @export

plot.embedR <- function(embedding, ...){

  # generate frame
  frame = er_frame(embedding)

  # generate plot
  plot.embedR_tbl(frame, ...)

  }
