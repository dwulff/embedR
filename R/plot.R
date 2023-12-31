#' Plot embedding frame
#'
#' Generic function \code{plot} shows the points in a two-dimensional embedding or projection space with options for customization.
#'
#' @param data a \code{data.frame} created using \link[emebedR]{er_frame}.
#' @param dimensions an \code{integer} vector determining the dimensions plotted. Default is \code{c(1, 2)}.
#' @param color an optional \code{character} string specifying the column used to determine point (and label) color.
#' @param size an optional \code{character} string specifying the column used to determine point size.
#' @param point_outline an optional \code{character} specifying the color of an optional point outline.
#' @param point_padding an optional \code{numeric} specifying the point padding.
#' @param point_alpha an optional \code{numeric} specifying the transparency of points.
#' @param label an optional \code{character} string specifying the column used to determine label texts.
#' @param label_size an optional \code{character} string specifying the column used to determine label size.
#' @param label_color an optional \code{numeric} altering the label color relative to point color. Values of \code{label_color > 0} renders labels darker and \code{label_color < 0} lighter. Argument \code{label_color} must be between -1 and 1.
#' @param label_outline an optional \code{numeric} specifying the width of an optional white outline around the text labels. The default is \code{label_outline = NULL} implying no outline.
#' @param label_filter an optional \code{logical} comparison determining which label texts to show in the plot. Can be based on any columns in \code{data}.
#' @param label_padding an optional \code{numeric} specifying the label box padding.
#' @param label_overlaps an optional \code{integer} specifying when to exclude text labels that overlap too many things. Defaults to 10.
#' @param segment_size an optional \code{numeric} specifying the width of the segments.
#' @param segment_type an optional \code{numeric} specifying the type of the segments.
#' @param viridis_set an optional \code{character} string determining the viridis color set. See \link[ggplot2]{scale_color_viridis_d}.
#' @param viridis_limits an optional \code{numeric} vector of length 2 determining the \code{begin} and \code{end} arguments of \link[ggplot2]{scale_color_viridis_d}.
#' @param ... further arguments passed to \link[ggplot2]{geom_point}.
#'
#' @return The function returns a \code{ggplot2} object.
#'
#' @references Wulff, D. U., Aeschbach, S., Hussain, Z., & Mata, R. (2024). embeddeR. In preparation.
#'
#' @examples
#'\dontrun{
#' # get embedding plot
#' neo$text %>%
#'   er_embed() %>%
#'   er_project() %>%
#'   er_frame()
#'   plot()
#'}
#' @method plot embedR_tbl
#'
#' @export

plot.embedR_tbl <- function(data,
                            dimensions = c(1, 2),
                            size = NULL,
                            color = NULL,
                            point_outline = NULL,
                            point_padding = .01,
                            point_alpha = NULL,
                            label = NULL,
                            label_size = 3,
                            label_color = NULL,
                            label_outline = NULL,
                            label_filter = NULL,
                            label_padding = .02,
                            label_overlaps = 10,
                            segment_size = .5,
                            segment_type = 1,
                            viridis_set = "E",
                            viridis_limits = c(0,.9),
                            ...){

  # run data
  if(!any(class(data) == "data.frame")) stop("Argument data must be a data.frame.")
  if(!is.numeric(dimensions) || length(dimensions) != 2 || !all(paste0("dim_",dimensions) %in% names(data))) stop('Argument dimensions must specify integer two indices of existing dimensions in data.')
  if(!is.numeric(label_size) || label_size < 0) stop("Argument label_size must be a positive numeric value.")
  if(!is.numeric(point_padding) || point_padding < 0) stop("Argument pt_padding must be a positive numeric value.")
  if(!is.numeric(label_padding) || label_padding < 0) stop("Argument box_padding must be a positive numeric value.")
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
                                                    fill = "color",
                                                    size = "size",
                                                    label = "label"))

  # add points
  if(is.null(point_outline)){
    p = p + ggplot2::geom_point(pch = 21)
    } else {
    p = p + ggplot2::geom_point(pch = 21, color = point_outline)
    }


  # add size
  if(!is.null(dplyr::enquo(size))){

    # variable name
    name = dplyr::enquo(size) %>%
      rlang::expr_text() %>%
      stringr::str_remove("~")

    # add legend name
    p = p + ggplot2::guides(size = ggplot2::guide_legend(title=name))

  }

  # add colors
  if(!is.null(viridis_set) & "color" %in% names(data)){

    # variable name
    name = dplyr::enquo(color) %>%
      rlang::expr_text() %>%
      stringr::str_remove("~")

    # check type
    fun = ifelse(is.numeric(data %>% dplyr::pull(color)),
                 ggplot2::scale_color_viridis_c,
                 ggplot2::scale_color_viridis_d)

    # check type
    fun2 = ifelse(is.numeric(data %>% dplyr::pull(color)),
                  ggplot2::scale_fill_viridis_c,
                  ggplot2::scale_fill_viridis_d)

    # style colors
    p = p +
      fun(option = viridis_set,
                begin = viridis_limits[1],
                end = viridis_limits[2],
                name = name) +
      fun2(option = viridis_set,
           begin = viridis_limits[1],
           end = viridis_limits[2],
           name = name)

  }

  # build plot
  b = ggplot2::ggplot_build(p)

  # handle labels
  if("label" %in% names(data)){

    # set to zero
    if(is.null(label_color)) label_color = 0

    # figure out color vector
    if(label_color >= 0){
      colors = cmix(b$data[[1]]$fill, "black", rep(label_color, nrow(b$data[[1]])))
      } else {
      colors = cmix(b$data[[1]]$fill, "white", rep(abs(label_color), nrow(b$data[[1]])))
      }

    # plot with custom colors
    p = p + ggrepel::geom_text_repel(point.padding = ggplot2::unit(point_padding,"npc"),
                                     box.padding = ggplot2::unit(label_padding,"npc"),
                                     size = label_size,
                                     min.segment.length = 0,
                                     max.overlaps = label_overlaps,
                                     #bg.color = "white",
                                     #bg.r = point_outline,
                                     color = colors,
                                     segment.size = segment_size,
                                     segment.linetype = segment_type)

    }

  # out
  p + theme_minimal()

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
#'\dontrun{
#' # get embedding plot
#' neo$text %>%
#'   er_embed() %>%
#'   er_project() %>%
#'   plot()
#'}
#' @method plot embedR
#'
#' @export

plot.embedR <- function(embedding, ...){

  # generate frame
  frame = er_frame(embedding)

  # generate plot
  plot.embedR_tbl(frame, ...)

  }
