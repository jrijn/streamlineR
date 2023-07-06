#' @import ggplot2
#'
#' @title Publication ready theme
#' @description \itemize{ \item \strong{theme_pubr()}: Create a publication ready
#'  theme \item \strong{theme_pubclean()}: a clean theme without axis lines, to
#'  direct more attention to the data.  \item \strong{labs_pubr()}: Format only
#'  plot labels to a publication ready style \item \strong{theme_classic2()}:
#'  Create a classic theme with axis lines. \item \strong{clean_theme()}: Remove
#'  axis lines, ticks, texts and titles. \item \strong{clean_table_theme()}:
#'  Clean the the theme of a table, such as those created by
#'  \code{\link{ggsummarytable}()}}.
#' @param base_size base font size
#' @param base_family base font family
#' @param border logical value. Default is FALSE. If TRUE, add panel border.
#' @param margin logical value. Default is TRUE. If FALSE, reduce plot margin.
#' @param legend character specifying legend position. Allowed values are one of
#'  c("top", "bottom", "left", "right", "none"). Default is "top" side position.
#'  to remove the legend use legend = "none". Legend position can be also
#'  specified using a numeric vector c(x, y).  In this case it is possible to
#'  position the legend inside the plotting area. x and y are the coordinates of
#'  the legend box. Their values should be between 0 and 1. c(0,0) corresponds
#'  to the "bottom left" and c(1,1) corresponds to the "top right" position. For
#'  instance use legend = c(0.8, 0.2).
#' @param x.text.angle Rotation angle of x axis tick labels. Default value is 0.
#'  Use 90 for vertical text.
#' @param flip logical. If TRUE, grid lines are added to y axis instead of x
#'  axis.
#' @param major_grid logical. If TRUE, major grid lines are added.
#' @param minor_grid logical. If TRUE, minor grid lines are added.
#' @param base_line_size numerical. Specifies the default element_line() size
#'  for plots. Defaults to base_size/20.
#' @param base_rect_size numerical. Specifies the default line size for
#'  element_rect() in plots. Defaults to base_size/20.
#' @param aspect.ratio numerical. Specifies the aspect ratio of the plot.
#' @examples
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point(aes(color = gear))
#'
#' # Default plot
#' p
#'
#' # Use publish()
#' p + publish()
#'
#' @name publish
#' @rdname publish
#' @export
publish <- function(base_size = 12,
                    base_family = "sans",
                    border = TRUE,
                    margin = TRUE,
                    legend = c("right", "top", "bottom", "left", "none"),
                    x.text.angle = 0,
                    flip = FALSE,
                    major_grid = TRUE,
                    minor_grid = FALSE,
                    facet_fill = "grey75",
                    facet_color = NA,
                    facet_text_color = "white",
                    base_line_size = 0.5,
                    base_rect_size = 1,
                    aspect.ratio = 1) {
  half_line <- base_size / 2
  if (!is.numeric(legend)) legend <- match.arg(legend)
  if (x.text.angle > 5) xhjust <- 1 else xhjust <- NULL
  if (border) {
    panel.border <- element_rect(fill = NA, colour = "black",
                                 size = base_rect_size)
    axis.line <- element_blank()
  } else {
    panel.border <- element_blank()
    axis.line <- element_line(colour = "black", size = base_line_size)
  }

  if (margin) {
    plot.margin <- margin(
      half_line, half_line, half_line, half_line
    )
  } else {
    plot.margin <- unit(c(0.5, 0.3, 0.3, 0.3), "mm")
  }

  if (major_grid) {
    panel.grid.major <- element_line(size = base_line_size)
  } else {
    panel.grid.major <- element_blank()
  }

  if (minor_grid) {
    panel.grid.minor <- element_line(size = base_line_size/2)
  } else {
    panel.grid.minor <- element_blank()
  }

  .theme <- theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      aspect.ratio = aspect.ratio,
      panel.border = panel.border,
      panel.grid.major = panel.grid.major,
      panel.grid.minor = panel.grid.minor,
      axis.line = axis.line,
      axis.text = element_text(color = "black", size = base_size),
      legend.key = element_blank(),
      legend.title = element_text(color = "black", size = base_size),
      legend.text = element_text(color = "black", size = base_size*.85),
      strip.background = element_rect(fill = facet_fill, colour = facet_color,
                                      size = base_line_size),
      strip.text = element_text(
        colour = facet_text_color,
        face = "bold",
        margin = margin(
          t = base_size / 2,
          r = base_size / 2,
          b = base_size / 2,
          l = base_size / 2
        )
      ),
      plot.margin = plot.margin,
      legend.position = legend,
      complete = TRUE
    )

  if (x.text.angle != 0) {
    .theme <- .theme + theme(axis.text.x = element_text(angle = x.text.angle,
                                                        hjust = xhjust))
  }

  .theme
}

#' Wong, Bang. ???Color Blindness???. Nature Methods 8, no. 6 (June 2011): 441. https://doi.org/10.1038/nmeth.1618.
#' @title Colorblind friendly palette (black)
#' @rdname colorBlindBlack8
#' @import ggplot2
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species))+
#'  geom_point()+
#'  publish(border = TRUE)+
#'  scale_color_manual(values = colorBlindBlack8)
#' @export
colorBlindBlack8 <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)

#' @title Colorblind friendly palette (grey)
#' @rdname colorBlindGrey8
#' @import ggplot2
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species))+
#'  geom_point()+
#'  publish(border = TRUE)+
#'  scale_color_manual(values = colorBlindGrey8)
#' @export
colorBlindGrey8 <- c(
  "#999999", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)
