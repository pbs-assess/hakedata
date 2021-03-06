#' ggplot2 theme for hake
#'
#' @export
#' @importFrom ggplot2 theme theme_bw element_rect element_blank element_line margin unit alpha
hake_theme <- function(){
  theme_bw() +
    theme(legend.box.background = element_rect(fill = alpha("white", 0.7)),
          legend.box.margin = margin(1, 1, 1, 1, "mm"),
          legend.key = element_blank(),
          legend.margin = margin(),
          legend.text.align = 1,
          panel.grid.major = element_line(colour = "darkgrey", size = 0.2),
          panel.grid.minor = element_line(colour = "darkgrey", size = 0.1),
          legend.background = element_rect(fill = "transparent"),
          #panel.spacing.x=unit(3, "lines"),
          plot.margin = unit(c(0.1, 0.6, 0.1, 0.1), "lines"))
}

#' When the package is loaded, set ggplot theme to hake theme
#'
#' @param pkgname See [.onLoad()]
#' @param libname See [.onLoad()]
#' @export
#' @importFrom ggplot2 theme_set
.onLoad <- function(libname, pkgname) {
  theme_set(hake_theme())
  # sensitivity_colors <- c("#000000", RColorBrewer::brewer.pal(8L, "Dark2"))
  # assign("scale_colour_continuous", ggplot2::scale_colour_viridis_c)
  # assign("scale_fill_continuous", ggplot2::scale_fill_viridis_c)
  # assign("scale_colour_discrete",
  #        function(..., values = sensitivity_colors)
  #          scale_colour_manual(..., values = values),
  #        globalenv())
  # assign("scale_fill_discrete",
  #        function(..., values = sensitivity_colors)
  #          scale_fill_manual(..., values = values),
  #        globalenv())
}
