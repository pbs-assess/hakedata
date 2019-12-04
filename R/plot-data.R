#' Colors for the Hake Assessment
#'
#' Generate the color scheme for the hake assessment
#' such that as more years of historical data are added on
#' that the more recent years are still the same color.
#' For example, the most recent year of data will always be
#' black and the second most recent year will always be red.
#'
#' @param n.cols number of colors to return
#'
#' @importFrom RColorBrewer brewer.pal
#' @export
#' @author Kelli Faye Johnson
#'
#' @return A vector of colours
#'
#' @examples
#' n <- 17
#' plot(data.frame(1:n, 1), col = plot_color(n), pch = 19, cex = 5)
plot_color <- function(n.cols = 10){
  base <- brewer.pal(name = "Set1", n = 9)
  colors <- c(base[(n.cols - 1):1], "#000000")
  if(n.cols > 10 & n.cols < 18){
    extra <- brewer.pal(name = "Set2", n = 7)
    colors <- c(extra[(n.cols - 10):1], rev(base), "#000000")
  }
  if(n.cols >= 18){
    stop(n.cols, " is too many colors, only 17 are allowed.", call. = FALSE)
  }
  return(colors)
}

#' Plot_cumu_catch
#'
#' Plot the cumulative catch for the given years
#'
#' @param d Data frame as output from [get_catch()]
#' @param inc_years Years to include in the plot
#' @param weight_factor Divide catch values by this to scale y-axis
#' @param ylim Vector of 2, Y-axis limits
#' @param horiz_line_spacing spacing to draw horizontal dotted lines at. If NA,
#'  no lines will be plotted
#' @param line_thickness thickness of the lines
#'
#' @return A ggplot object
#' @export
#' @importFrom ggplot2 aes geom_point geom_line scale_color_manual theme element_blank
#'  element_text element_rect scale_y_continuous scale_x_continuous ylab xlab
#'  geom_hline
#' @importFrom dplyr n filter select mutate group_by summarize ungroup transmute
#' @importFrom lubridate year now
#' @importFrom tidyr complete
#' @importFrom scales comma
plot_cumu_catch <- function(d,
                            inc_years = (year(now()) - 5):year(now()),
                            weight_factor = 1e6,
                            ylim = c(0, 100),
                            line_thickness = 1.5,
                            horiz_line_spacing = NA){
  d <- d %>%
    filter(year %in% inc_years) %>%
    group_by(year) %>%
    mutate(cumu_day = seq(1, n()),
           cumu_catch = cumsum(landings) / weight_factor) %>%
    ungroup() %>%
    transmute(year = as.factor(year),
              cumu_day,
              cumu_catch)
  days_in_months <- c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  x_ticks <- cumsum(days_in_months)
  mon <- c(month.abb, month.abb[1])

  colors <- plot_color(length(unique(d$year)))

  g <- ggplot2::ggplot(d) +
    aes(x = cumu_day, y = cumu_catch, color = year) +
    geom_line(size = line_thickness) +
    scale_color_manual(values = colors) +
    theme(legend.position = c(0, 1),
          legend.justification = c(-0.2, 1.15),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.background = element_rect(fill = "white")) +
    scale_y_continuous(labels = comma,
                       limits = ylim) +
    scale_x_continuous(breaks = x_ticks,
                       labels = mon) +
    ylab("Cumulative landings (thousand mt)") +
    xlab("Month")
  if(!is.na(horiz_line_spacing)){
    g <- g + geom_hline(yintercept = seq(0, ylim[2], horiz_line_spacing),
                        linetype = "dashed")
  }
  g
}

#' Spatial plot of data by block
#'
#'
#' @param grd A grid of spatial data as created using the [make_grid()] function
#' @param crs See [contours_as_sfg()]
#' @param extents A data frame with two columns, named 'lon' and 'lat' which represent the
#'   extents of the plotting area. The data frame must have two rows and two columns
#' @param contour_depths A vector of depths to plot. Must be already present in the contour data
#'   ([bc_bathymetry]). If NA, no contours will be plotted
#' @param contour_color Color for the contour lines
#'
#' @return A ggplot object
#' @export
#' @importFrom ggplot2 ggplot aes geom_sf scale_fill_viridis_c coord_sf
#' @importFrom ggspatial annotation_scale annotation_north_arrow north_arrow_orienteering north_arrow_fancy_orienteering
#' @importFrom sf st_as_sf st_crs<- st_coordinates
#' @importFrom rnaturalearth ne_countries
#' @importFrom tibble as_tibble
#' @importFrom grDevices contourLines
plot_spatial <- function(grd,
                         crs = 4326,
                         extents = data.frame(lon = c(-135, -122),
                                              lat = c(48, 55)),
                         contour_depths = c(100, 200, 400, 1000, 1500, 2000),
                         contour_color = "lightblue"){

  world <- ne_countries(scale = "large", returnclass = "sf")
  world_proj <- world %>% `st_crs<-`(crs)
  extents <- st_as_sf(extents, coords = c("lon", "lat")) %>%
    `st_crs<-`(crs) %>%
    st_coordinates() %>%
    as_tibble()

  g <- ggplot(data = world_proj) +
    geom_sf(color = "royalblue", fill = "antiquewhite", size = 1)

  # Contour lines
  bc_isob <- contourLines(bc_bathymetry, levels = contour_depths)
  bc_contours <- contours_as_sfg(bc_isob, contour_depths)
  if(!is.na(contour_depths[1])){
    for(i in seq_along(contour_depths)){
      g <- g +
        geom_sf(data = bc_contours[[i]], color = contour_color)
    }
  }

  g <- g + geom_sf(aes(fill = num_fids),
                   data = grd) +
    scale_fill_viridis_c(option = "C", trans = "sqrt", alpha = 0.4) +
    coord_sf(xlim = extents[[1]],
             ylim = extents[[2]]) +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "bl", which_north = "true",
                           pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering)
  g
}
