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
#' @param grd A list of length 2, the output of the [make_grid()] function
#' @param data_col The column in the `grd` data frame to use as the plotting value
#' @param data_factor Value to divide the `data_col` values by in the `grd` data frame
#' @param crs See [contours_as_sfg()]
#' @param extents A data frame with two columns, named 'lon' and 'lat' which represent the
#'   extents of the plotting area. The data frame must have two rows and two columns
#' @param contour_depths A vector of depths to plot. Must be already present in the contour data
#'   ([bc_bathymetry]). If NA, no contours will be plotted
#' @param contour_color Color for the contour lines
#' @param contour_thickness Thickness of contour lines
#'
#' @return A ggplot object
#' @export
#' @importFrom ggplot2 ggplot aes geom_sf scale_fill_viridis_c coord_sf aes_string
#' @importFrom ggspatial annotation_scale annotation_north_arrow north_arrow_orienteering north_arrow_fancy_orienteering
#' @importFrom sf st_as_sf st_crs<- st_coordinates
#' @importFrom rnaturalearth ne_countries
#' @importFrom tibble as_tibble
#' @importFrom grDevices contourLines
plot_spatial <- function(grd,
                         data_col = "num_fids",
                         data_factor = 1,
                         crs = 4326,
                         extents = data.frame(lon = c(-135, -122),
                                              lat = c(48, 55)),
                         contour_depths = c(100, 200, 400, 1000, 1500, 2000),
                         contour_color = "lightblue",
                         contour_thickness = 0.25){

  num_removed <- grd[[2]]
  grd <- grd[[1]]

  world <- ne_countries(scale = "large", returnclass = "sf")
  world_proj <- world %>% `st_crs<-`(crs)
  extents <- st_as_sf(extents, coords = c("lon", "lat")) %>%
    `st_crs<-`(crs) %>%
    st_coordinates() %>%
    as_tibble()

  g <- ggplot(data = world_proj) +
    geom_sf(color = "black", fill = "antiquewhite")

  # Contour lines
  bc_isob <- contourLines(bc_bathymetry, levels = contour_depths)
  bc_contours <- contours_as_sfg(bc_isob, contour_depths)
  if(!is.na(contour_depths[1])){
    for(i in seq_along(contour_depths)){
      g <- g +
        geom_sf(data = bc_contours[[i]], color = contour_color, size = contour_thickness)
    }
  }

  grd[[data_col]] <- grd[[data_col]] / data_factor

  g <- g + geom_sf(aes_string(fill = data_col),
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

#' Plot depth boxplots by year (catchdate) or histograms for a single year
#'
#' @param d The data frame as returned by the spatial SQL queries found in this in
#'   the variable `spatial_catch_sql_file`
#' @param plot_type Type of plot, one of 'hist' or 'box'
#' @param fishery_type One of NA, 'ft', 'ss', or 'jv'. If NA, all fisheries will be plotted against one another,
#'   if one of the others, that will be filtered before plotting
#' @param bin_width See [ggplot2::geom_histogram()]
#' @param xlim Limits for the x-axis. For c(NA, NA), plot will extend to limits of data
#' @param alpha Transparency of fill from 0 - 1
#' @param legend_loc Where to place legend "inside" or "outside" the frame of the plot
#' @param depth_type Type of depth to plot: 'bottom' or 'gear'.
#' @param depth_max Maximum depth. Remove records with depth greater than this. IF `NA`,
#' no restrictions will be placed on the depth.
#' @param show_legend Logical for inclusion of legend
#'
#' @return A [ggplot2] object
#' @export
#' @importFrom dplyr filter mutate summarize
#' @importFrom ggplot2 aes geom_boxplot geom_histogram xlab ylab guides guide_legend scale_x_continuous geom_vline
#' @importFrom grid unit
#' @importFrom stats quantile
#' @importFrom scales comma
plot_depths <- function(d,
                        plot_type = "hist",
                        depth_type = "bottom",
                        fishery_type = NA,
                        depth_max = 1500,
                        bin_width = 10,
                        xlim = c(NA, NA),
                        ylim = c(NA, NA),
                        alpha = 0.6,
                        show_legend = FALSE,
                        legend_loc = "inside"){
  stopifnot(plot_type %in% c("hist", "box"))
  stopifnot(depth_type %in% c("bottom", "gear"))
  stopifnot(fishery_type %in% c("ft", "ss", "jv", NA))

  if(!is.na(fishery_type)){
    d <- d %>%
      filter(fishery %in% fishery_type)
  }
  depth_type <- paste0(depth_type, "depth_fm")

  d <- d %>%
    rename(fdep = !!depth_type) %>%
    filter(!is.na(fdep)) %>%
    mutate(yr = factor(year(catchdate)),
           fishery = toupper(fishery),
           fdep = fdep * 1.8288) # Convert fathoms to meters
  if(!is.na(depth_max)){
    d <- d %>%
      filter(fdep <= depth_max)
  }

  if(plot_type == "hist"){
    med <- d %>%
      group_by(fishery) %>%
      summarize(low = quantile(fdep)[2],
                hi = quantile(fdep)[4],
                fdep = quantile(fdep)[3])
    g <- ggplot(data = d) +
      geom_histogram(aes(x = fdep, fill = fishery),
                     color = "#e9ecef",
                     alpha = alpha,
                     position = 'identity',
                     binwidth = bin_width) +
      geom_vline(data = med, aes(xintercept = fdep, col = fishery), size = 1.25, show.legend = FALSE) +
      geom_vline(data = med, aes(xintercept = low, col = fishery), size = 1.25, linetype = "dashed", show.legend = FALSE) +
      geom_vline(data = med, aes(xintercept = hi, col = fishery), size = 1.252, linetype = "dashed", show.legend = FALSE) +
      scale_x_continuous(limits = xlim, expand = c(0, 0)) +
      scale_y_continuous(limits = ylim, label = comma, expand = c(0, 0)) +
      xlab("Depth (m)") +
      ylab("Number of tows")
  }else{
    g <- ggplot(data = d) +
      geom_boxplot(aes(x = yr, y = fdep, fill = fishery),
                   alpha = alpha) +
      xlab("Year") +
      ylab(paste0(ifelse(depth_type = "bottom", "Bottom", "Gear"), " Depth (m)"))
  }
  g <- g +
    scale_fill_manual(values = c("#69b3a2", "#404080")) +
    scale_color_manual(values = c("#69b3a2", "#404080"))

  if(show_legend){
    g <- g +
      guides(fill = guide_legend(title = "Fishery")) +
      theme(legend.justification = c(1, 1),
            legend.key.height = unit(30, units = "points"))

    if(legend_loc == "inside"){
      g <- g +
        theme(legend.position = c(1, 1))
    }
  }else{
    g <- g +
      theme(legend.position="none")
  }

  g
}

#' Plot length distributions as histograms
#'
#' @param yrs Which years to include in the data to plot. If NA, all years in the `Years` column
#'   will be used
#' @param bin_width See [ggplot2::geom_histogram()]
#' @param xlim Limits for the x-axis. For c(NA, NA), plot will extend to limits of data
#' @param alpha Transparency of fill from 0 - 1
#' @param legend_loc Where to place legend "inside" or "outside" the frame of the plot
#'
#' @return A [ggplot2] object
#' @export
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 aes geom_histogram xlab ylab guides guide_legend scale_fill_manual theme scale_x_continuous
plot_lengths <- function(yrs = NA,
                         bin_width = 5,
                         xlim = c(NA, NA),
                         alpha = 0.6,
                         legend_loc = "inside"){
  d <- read_csv(here("data/hake_domestic_obs_len_wt_age.csv")) %>%
    mutate(fishery = ifelse(VESSEL_ID %in% freezer_trawlers$GFBIO.ID, "FT", "SS")) %>%
    filter(!is.na(Length_cm))
  if(!is.na(yrs)){
    d <- d %>%
      filter(Year %in% yrs)
  }
  g <- ggplot(data = d) +
    geom_histogram(aes(x = Length_cm, fill = fishery),
                   color = "#e9ecef",
                   alpha = alpha,
                   position = 'identity',
                   binwidth = bin_width) +
    xlab("Length") +
    ylab("Number of specimens") +
    scale_fill_manual(values = c("#69b3a2", "#404080")) +
    guides(fill = guide_legend(title = "Fishery")) +
    theme(legend.justification = c(1, 1),
          legend.key.height = unit(30, units = "points")) +
    scale_x_continuous(limits = xlim)

  if(legend_loc == "inside"){
    g <- g +
      theme(legend.position = c(1, 1))
  }

  g
}
