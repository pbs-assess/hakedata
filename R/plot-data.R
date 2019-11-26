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
plot_color <- function(n.cols = 10) {
  base <- brewer.pal(name = "Set1", n = 9)
  colors <- c(base[(n.cols - 1):1], "#000000")
  if (n.cols > 10 & n.cols < 18) {
    extra <- brewer.pal(name = "Set2", n = 7)
    colors <- c(extra[(n.cols - 10):1], rev(base), "#000000")
  }
  if (n.cols >= 18) stop(n.cols, " is too many colors, only 17 are allowed.")
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

#' Plot distribution of catch by area
#'
#' @param d Data frame as output from catch_by_day()
#' @param inc_areas Areas to include in the plot
#' @param inc_years Years to include in the plot
#' @param ylim Vector of 2, Y-axis limits
#'
#' @return A ggplot object
#' @export
#' @importFrom ggplot2 aes geom_point geom_line scale_color_manual theme element_blank
#'  element_text element_rect scale_y_continuous scale_x_continuous ylab xlab
#'  geom_hline
#' @importFrom dplyr n filter select mutate group_by summarize ungroup transmute
#'
plot_area_dist <- function(d,
                           inc_areas = major_hake_areas,
                           inc_years = (year(now()) - 5):year(now()),
                           ylim = c(0, 1300)){

  d_out <- d %>%
    mutate(year = year(best_date),
           area = major_stat_area_code) %>%
    dplyr::filter(area %in% inc_areas,
                  year %in% inc_years) %>%
    dplyr::select(-best_date) %>%
    group_by(year, area) %>%
    summarize(landings = sum(num_landings))

browser()



}
