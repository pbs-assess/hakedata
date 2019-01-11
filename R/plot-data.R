#' Colors for the Hake Assessment
#'
#' Generate the color scheme for the hake assessment
#' such that as more years of historical data are added on
#' that the more recent years are still the same color.
#' For example, the most recent year of data will always be
#' black and the second most recent year will always be red.
#'
#' @param n The default number of colours to generate is 10.
#'
#' @importFrom RColorBrewer brewer.pal
#' @export
#' @author Kelli Faye Johnson
#'
#' @return A vector of colours
#'
#' @examples
#' n <-18
#' plot(data.frame(1:n, 1), col = plot_color(n), pch = 19, cex = 5)
plot_color <- function(n.cols = 10) {
  base <- RColorBrewer::brewer.pal(name = "Set1", n = 9)
  colors <- c(base[(n.cols - 1):1], "#000000")
  if (n.cols > 10 & n.cols < 18) {
    extra <- RColorBrewer::brewer.pal(name = "Set2", n = 7)
    colors <- c(extra[(n.cols - 10):1], rev(base), "#000000")
  }
  if (n.cols >= 18) stop(n.cols, " is too many colors, only 17 are allowed.")
  return(colors)
}

#' Plot_cumu_catch
#'
#' Plot the cumulative catch for the given years
#'
#' @param d Data frame as output from catch_by_day()
#' @param inc_years Years to include in the plot
#' @param weight_factor Divide catch values by this to scale y-axis
#' @param ylim Vector of 2, Y-axis limits
#' @param horiz.line.spacing
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' d <- load_data()
#' ct <- catch_by_day(d)
#' ct.ft <- catch_by_day(d, fishery = fishery_enum()$ft)
#' plot_cumu_catch(ct.ft, horiz.line.spacing = 5)
plot_cumu_catch <- function(d,
                            inc_years = 2009:lubridate::year(lubridate::now()),
                            weight_factor = 1e6,
                            ylim = c(0, 45),
                            horiz.line.spacing = NA){
  d <- d %>%
    mutate(Date = as.Date(best_date)) %>%
    tidyr::complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>%
    mutate(year = lubridate::year(Date)) %>%
    dplyr::filter(year %in% inc_years) %>%
    dplyr::select(-best_date)

  d$total_catch[is.na(d$total_catch)] <- 0
  d$num_landings[is.na(d$num_landings)] <- 0

  ## Aggregate by day to get total catch each day
  d <- d %>%
    group_by(Date) %>%
    summarize(year = year[1],
              total_catch = sum(total_catch) / weight_factor) %>%
    dplyr::ungroup()

  ## Cumulative sums by year
  d <- d %>%
    group_by(year) %>%
    mutate(day = seq(1, n()),
           cumu_catch = cumsum(total_catch)) %>%
    dplyr::ungroup()

  d <- d %>%
    transmute(year = as.factor(year),
              day,
              cumu_catch)

  days.in.months <- c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  x.ticks <- cumsum(days.in.months)
  mon <- c(month.abb, month.abb[1])

  colors <- plot_color(length(unique(d$year)))

  g <- ggplot2::ggplot(d) +
    ggplot2::aes(x = day, y = cumu_catch, color = year) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::theme(legend.position = c(0, 1),
                   legend.justification = c(-0.2, 1.15),
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 12),
                   legend.background = ggplot2::element_rect(fill = "white")) +
    ggplot2::scale_y_continuous(labels = scales::comma,
                                limits = ylim) +
    ggplot2::scale_x_continuous(breaks = x.ticks,
                                labels = mon) +
    ggplot2::ylab("Cumulative landings (thousand mt)") +
    ggplot2::xlab("Month")
  if(!is.na(horiz.line.spacing)){
    g <- g + ggplot2::geom_hline(yintercept = seq(0, ylim[2], horiz.line.spacing),
                                 linetype = "dashed")
  }
  g
}