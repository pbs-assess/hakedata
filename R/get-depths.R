#' Get depth data by year and calculate boxplot stats on it.
#' This writes a CSV file containing the output
#'
#' @param d Output from [load_spatial_catch_data()]
#' @param type One of "bottom" or "gear" for depth type
#' @param yrs A vector of years to include. If NULL, all years in the data will be included
#'
#' @return Invisibly - A tibble containing year and depth record stats
#' @export
#'
#' @examples
#' d_ss <- load_spatial_catch_data("ss")
#' gear_depth_ss <- get_depth_by_year(d_ss, "gear")
get_depth_by_year <- function(d,
                              type = "bottom",
                              yrs = NULL){
  if(type == "bottom"){
    dpth <- d %>% filter(!is.na(bottomdepth_fm)) %>%
      transmute(year = year(catchdate),
                depth = bottomdepth_fm * 1.8288)
  }else if(type == "gear"){
    dpth <- d %>% filter(!is.na(geardepth_fm)) %>%
      transmute(year = year(catchdate),
                depth = geardepth_fm * 1.8288)
  }else{
    stop("type must be 'bottom' or 'gear'", call. = FALSE)
  }
  dpth <- dpth %>%
    group_by(year) %>%
    do(as.data.frame(t(boxplot.stats(.$depth)$`stats`))) %>%
    ungroup() %>%
    transmute(year,
              lower95 = V1,
              lowerhinge = V2,
              median = V3,
              upperhinge = V4,
              upper95 = V5)
  if(!is.null(yrs)){
    dpth <- dpth %>%
      filter(year %in% yrs)
  }
  fleet <- paste0(unique(d$fishery), collapse = "")
  filename <- here::here("data", paste0("depth-can-", fleet, "-", type,".csv"))
  write_csv(dpth, filename)
  message("Wrote file ", filename)
  invisible(dpth)
}
