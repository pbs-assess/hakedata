#' Fetch the sample data from the GFBIOSQL database
#' The data will be filtered to only include hake major and minor areas
#'
#' @param overwrite Logical. Overwrite the RDS file for sample data if it exists
#'
#' @export
#' @importFrom gfdata get_commercial_samples
fetch_sample_data <- function(overwrite = FALSE){
  if(overwrite || !file.exists(here("data", sample_data_raw_file))){
    d <- get_commercial_samples(225) %>%
      filter(major_stat_area_code == "03" |
             major_stat_area_code == "04" |
             major_stat_area_code == "05" |
             major_stat_area_code == "06" |
             major_stat_area_code == "07" |
             major_stat_area_code == "08" |
             major_stat_area_code == "09" |
             (major_stat_area_code == "01" & minor_stat_area_code == "20"))
    saveRDS(d, here("data", sample_data_raw_file))
  }
}

#' Fit a length-weight model
#'
#' @param d Dataframe containing the columns `length` and `weight`
#' @param tol See [stats::nls()]
#' @param maxiter See [stats::nls()]
#'
#' @return The [stats::coefficients()] of the model fit
#' @importFrom stats nls coefficients
fit_lw <- function(d,
                   tol = 0.1,
                   maxiter = 500){
  d <- d %>%
    filter(!is.na(length),
           !is.na(weight))
  w <- d$weight
  l <- d$length
  fit <- nls(w ~ a * l ^ b,
             start = c(a = 0.5, b = 2.0),
             control = list(tol = tol, maxiter = maxiter))
  coefficients(fit)
}

#' Calculate the age proportions
#'
#' @param min_date Earliest date to include
#' @param plus_grp Age plus group for maximum grouping
#'
#' @return
#' @export
get_age_props <- function(min_date = as.Date("1985-01-01"),
                          plus_grp = 15,
                          lw_model_by_year = FALSE){
  d <- readRDS(here("data", sample_data_raw_file)) %>%
    filter(!is.na(age)) %>%
    mutate(age = ifelse(age > plus_grp, plus_grp, age)) %>%
    mutate(trip_start_date = as.Date(trip_start_date)) %>%
    filter(trip_start_date >= min_date)

  all_lw <- fit_lw(d)

  j <- d %>%
    group_by(year) %>%
    summarize(catch_weight = sum(catch_weight))



  browser()
}

