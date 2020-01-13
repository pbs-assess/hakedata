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
                   maxiter = 1000){
  d <- d %>%
    filter(!is.na(length),
           !is.na(weight))
  if(!nrow(d)){
    return(c(NA, NA))
  }
  w <- d$weight
  l <- d$length
  fit <- nls(w ~ lw_alpha * l ^ lw_beta,
             start = c(lw_alpha = 0.5, lw_beta = 2.0),
             control = list(tol = tol, maxiter = maxiter))
  coefficients(fit)
}

#' Calculate the length-weight relationship parameters for the data with both length and weight,
#' given a grouping variable
#'
#' @param d The data frame as extracted using [fetch_sample_data()]
#' @param grouping_col A character string matching the name of the column you want to group for
#' @param lw_cutoff How many length-weight records are required per sample to use the
#' length-weight model for that sample. If less than this, the overall yearly values will be used
#' @param lw_tol See [fit_lw()]
#' @param lw_maxiter See [fit_lw()]
#'
#' @return The original data frame with the columns `lw_alpha` and `lw_beta` added (if necessary). If those columns
#' exist, they will be overwritten with NAs and values calculated where data exist
#' @export
#' @importFrom dplyr group_map left_join sym coalesce
calc_lw_params <- function(d,
                           grouping_col = "year",
                           lw_cutoff = 10,
                           lw_tol = 0.1,
                           lw_maxiter = 1000){
  coalesce_after <- ifelse("lw_alpha" %in% names(d), TRUE, FALSE)
  x <- d %>%
    group_by(!! sym(grouping_col)) %>%
    group_map(~ fit_lw(.x, lw_tol, lw_maxiter)) %>%
    set_names(1:length(.))

  y <- do.call(rbind, x)
  y <- cbind(unique(d[[grouping_col]]), y) %>%
    as_tibble()
  names(y)[1] <- grouping_col
  out <- left_join(d, y, by = grouping_col)
  if(coalesce_after){
    out <- out %>% mutate(lw_alpha = coalesce(lw_alpha.x, lw_alpha.y),
                          lw_beta = coalesce(lw_beta.x, lw_beta.y)) %>%
      select(-c(lw_alpha.x, lw_alpha.y, lw_beta.x, lw_beta.y))
  }
  out
}

#' Calculate the age proportions
#'
#' @param min_date Earliest date to include
#' @param plus_grp Age plus group for maximum grouping
#' @param lw_cutoff How many length-weight records are required per sample to use the
#' length-weight model for that sample. If less than this, the overall yearly values will be used
#' @param lw_tol See [fit_lw()]
#' @param lw_maxiter See [fit_lw()]
#'
#' @return Age proportion dataframe
#' @export
#' @importFrom dplyr tally pull group_map bind_cols left_join
#' @importFrom purrr set_names map_dfr map
get_age_props <- function(min_date = as.Date("1972-01-01"),
                          plus_grp = 15,
                          lw_cutoff = 10,
                          lw_tol = 0.1,
                          lw_maxiter = 1000){

  d <- readRDS(here("data", sample_data_raw_file)) %>%
    filter(!is.na(age)) %>%
    mutate(age = ifelse(age > plus_grp, plus_grp, age)) %>%
    mutate(trip_start_date = as.Date(trip_start_date)) %>%
    filter(trip_start_date >= min_date)

  # The total catch by year
  # ct_yr <- d %>%
  #   group_by(year) %>%
  #   summarize(catch_weight = sum(catch_weight))

  # The following estimates the LW params for sample IDs with enough (lw_cutoff) lengths,
  # followed by year if not enough, followwd by whole time series for the remainder. Once this
  # call is done, the lw_alpha and lw_beta columns will be fully populated for every specimen (no NAs)
  all_yrs_lw <- fit_lw(d, lw_tol, lw_maxiter)
  ds <- d %>%
    calc_lw_params("sample_id", lw_cutoff, lw_tol, lw_maxiter) %>%
    calc_lw_params("year", lw_cutoff, lw_tol, lw_maxiter) %>%
    rename(lw_alpha.x = lw_alpha,
           lw_beta.x = lw_beta) %>%
    mutate(lw_alpha.y = all_yrs_lw[1],
           lw_beta.y = all_yrs_lw[2]) %>%
    mutate(lw_alpha = coalesce(lw_alpha.x, lw_alpha.y),
           lw_beta = coalesce(lw_beta.x, lw_beta.y)) %>%
    select(-c(lw_alpha.x, lw_alpha.y, lw_beta.x, lw_beta.y))

  # Now, calculate the weights from length for all missing weights, using speimen-specific LW params
  ds <- ds %>%
    mutate(weight = ifelse(is.na(weight),
                           lw_alpha * length ^ lw_beta,
                           weight)) %>%
    filter(!is.na(weight))
  browser()

  d_all <- d_all %>% filter(sample_id  %in% c(66357, 67173))
}

#' Summarize the numbers of lengths, weights, ages, sample_weights, and catch_weights in the sample data
#'
#' @param d A dataframe as extracted using [fetch_sample_data()]
#'
#' @return A list of two dataframes, the first summarized by year and the second sampled by sample_id and year
#' @export
sample_summary <- function(d = readRDS(here("data", sample_data_raw_file))){
  d_by_sample_id <- d %>%
    group_by(year, sample_id) %>%
    summarize(ages = sum(!is.na(age)),
              na_ages = sum(is.na(age)),
              lengths = sum(!is.na(length)),
              na_lengths = sum(is.na(length)),
              weights = sum(!is.na(weight)),
              na_weights = sum(is.na(weight)),
              sample_weights = sum(!is.na(sample_weight)),
              na_sample_weights = sum(is.na(sample_weight)),
              catch_weights = sum(!is.na(catch_weight)),
              na_catch_weights = sum(is.na(catch_weight))) %>%
    ungroup()

  d_by_yr <- d_by_sample_id %>%
    group_by(year) %>%
    summarize(ages = sum(ages),
              na_ages = sum(na_ages),
              lengths = sum(lengths),
              na_lengths = sum(na_lengths),
              weights = sum(weights),
              na_weights = sum(na_weights),
              sample_weights = sum(sample_weights),
              na_sample_weights = sum(na_sample_weights),
              catch_weights = sum(catch_weights),
              na_catch_weights = sum(na_catch_weights)) %>%
    ungroup()
  list(d_by_yr, d)
}
