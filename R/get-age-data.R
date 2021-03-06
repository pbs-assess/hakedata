#' Fetch the sample data from the GFBIOSQL database
#' The data will be filtered to only include hake major and minor areas
#'
#' @param overwrite Logical. Overwrite the RDS file for sample data if it exists
#'
#' @export
#' @importFrom readr read_csv
#' @importFrom magrittr %<>%
#' @importFrom tibble enframe
fetch_sample_data <- function(overwrite = FALSE){
  d <- read_csv(here("data", "hake_domestic_obs_len_wt_age.csv"))
  names(d) %<>% tolower
  nm <- names(d)
  nm <- gsub("length_cm", "length", nm)
  nm <- gsub("weight_g", "weight", nm)
  nm <- gsub("specimen_age", "age", nm)
  names(d) <- nm
  saveRDS(d, here("data", sample_data_raw_file))
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
#' @param grouping_cols A vector of character strings matching the names of the columns you want to group for
#' @param lw_cutoff How many length-weight records are required per sample to use the
#' length-weight model for that sample. If less than this, the overall yearly values will be used
#' @param lw_tol See [fit_lw()]
#' @param lw_maxiter See [fit_lw()]
#'
#' @return The original data frame with the columns `lw_alpha` and `lw_beta` added (if necessary). If those columns
#' exist, they will be overwritten with NAs and values calculated where data exist
#' @export
#' @importFrom dplyr group_map left_join sym coalesce
#' @importFrom purrr set_names
calc_lw_params <- function(d,
                           grouping_cols = "year",
                           lw_cutoff = 10,
                           lw_tol = 0.1,
                           lw_maxiter = 1000){
  coalesce_after <- ifelse("lw_alpha" %in% names(d), TRUE, FALSE)

  x <- d %>%
    group_by_at(vars(one_of(grouping_cols))) %>%
    group_map(~ fit_lw(.x, lw_tol, lw_maxiter)) %>%
    set_names(1:length(.))
#browser()
  y <- do.call(rbind, x)
  y <- cbind(unique(d %>% select(grouping_cols)), y) %>%
    as_tibble()
  out <- left_join(d, y, by = grouping_cols)
  if(coalesce_after){
    out <- out %>% mutate(lw_alpha = coalesce(lw_alpha.x, lw_alpha.y),
                          lw_beta = coalesce(lw_beta.x, lw_beta.y)) %>%
      select(-c(lw_alpha.x, lw_alpha.y, lw_beta.x, lw_beta.y))
  }
  out
}

#' Calculate the weighted age proportions for the input data
#'
#' @details
#' Each record will have an `lw_alpha` and `lw_beta` assigned to it. To get these:
#' Estimate them for `sample_id`s with enough (`lw_cutoff`) length samples in them.
#' Next, group the data by `year` and coalesce those into the same columns.
#' To fill in the remaining ones, use an overall (all years) estimate (local variable
#' `all_yrs_lw`). At this point, the `lw_alpha` and `lw_beta` columns will be fully
#' populated for every specimen (no NAs).
#' Hake sampling for length has been very good, so specimen weights are calculated using the
#' length/weight parameters estimated for each specimen (for records in which weights
#' haven't been recorded as data).
#'
#' Calculate missing sample weights, by summing individual specimen weights in each sample
#' They are divided by 1,000 because the specimen samples are in grams and sample weights in kilograms.
#' If there is no catch weight for a sample, both `catch_weight` and `sample_weight` are
#' set to 1 so that the ratio multiplied later for weighting is 1 and raw proportions are used
#' instead for these samples.
#'
#' Counts of ages by `sample_id` are made, and missing ages are filled in using [tidyr::complete()].
#' Missing `year`, `sample_weight`, and `catch_weight` for cases added with [tidyr::complete()] are added.
#' Numbers-at-age are weighted by `catch_weight` / `sample_weight`.
#' Numbers for each `year` and `age` are summed.
#' Weighted proportions by `year` and `age` are produced.
#'
#' @param d Dataframe as extracted by [fetch_sample_data()]
#' @param min_date Earliest date to include
#' @param plus_grp Age plus group for maximum grouping
#' @param lw_cutoff How many length-weight records are required to estimate a length/weight model
#' @param lw_tol See [fit_lw()]
#' @param lw_maxiter See [fit_lw()]
#' @param by_month Logical. If TRUE the return dataframe with have a `month` column
#'
#' @return Age proportion dataframe with ages as columns and years as rows
#' @export
#' @importFrom dplyr first group_by_at vars one_of
#' @importFrom reshape2 dcast
#' @importFrom lubridate month
get_age_props <- function(d = readRDS(here("data", sample_data_raw_file)),
                          min_date = as.Date("1972-01-01"),
                          plus_grp = 15,
                          lw_cutoff = 10,
                          lw_tol = 0.1,
                          lw_maxiter = 1000,
                          by_month = FALSE){

  temporal_grouping <- if(by_month) c("year", "month") else "year"

  d <- d %>%
    filter(!is.na(age)) %>%
    mutate(age = ifelse(age > plus_grp, plus_grp, age)) %>%
    mutate(trip_start_date = as.Date(trip_start_date)) %>%
    filter(trip_start_date >= min_date)

  if(by_month){
    d <- d %>%
      mutate(month = month(trip_start_date)) %>%
      select(year, month, sample_id, length, weight, age, sample_weight, catch_weight)
  }else{
    d <- d %>%
      select(year, sample_id, length, weight, age, sample_weight, catch_weight)
  }

  # LW paramater estimation
  all_yrs_lw <- fit_lw(d, lw_tol, lw_maxiter)
  ds <- d %>%
    calc_lw_params("sample_id", lw_cutoff, lw_tol, lw_maxiter) %>%
    calc_lw_params(temporal_grouping, lw_cutoff, lw_tol, lw_maxiter) %>%
    rename(lw_alpha.x = lw_alpha,
           lw_beta.x = lw_beta) %>%
    mutate(lw_alpha.y = all_yrs_lw[1],
           lw_beta.y = all_yrs_lw[2]) %>%
    mutate(lw_alpha = coalesce(lw_alpha.x, lw_alpha.y),
           lw_beta = coalesce(lw_beta.x, lw_beta.y)) %>%
    select(-c(lw_alpha.x, lw_alpha.y, lw_beta.x, lw_beta.y))

  # Calculate the weights from length for all missing weights,
  # using specimen-specific LW params
  ds <- ds %>%
    filter(!is.na(length)) %>%
    mutate(weight = ifelse(is.na(weight),
                           lw_alpha * length ^ lw_beta,
                           weight))

  ap <- ds %>%
    group_by(sample_id) %>%
    mutate(sample_weight = ifelse(is.na(sample_weight),
                                  sum(weight) / 1000.0,
                                  sample_weight)) %>%
    mutate(sample_weight = ifelse(all(is.na(catch_weight)),
                                  1,
                                  sample_weight),
           catch_weight = ifelse(all(is.na(catch_weight)),
                                 1,
                                 catch_weight)) %>%
    ungroup() %>%
    group_by(sample_id, age) %>%
    summarize(year = first(year),
              num_ages = n(),
              sample_weight = first(sample_weight),
              catch_weight = first(catch_weight)) %>%
    ungroup() %>%
    complete(sample_id, age) %>%
    filter(age > 0) %>%
    group_by(sample_id) %>%
    mutate(num_ages = ifelse(is.na(num_ages),
                             0,
                             num_ages),
           year = max(year, na.rm = TRUE),
           sample_weight = max(sample_weight, na.rm = TRUE),
           catch_weight = max(catch_weight, na.rm = TRUE)) %>%
    mutate(num_ages_weighted = num_ages * catch_weight / sample_weight) %>%
    ungroup() %>%
    group_by(year, age) %>%
    summarize(num_ages_weighted = sum(num_ages_weighted)) %>%
    mutate(age_prop = num_ages_weighted / sum(num_ages_weighted)) %>%
    ungroup() %>%
    select(-num_ages_weighted)

  dcast(ap, year ~ age, value.var = "age_prop")
}

#' Summarize the numbers of lengths, weights, ages, sample_weights, and catch_weights in the sample data
#'
#' @param d A dataframe as extracted using [fetch_sample_data()]
#' @param vessel_filter If NULL, no filter. If "ft", then freezer trawlers as defined in this package. If "ss",
#' then Shoreside vessels as defined in this package
#'
#' @return A list of two dataframes, the first summarized by year and the second sampled by sample_id and year
#' @export
#' @importFrom dplyr n_distinct
sample_summary <- function(d = readRDS(here("data", sample_data_raw_file)), vessel_filter = NULL){
  if(!is.null(vessel_filter)){
    if(vessel_filter == "ft"){
      d <- d %>%
        filter(vessel_id %in% freezer_trawlers$GFBIO.ID)
    }else if(vessel_filter == "ss"){
      d <- d %>%
        filter(!vessel_id %in% freezer_trawlers$GFBIO.ID)
    }else{
      stop("You must choose either 'ft' or 'ss' for vessel_filter",
           call. = FALSE)
    }
  }

  browser()
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
              na_catch_weights = sum(is.na(catch_weight)),
              num_fish = n_distinct(specimen_id)) %>%
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
              na_catch_weights = sum(na_catch_weights),
              num_fish = sum(num_fish)) %>%
    ungroup()
  list(d_by_yr, d_by_sample_id)
}

age_sample_summary <- function(d = readRDS(here("data", sample_data_raw_file)), vessel_filter = NULL){

  if(!is.null(vessel_filter)){
    if(vessel_filter == "ft"){
      d <- d %>%
        filter(vessel_id %in% freezer_trawlers$GFBIO.ID)
    }else if(vessel_filter == "ss"){
      d <- d %>%
        filter(!vessel_id %in% freezer_trawlers$GFBIO.ID)
    }else{
      stop("You must choose either 'ft' or 'ss' for vessel_filter",
           call. = FALSE)
    }
  }
  d %<>%
    filter(!is.na(age)) %>%
    group_by(year) %>%
    summarize(num_samples = n_distinct(sample_id),
              num_fish = n_distinct(specimen_id)) %>%
    ungroup()

  d
}

