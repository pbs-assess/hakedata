#' Fetch the sample data from the GFBIOSQL database
#'
#' @param overwrite Logical. Overwrite the RDS file for sample data if it exists
#'
#' @export
#' @importFrom gfdata get_commercial_samples
fetch_sample_data <- function(overwrite = FALSE){
  if(overwrite || !file.exists(here("data", sample_data_raw_file))){
    d <- get_commercial_samples(225)
    saveRDS(d, here("data", sample_data_raw_file))
  }
}

#' Calculate the age proportions
#'
#' @param min_date Earliest date to include
#' @param plus_grp Age plus group for maximum grouping
#'
#' @return
#' @export
get_age_props <- function(min_date = as.Date("1985-01-01"),
                          plus_grp = 15){
  d <- readRDS(here("data", sample_data_raw_file)) %>%
    as_tibble() #%>%

browser()

}

