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

}

