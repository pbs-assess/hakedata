#' Get the survey sample data for the Hake acoustic survey
#'
#' @return data
#' @importFrom gfdata get_hake_survey_samples
#' @export
fetch_survey_data <- function(overwrite = FALSE){
  if(overwrite || !file.exists(here("data", survey_data_file))){

    d <- get_hake_survey_samples() %>%
      filter(survey_series_desc == "Joint Canada/US Hake Acoustic")
    saveRDS(d, here("data-cache", survey_data_file))
  }

}

#' Loads survey data from RDS files found in the data directory
#'
#' @return The tibble with the data
#' @export
load_survey_data <- function(){
  readRDS(here("data-cache", survey_data_file))
}

