#' fetch_data
#'
#' @param file the full path filename including extension .rds
#'
#' @export
#'
#' @examples
#' fetch_data()
fetch_data <- function(file = file.path(here::here("generated-data"),
                                        paste0(gsub(" ",
                                                    "-",
                                                    species_name),
                                               ".rds"))){
  d <- list()
  d$commercial_samples <- gfplot::get_commercial_samples(species_name)
  d$survey_samples <- gfplot::get_survey_samples(species_name)
  d$catch <- gfplot::get_catch(species_name)
  saveRDS(d, file)
}

#' load_data
#'
#' @param file the full path filename including extension .rds
#'
#' @return the contents of the rds file as a list
#' @export
#'
#' @examples
#' d <- load_data()
load_data <- function(file = file.path(here::here("generated-data"),
                                        paste0(gsub(" ",
                                                    "-",
                                                    species_name),
                                               ".rds"))){
  readRDS(file)
}

#' get_catch
#'
#' @param d a list of data retrieved using gfplot package functions
#' @param major_areas a vector of major stat areas as strings. e.g. "01" is 4B Strait of Georgia
#' @param vessels a data frame of vessel names (Vessel) and registration numbers (ID) to either
#'   include or exclude. If NA, all vessels will be included and vessels.include will be ignored
#' @param vessels.include if TRUE data for vessels in the vessels argument will be returned, if FALSE
#'   data for vessels other than those given will be returned
#'
#' @return the modified catch data frame
#' @export
#' @importFrom dplyr mutate group_by summarize filter bind_rows
#' @importFrom lubridate month day year
get_catch <- function(d,
                      major_areas = major_hake_areas,
                      vessels = ft,
                      vessels.include = TRUE){

  ## Filter by vessel
  ifelse(!is.na(vessels),{
    ifelse(vessels.include,{
      d$catch <- d$catch %>%
        dplyr::filter(vessel_registration_number %in% vessels$ID)
    },{
      d$catch <- d$catch %>%
        dplyr::filter(!vessel_registration_number %in% vessels$ID)
    })
  }, NULL)

  d_majors <- d$catch %>%
    dplyr::filter(fishery_sector == "GROUNDFISH TRAWL" &
                  major_stat_area_code %in% major_areas &
                  gear == "MIDWATER TRAWL")

  ## For hake, include the Strait of Juan De Fuca, even though its major area is 4B
  d_juandefuca <- d$catch %>%
    dplyr::filter(fishery_sector == "GROUNDFISH TRAWL" &
                  major_stat_area_code == "01" &
                  gear == "MIDWATER TRAWL" &
                  minor_stat_area_code == "20" &
                  gear == "MIDWATER TRAWL")

  bind_rows(d_majors, d_juandefuca) %>%
    mutate(year = year(best_date),
           month = month(best_date)) %>%
    group_by(year, month) %>%
    summarize(total_catch = sum(landed_kg + discarded_kg),
              num_landings = n())
}

get_comm_samples <- function(d){
  x=d$commercial_samples
  browser()
}

#' get_alw
#'
#' @param d a list of data retrieved using gfplot package functions
#'
#' @return the age/sex/length by year data frame
#' @export
#' @importFrom dplyr mutate transmute
#' @importFrom lubridate month day year
get_alw <- function(d){
  x <- d$survey_samples %>%
    mutate(year = year(trip_start_date)) %>%
    transmute(year, age, length, weight)
  browser()
}
