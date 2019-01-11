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
                                                    hakedata::species_name),
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
                                                    hakedata::species_name),
                                               ".rds"))){
  readRDS(file)
}

fishery_enum <- function(){
  list(jv = "JV",
       ss = "Shoreside",
       ft = "Freezer Trawlers",
       all = "All")
}

#' catch_by_day
#'
#' @param d a list of data retrieved using gfplot package functions
#' @param major_areas a vector of major stat areas as strings. e.g. "01" is 4B Strait of Georgia
#' @param fishery the fishery to return the catch for. Default is all records. Uses the fishery_enum
#'   function as an enumerator to shorten names.
#' @param include_juandefuca Include the minor area of Juan De Fuca Strait which is located in major area 4B
#' @return the catch data frame by year, month, and day
#' @export
#' @importFrom dplyr mutate group_by summarize filter bind_rows
#' @importFrom lubridate month day year
#' @examples
#' fetch_data()
#' d <- load_data()
#' ct <- catch_by_day(d)
#' ct.ft <- catch_by_day(d, fishery = fishery_enum()$ft)
catch_by_day <- function(d,
                         major_areas = hakedata::major_hake_areas,
                         fishery = fishery_enum()$all,
                         include_juandefuca = TRUE){

  if(fishery == fishery_enum()$ss){
    ct <- d$catch %>%
      dplyr::filter(!vessel_registration_number %in% hakedata::freezer_trawlers$FOS.ID)
  }else if(fishery == fishery_enum()$ft){
    ct <- d$catch %>%
      dplyr::filter(vessel_registration_number %in% hakedata::freezer_trawlers$FOS.ID)
  }else if(fishery == fishery_enum()$jv){
    ct <- d$catch %>% dplyr::filter(trip_type_name == "OPT A - HAKE QUOTA (JV)")
  }else{ ## Assume catch from all fisheries
    ct <- d$catch
  }

  d_out <- ct %>%
    dplyr::filter(fishery_sector == "GROUNDFISH TRAWL" &
                  major_stat_area_code %in% major_areas &
                  gear == "MIDWATER TRAWL")

  if(include_juandefuca){
    d_juandefuca <- ct %>%
      dplyr::filter(fishery_sector == "GROUNDFISH TRAWL" &
                    major_stat_area_code == "01" &
                    minor_stat_area_code == "20" &
                    gear == "MIDWATER TRAWL")

    d_out <- bind_rows(d_out, d_juandefuca)
  }
  d_out %>%
    group_by(best_date) %>%
    summarize(total_catch = sum(landed_kg + discarded_kg),
              num_landings = n()) %>%
    dplyr::ungroup()
}

get_comm_samples <- function(d,
                             major_areas = hakedata::major_hake_areas,
                             include_juandefuca = TRUE){
  cs <- d$commercial_samples
  d_out <- cs %>%
    dplyr::filter(major_stat_area_code %in% major_areas)

  if(include_juandefuca){
    d_juandefuca <- cs %>%
      dplyr::filter(major_stat_area_code == "01" &
                    minor_stat_area_code == "20")

    d_out <- bind_rows(d_out, d_juandefuca) %>%
      mutate(year = year(best_date),
             month = month(best_date)) %>%
      group_by(year, month) %>%
      summarize(total_catch = sum(landed_kg + discarded_kg),
                num_landings = n()) %>%
      dplyr::ungroup()
  }
  d_out
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

.onAttach <- function(pkgname, libname) {
  ggplot2::theme_set(gfplot::theme_pbs())
  sensitivity_colors <- c("#000000", RColorBrewer::brewer.pal(8L, "Dark2"))
  assign("scale_colour_continuous", ggplot2::scale_colour_viridis_c)
  assign("scale_fill_continuous", ggplot2::scale_fill_viridis_c)
  assign("scale_colour_discrete",
         function(..., values = sensitivity_colors)
           scale_colour_manual(..., values = values),
         globalenv())
  assign("scale_fill_discrete",
         function(..., values = sensitivity_colors)
           scale_fill_manual(..., values = values),
         globalenv())
}
