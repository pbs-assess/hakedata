#' Get the weight-at-age data frame for the acoustic survey
#'
#' @param d
#'
#' @return a data frame
#' @export
get_surv_wa <- function(d){

  keep_yrs <- c(1995, 1998, 2001, 2003, 2009, 2011, 2012)
  all_yrs <- unique(d$year)
  remain_yrs <- all_yrs[all_yrs > 2012]
  keep_yrs <- sort(c(keep_yrs, remain_yrs[as.logical(remain_yrs %% 2)]))

  d %>%
    transmute(Source = "CAN_acoustic",
              Weight_kg = weight / 1000,
              Sex = ifelse(is.na(sex), NA_character_, ifelse(sex == 1, "M", "F")),
              Age_yrs = age,
              Length_cm = length,
              Month = month,
              Year = year) %>%
    filter(!is.na(Weight_kg),
           !is.na(Sex),
           !is.na(Age_yrs),
           !is.na(Length_cm),
           !is.na(Weight_kg),
           !is.na(Month),
           !is.na(Year)) %>%
  filter(Year %in% keep_yrs)
}

#' Get the weight-at-age data frame for commercial data.
#'
#' @param d
#'
#' @return a data frame with Source set to `CAN_shoreside` and `CAN_freezer`
#' for the two fishery types
#' @importFrom lubridate year month
#' @export
get_comm_wa <- function(d){
  major_areas <- c("02", "03", "04", "05", "06", "07", "08", "09")
  # Include BC offshore and St. of Juan de Fuca (Major 1, Minor 20)

  map(c("CAN_shoreside", "CAN_freezer", "CAN_jv", "CAN_polish"), ~{
    k <- d %>%
      filter(major_stat_area_code %in% major_areas |
               (major_stat_area_code == "01" & minor_stat_area_code == "20")) %>%
      filter(gear_desc == "MIDWATER TRAWL")
    if(.x == "CAN_shoreside"){
      k <- k %>%
        filter(trip_sub_type_desc %in% c("OBSERVED DOMESTIC", "NON - OBSERVED DOMESTIC")) %>%
        filter(!vessel_id %in% freezer_trawlers$GFBIO.ID)
    }else if(.x == "CAN_freezer"){
      k <- k %>%
        filter(trip_sub_type_desc %in% c("OBSERVED DOMESTIC", "NON - OBSERVED DOMESTIC")) %>%
        filter(vessel_id %in% freezer_trawlers$GFBIO.ID)
    }else if(.x == "CAN_jv"){
      k <- k %>%
        filter(trip_sub_type_desc == "OBSERVED J-V")
    }else{
      k <- k %>%
        filter(trip_sub_type_desc %in% c("POLISH COMM NATIONAL", "POLISH COMMERCIAL SUPPLEMENTAL"))
    }
    k %>% transmute(Source = .x,
                    Weight_kg = weight / 1000,
                    Sex = ifelse(is.na(sex), NA_character_, ifelse(sex == 1, "M", "F")),
                    Age_yrs = age,
                    Length_cm = length,
                    Month = month(trip_start_date),
                    Year = year(trip_start_date)) %>%
      filter(!is.na(Weight_kg),
             !is.na(Sex),
             !is.na(Age_yrs),
             !is.na(Length_cm),
             !is.na(Weight_kg),
             !is.na(Month),
             !is.na(Year))
  }) %>% bind_rows
}
