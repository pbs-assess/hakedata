#' Loads data from csv files found in the data directory. See [hakedata::landings_file]
#' and [hakedata::logs_pattern] for filenames.
#'
#' @return
#' @export
#' @importFrom readr read_csv
#' @importFrom purrr map reduce
#'
#' @examples
load_data <- function(){
  data_path <- here("data")

  # DMP LANDINGS
  dmp <- readLines(file.path(data_path, dmp_file))
  if(length(grep("^PACIFIC HAKE", dmp[1]))){
    dmp <- dmp[-1]
    writeLines(dmp, file.path(data_path, dmp_file))
  }
  dmp <- read_csv(file.path(data_path, dmp_file))
  # Replace all spaces in column names with periods
  names(dmp) <- gsub(" +", ".", names(dmp))
  names(dmp) <- gsub("\\(", ".", names(dmp))
  names(dmp) <- gsub("\\)", "", names(dmp))
  dmp$LANDING.DATE <- gsub(" 00:00", "", dmp$LANDING.DATE)
  dmp$LANDING.DATE <- as.Date(dmp$LANDING.DATE, "%B %d %Y")
  dmp <- dmp %>%
    mutate(year = year(LANDING.DATE),
           month = month(LANDING.DATE),
           day = day(LANDING.DATE))
  dmp <- remove_trip_data(dmp, c("GULF", "OPT B"), "LICENCE.TRIP.TYPE")

  # LOGS LANDINGS
  logs_files <- dir(data_path, pattern = logs_pattern)
  map(logs_files, ~strip_lines(file.path(data_path, .)))
  logs <- map(logs_files, ~read_csv(file.path(data_path, .)))
  logs <- reduce(logs, rbind)
  # Replace all spaces in column names with periods
  names(logs) <- gsub(" +", ".", names(logs))
  names(logs) <- gsub("/", ".", names(logs))
  logs$LANDING.DATE <- as.Date(logs$LANDING.DATE, "%B %d %Y")
  logs <- logs %>%
    mutate(year = year(LANDING.DATE),
           month = month(LANDING.DATE),
           day = day(LANDING.DATE))
  logs <- remove_trip_data(logs, c("GULF", "OPT B"), "TRIP.TYPE")
  if(any(logs$AREA == "4B")){
    area4b <<- filter(logs, AREA == "4B")
    logs <- filter(logs,
                   LANDING.PORT != "FRENCH CREEK",
                  !(AREA == "4B" && month < 6))
    warning("Some of the LOGS landings are in the 4B area. They ",
            "are stored in the variable `area4b` for manual checking. ",
            "All landings from French Creek were removed, and those landed ",
            "before June in 4B, but you may want to remove some other records.")
  }
  list(dmp, logs)
}

#' Get the shoreside catch data from the DMP and LOGS data frames
#'
#' @param dmp The Dockside Monitoring Program data frame as extracted by [hakedata::load_data()]
#' @param logs The Logbook data frame as extracted by [hakedata::load_data()]
#'
#' @return a data frame containing only shoreside data
#' @export
#' @importFrom dplyr filter group_by summarize ungroup full_join
#'
#' @examples
get_ss_catch <- function(dmp, logs){

}

#' Get the freezer trawler data from the DMP and LOGS data frames
#'
#' @param dmp The Dockside Monitoring Program data frame as extracted by [hakedata::load_data()]
#' @param logs The Logbook data frame as extracted by [hakedata::load_data()]
#'
#' @return a data frame containing only freezer trawler data
#' @export
#' @importFrom dplyr filter group_by summarize ungroup full_join
#'
#' @examples
get_ft_catch <- function(dmp, logs){
  ft_dmp <- dmp %>%
    filter(VRN %in% freezer_trawlers$FOS.ID)
  ft_logs <- logs %>%
    filter(VRN %in% freezer_trawlers$FOS.ID)

  # In case the freezer trawlers were ever fishing the JV fishery, remove those
  tmp <- grep("JV", ft_dmp$LICENCE.TRIP.TYPE)
  if(length(tmp)){
    ft_dmp <- ft_dmp[-grep("JV", ft$LICENCE.TRIP.TYPE),]
  }
  tmp <- grep("JV", ft_logs$TRIP.TYPE)
  if(length(tmp)){
    ft_logs <- ft_logs[-grep("JV", ft_logs$TRIP.TYPE),]
  }

  # Fetch At-sea-observer discard records for freezer trawlers
  ft_logs <- ft_logs %>%
    filter(SOURCE == "ASOP",
           !is.na(RELEASED.WT))

  ft_dmp <- ft_dmp %>%
    select(year, month, CONVERTED.WGHT.LBS) %>%
    group_by(year, month) %>%
    summarize(landings = sum(CONVERTED.WGHT.LBS) / lbs_to_kilos,
              count =  n()) %>%
    ungroup()
  ft_discards <- ft_logs %>%
    select(year, month, RELEASED.WT) %>%
    group_by(year, month) %>%
    summarize(landings = sum(RELEASED.WT) / lbs_to_kilos,
              count =  n()) %>%
    ungroup()

  full_join(ft_dmp, ft_discards, by = c("year", "month")) %>%
    group_by(year, month) %>%
    mutate(landings.x = ifelse(is.na(landings.x), 0, landings.x),
           landings.y = ifelse(is.na(landings.y), 0, landings.y)) %>%
    summarize(landings = landings.x + landings.y,
              landings_count = count.x,
              discards_count = count.y) %>%
    ungroup()
}

#' Remove some trip types from a data frame
#'
#' @param d the data frame with a column named the same as `trip_type_colname`
#' @param types a vector of strings to grep for in the `trip_type_colname` column for removal
#' @param trip_type_colname the column name containing the trip type strings
#'
#' @return a data frame with the data removed for the trip types
#'
#' @examples
remove_trip_data <- function(d, types, trip_type_colname){
  for(ty in types){
    gr <- grep(ty, d[[trip_type_colname]])
    if(length(gr)){
      d <- d[-gr,]
    }
  }
  d
}

#' Sum and count the landings by aggregating by years and months. Assumes the
#' catch weight is in pounds and converts the sum to kilograms.
#'
#' @param df data frame as
#' @param fishery
#'
#' @return a data frame with the following five columns:
#'   Fishery, Year, Month, weightKg, numLandings
#'   Where Fishery will be the same for all records (fishery string)
#' @export
#'
#' @examples
calc_landings <- function(df, fishery){
  ## Remove the unneeded fields to prep for aggregation
  d <- as.data.frame(cbind(df$year, df$month, df$CONVERTED.WGHT.LBS.))
  if(nrow(d) == 0){
    return(NULL)
  }
  ## Aggregate the data frame by year and month, summing landings
  dsum <- aggregate(d[,3], by=list(d[,1], d[,2]), FUN=sum)
  ## Sort the data table by year and month, ascending
  dsum <- dsum[order(dsum[,1],dsum[,2]),]
  ## Convert all weights
  dsum[,3] <- dsum[,3] / POUNDS.TO.KILOS

  ## Aggregate the data frame by year and month, counting landings
  dcount <- aggregate(d[,3], by=list(d[,1], d[,2]), FUN=length)
  ## Sort the data table by year and month, ascending
  dcount <- dcount[order(dcount[,1],dcount[,2]),]

  ## Merge the two tables
  dboth <- cbind(dsum, dcount[,3])
  ## Add the field describing the fishery
  dboth <- cbind(rep(fishery, nrow(dboth)), dboth)
  colnames(dboth) <- c("Fishery", "Year", "Month", "weightKg", "numLandings")
  return(dboth)
}

#' Strip the first n lines off a file, and re-save
#'
#' @param file filename
#'
#' @export
#'
#' @examples
strip_lines <- function(file){
  d <- readLines(file)
  if(length(grep("^LOGBOOK", d[1]))){
    # Strip the lines
    ind_empty_lines <- which(!nzchar(d))
    if(!length(ind_empty_lines)){
      stop("Could not find an empty line in the file ",
           file,
           ". Check the file, there should be an empty line before the header line.",
           call. = FALSE)
    }
    ind_last_empty_line <- max(ind_empty_lines)
    d <- d[-c(1:ind_last_empty_line)]
    writeLines(d, file)
  }
}

#' fetch_data
#'
#' @param file the full path filename including extension .rds
#'
#' @export
#' @importFrom gfdata get_commercial_samples get_survey_samples get_hake_catch
#' @importFrom here here
#'
#' @examples
#' fetch_data()
fetch_data <- function(file = here("generated-data",
                                   paste0(gsub(" ",
                                               "-",
                                               species_name),
                                          ".rds")),
                       overwrite = FALSE){
  if(file.exists(file)){
    if(overwrite){
      unlink(file)
    }else{
      message("File already exists and overwrite is FALSE so nothing was done.")
      return()
    }
  }
  d <- list()
  d$commercial_samples <- get_commercial_samples(species_name)
  d$survey_samples <- get_survey_samples(species_name)
  d$catch <- get_hake_catch()
  saveRDS(d, file)
}

#' load_data_gfdata
#'
#' @param file the full path filename including extension .rds
#'
#' @return the contents of the rds file as a list
#' @export
#' @importFrom here here
#'
#' @examples
#' d <- load_data_gfdata()
load_data_gfdata <- function(file = here("generated-data",
                                  paste0(gsub(" ",
                                              "-",
                                              species_name),
                                         ".rds"))){
  if(!file.exists(file)){
    stop("Error, file ", file, " does not exist. To create it, run fetch_data().",
         call. = FALSE)
  }
  readRDS(file)
}

#' fishery_enum
#'
#' An enumeration function used to generalize code used in other functions
#'
#' @return A character astring representing the fishery
#' @export
#'
#' @examples
#' fishery_enum()$ft
fishery_enum <- function(){
  list(jv = "JV",
       ss = "Shoreside",
       ft = "Freezer Trawlers",
       all = "All")
}

#' catch_by_day
#'
#' Calculate the catch by day for a given fishery
#'
#' @param d a list of data retrieved using gfdata package functions
#' @param major_areas a vector of major stat areas as strings. e.g. "01" is 4B Strait of Georgia
#' @param fishery the fishery to return the catch for. Default is all records. Uses the fishery_enum
#'   function as an enumerator to shorten names.
#' @param include_juandefuca Include the minor area of Juan De Fuca Strait which is located in major area 4B
#' @param byarea If TRUE, data frame will be have one row per unique date & area. If FALSE,
#'   there will be one unique row for each day
#' @return the catch data frame
#' @export
#' @importFrom dplyr mutate group_by summarize filter bind_rows ungroup row_number
#' @importFrom lubridate month day year
#' @examples
#' fetch_data()
#' d <- load_data()
#' ct <- catch_by_day(d)
#' ct.ft <- catch_by_day(d, fishery = fishery_enum()$ft)
catch_by_day <- function(d,
                         major_areas = major_hake_areas,
                         fishery = fishery_enum()$all,
                         include_juandefuca = TRUE,
                         byarea = FALSE){

  if(fishery == fishery_enum()$ss){
    ct <- d$catch %>%
      filter(!vessel_registration_number %in% freezer_trawlers$FOS.ID)
  }else if(fishery == fishery_enum()$ft){
    ct <- d$catch %>%
      filter(vessel_registration_number %in% freezer_trawlers$FOS.ID)
  }else if(fishery == fishery_enum()$jv){
    ct <- d$catch %>%
      filter(trip_type_name == "OPT A - HAKE QUOTA (JV)")
  }else{ ## Assume catch from all fisheries
    ct <- d$catch
  }

  d_out <- ct %>%
    filter(fishery_sector == "GROUNDFISH TRAWL" &
             major_stat_area_code %in% major_areas &
             gear == "MIDWATER TRAWL")

  if(include_juandefuca){
    d_juandefuca <- ct %>%
      filter(fishery_sector == "GROUNDFISH TRAWL" &
               major_stat_area_code == "01" &
               minor_stat_area_code == "20" &
               gear == "MIDWATER TRAWL")

    d_out <- bind_rows(d_out, d_juandefuca)
  }

  if(byarea){
    d_out <- d_out %>%
      group_by(best_date, major_stat_area_code)
  }else{
    d_out <- d_out %>%
      group_by(best_date)
  }
  browser()
  d_out %>%
    summarize(total_catch = sum(landed_kg + discarded_kg),
              num_landings = row_number()) %>%
    ungroup()
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
      ungroup()
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
