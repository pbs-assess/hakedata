#' Loads data from csv files found in the data directory. See [hakedata::landings_file]
#' and [hakedata::logs_pattern] for filenames.
#'
#' @return a list of two dataframes, one for the DMP data and one for the LOGS data
#' @export
#' @importFrom readr read_csv
#' @importFrom purrr map reduce
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

#' Get the catch data from the DMP and LOGS data frames
#'
#' @param dmp The Dockside Monitoring Program data frame as extracted by [hakedata::load_data()]
#' @param logs The Logbook data frame as extracted by [hakedata::load_data()]
#' @param type one of "ft", "ss", or "jv" for Freezer Trawler, Shoreside, and Joint Venture respectively
#'
#' @return a data frame containing only freezer trawler data
#' @export
#' @importFrom dplyr filter group_by summarize ungroup full_join
get_catch <- function(dmp, logs, type){
  if(!type %in% c("ft", "ss", "jv")){
    stop("type must be one of 'ft', 'ss', or 'jv'", call. = FALSE)
  }

  if(type == "ft" || type == "ss"){
    # If the non-JV vessel was fishing in JV, remove those catches
    dmp <- dmp %>%
      filter(!grepl("JV", LICENCE.TRIP.TYPE))
    logs <- logs %>%
      filter(!grepl("JV", TRIP.TYPE))
  }
  # At Sea Observer Program records only (all of those are discards in the LOGS data), for all fleet types
  discards <- logs %>%
    filter(SOURCE == "ASOP",
           !is.na(RELEASED.WT))

  if(type == "ft"){
    dmp <- dmp %>%
      filter(VRN %in% freezer_trawlers$FOS.ID)
    discards <- discards %>%
      filter(VRN %in% freezer_trawlers$FOS.ID)
  }else if(type == "ss"){
    dmp <- dmp %>%
      filter(!VRN %in% freezer_trawlers$FOS.ID)
    discards <- discards %>%
      filter(!VRN %in% freezer_trawlers$FOS.ID)
  }else if(type == "jv"){
    dmp <- dmp %>%
      filter(grepl("JV", LICENCE.TRIP.TYPE))
    logs <- logs %>%
      filter(grepl("JV", TRIP.TYPE))
  }

  dmp <- dmp %>%
    select(year, month, CONVERTED.WGHT.LBS) %>%
    group_by(year, month) %>%
    summarize(landings = sum(CONVERTED.WGHT.LBS) / lbs_to_kilos,
              count =  n()) %>%
    ungroup()
  discards <- discards %>%
    select(year, month, RELEASED.WT) %>%
    group_by(year, month) %>%
    summarize(landings = sum(RELEASED.WT) / lbs_to_kilos,
              count =  n()) %>%
    ungroup()

  full_join(dmp, discards, by = c("year", "month")) %>%
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
remove_trip_data <- function(d, types, trip_type_colname){
  for(ty in types){
    gr <- grep(ty, d[[trip_type_colname]])
    if(length(gr)){
      d <- d[-gr,]
    }
  }
  d
}

#' Strip the first n lines off a file, and re-save
#'
#' @param file filename
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

#' Read in sql code from a package file
#'
#' @param fn filename for sql code
#'
#' @return a vector of charcter strings, one for each line in `fn``
read_sql <- function(fn) {
  if(file.exists(system.file("sql", fn, package = "hakedata"))){
    readLines(system.file("sql", fn, package = "hakedata"))
  }else{
    stop("The sql file does not exist.", call. = FALSE)
  }
}

#' Inject SQL code for fihery categories and types based on the fleet type for hake
#'
#' @param sql SQL code as a vector of character strings (from [base::readLines()])
#' @param type one of "ft", "ss", or "jv" for Freezer Trawler, Shoreside, and Joint Venture respectively
inject_fishery_filter <- function(sql, type){
  if(!type %in% c("ft", "ss", "jv")){
    stop("type must be one of 'ft', 'ss', or 'jv'", call. = FALSE)
  }

  if(type == "ft"){
    search_flag = "-- inject fishery categories here"
    i <- suppressWarnings(grep(search_flag, sql))
    sql[i] <- paste0("(c.TRIP_CATEGORY = 'OPT A - HAKE QUOTA (SHORESIDE)' or ",
                     "c.TRIP_CATEGORY = 'OPT A - QUOTA') and ")
    search_flag = "-- inject vessel codes here"
    i <- suppressWarnings(grep(search_flag, sql))
    sql[i] <- "("
    for(j in 1:nrow(freezer_trawlers)){
      sql[i] <- paste0(sql[i], "c.VESSEL_REGISTRATION_NUMBER = ", freezer_trawlers[j,]$FOS.ID, " or ")
    }
    sql[i] <- gsub(" or $", ") and ", sql[i])
  }else if(type == "ss"){
    search_flag = "-- inject fishery categories here"
    i <- suppressWarnings(grep(search_flag, sql))
    sql[i] <- paste0("(c.TRIP_CATEGORY = 'OPT A - HAKE QUOTA (SHORESIDE)' or ",
                     "c.TRIP_CATEGORY = 'OPT A - QUOTA') and ")
    search_flag = "-- inject vessel codes here"
    i <- suppressWarnings(grep(search_flag, sql))
    sql[i] <- "("
    for(j in 1:nrow(freezer_trawlers)){
      sql[i] <- paste0(sql[i], "c.VESSEL_REGISTRATION_NUMBER <> ", freezer_trawlers[j,]$FOS.ID, " and ")
    }
    sql[i] <- gsub(" and $", ") and ", sql[i])
  }else if(type == "jv"){
    search_flag = "-- inject fishery categories here"
    i <- suppressWarnings(grep(search_flag, sql))
    sql[i] <- "c.TRIP_CATEGORY = 'OPT A - HAKE QUOTA (JV)' and"
  }
sql
}

#' Run SQL code on the server database and return the records
#'
#' @param type one of "ft", "ss", or "jv" for Freezer Trawler, Shoreside, and Joint Venture respectively
#'
#' @return a data frame of the records for the SQL code given
#' @export
#' @importFrom gfdata run_sql
spatial_catch_sql <- function(type){
  if(!type %in% c("ft", "ss", "jv")){
    stop("type must be one of 'ft', 'ss', or 'jv'", call. = FALSE)
  }
  if(type == "ft"){
    sql <- inject_fishery_filter(read_sql(spatial_catch_sql_file), "ft")
  }else if(type == "ss"){
    sql <- inject_fishery_filter(read_sql(spatial_catch_sql_file), "ss")
  }else if(type == "jv"){
    sql <- inject_fishery_filter(read_sql(spatial_catch_sql_file), "jv")
  }
  run_sql("GFFOS", sql)
}

#' Save Hake data into an RDS file
#'
#' @export
#' @importFrom gfdata cache_pbs_data
cache_hake_data <- function(){
  cache_pbs_data(225,
                 file_name = "hake-data.rds",
                 path = here("data-cache"))
}

#' Read in Hake data from RDS file
#'
#' @return the list of data
#' @export
get_hake_data <- function(){
  file <- here("data-cache", "hake-data.rds")
  if(file.exists(file)){
    readRDS(file)
  }else{
    warning(file, " does not exist. Creating the file now...")
    cache_hake_data()
    readRDS(file)
  }
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
