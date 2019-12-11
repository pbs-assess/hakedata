#' Loads data from csv files found in the data directory
#'
#' @description See [dmp_file] and [logs_pattern] for descriptions of filenames
#' @return A list of three dataframes, `[[1]]` for the DMP data, `[[2]]` for the LOGS data, and `[[3]]` for the area 4B
#'   data which were not included. `[[3]]` is returned as a convinience for you to see if any other data from area 4B
#'   should be included
#' @export
#' @importFrom readr read_csv
#' @importFrom purrr map reduce
#' @importFrom here here
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
  dmp <- remove_trip_data(dmp,
                          types = c("GULF", "OPT B"),
                          trip_type_colname = "LICENCE.TRIP.TYPE")

  # LOGS LANDINGS
  logs_files <- dir(data_path, pattern = logs_pattern)
  map(logs_files, ~strip_lines(file.path(data_path, .)))
  logs <- map(logs_files, ~read_csv(file.path(data_path, .)))
  logs <- reduce(logs, rbind)
  # Replace all spaces in column names with periods
  names(logs) <- gsub(" +", ".", names(logs))
  names(logs) <- gsub("/", ".", names(logs))
  logs$LANDING.DATE <- as.Date(logs$LANDING.DATE, "%B %d %Y")
  logs <- remove_trip_data(logs,
                           types = c("GULF", "OPT B"),
                           trip_type_colname = "TRIP.TYPE")
  area4b <- NULL
  if(any(logs$AREA == "4B")){
    area4b <- filter(logs, AREA == "4B")
    logs <- filter(logs,
                   LANDING.PORT != "FRENCH CREEK",
                  !(AREA == "4B" && month < 6))
    message("Some of the LOGS landings are in the 4B area. They ",
            "are stored in the global variable `area4b` for manual checking. ",
            "All landings from French Creek were removed, and those landed ",
            "before June in 4B, but you may want to remove some other records.")
  }
  list(dmp, logs, area4b)
}

#' Get the catch data from the DMP and LOGS data frames
#'
#' @param d a list of length 2 with elements being (1) The Dockside Monitoring Program
#'  data frame as extracted by [load_data()] and (2) the Logbook data frame
#'  as extracted by [load_data()]
#' @param type one of "ft", "ss", or "jv" for Freezer Trawler, Shoreside, or Joint Venture respectively
#' @return A data frame containing only data for the type given by `type`
#' @export
#' @importFrom dplyr filter group_by summarize ungroup full_join arrange
#' @importFrom lubridate year month day
get_catch <- function(d, type){
  if(!type %in% c("ft", "ss", "jv")){
    stop("type must be one of 'ft', 'ss', or 'jv'", call. = FALSE)
  }
  dmp <- d[[1]]
  logs <- d[[2]]
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
    discards <- discards %>%
      filter(grepl("JV", TRIP.TYPE))
  }
  dmp <- dmp %>%
    complete(LANDING.DATE = seq.Date(min(LANDING.DATE), max(LANDING.DATE), by = "day")) %>%
    mutate(year = year(LANDING.DATE),
           month = month(LANDING.DATE),
           day = day(LANDING.DATE)) %>%
    select(year, month, day, CONVERTED.WGHT.LBS) %>%
    group_by(year, month, day) %>%
    summarize(landings = sum(CONVERTED.WGHT.LBS) * lbs_to_kilos,
              count =  n()) %>%
    ungroup()
  discards <- discards %>%
    complete(LANDING.DATE = seq.Date(min(LANDING.DATE), max(LANDING.DATE), by = "day")) %>%
    mutate(year = year(LANDING.DATE),
           month = month(LANDING.DATE),
           day = day(LANDING.DATE)) %>%
    select(year, month, day, RELEASED.WT) %>%
    group_by(year, month, day) %>%
    summarize(landings = sum(RELEASED.WT) * lbs_to_kilos,
              count =  n()) %>%
    ungroup()

  full_join(dmp, discards, by = c("year", "month", "day")) %>%
    group_by(year, month, day) %>%
    mutate(landings.x = ifelse(is.na(landings.x), 0, landings.x),
           landings.y = ifelse(is.na(landings.y), 0, landings.y)) %>%
    summarize(landings = landings.x + landings.y,
              landings_count = count.x,
              discards_count = count.y) %>%
    ungroup() %>%
    arrange(year, month, day)
}

#' Remove some trip types from a data frame
#'
#' @param d A data frame containing the column `trip_type_colname`
#' @param types A vector of strings to grep for in the `trip_type_colname` column for removal
#' @param trip_type_colname The column name containing the trip type strings
#' @return A data frame with the data removed for the trip types
#' @export
remove_trip_data <- function(d, types, trip_type_colname){
  for(ty in types){
    gr <- grep(ty, d[[trip_type_colname]])
    if(length(gr)){
      d <- d[-gr,]
    }
  }
  d
}

#' Strip header lines of LOGBOOK file
#'
#' @description Search the file for the first occurance of the line starting with 'LOGBOOK'
#'   and remove it, and any following blank lines leaving only the column headers and data.
#'   Overwrite the file with this.
#' @param file filename (csv file)
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

#' Read in SQL code from a package file
#'
#' @param fn Filename for SQL code
#'
#' @return a vector of charcter strings, one for each line in `fn``
read_sql <- function(fn) {
  if(file.exists(system.file("sql", fn, package = "hakedata"))){
    readLines(system.file("sql", fn, package = "hakedata"))
  }else{
    stop("The sql file does not exist.", call. = FALSE)
  }
}

#' Inject SQL code for fishery categories and types based on the fleet type
#'
#' @description Injects GFBIO VESSEL_REGISTRATION_NUMBERs where '-- inject vessel codes here' appear in the SQL code and
#' and TRIP_CATEGORYs where '-- inject fishery categories here' appear
#'
#' @param sql SQL code as a vector of character strings (from [readLines()])
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
#' @param type One of 'ft', 'ss', or 'jv' for Freezer Trawler, Shoreside, and Joint Venture respectively
#' @param overwrite_file If TRUE, and the function has been run before and an rds file generated,
#'  overwrite this. If FALSE, run the SQL and generate the file.
#'
#' @return A data frame of the records returned for the SQL code given
#' @export
#' @importFrom gfdata run_sql
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
get_spatial_catch_sql <- function(type, overwrite_file = FALSE){
  cache_dir <- here("data-cache")
  if(!dir.exists(cache_dir)){
    dir.create(cache_dir)
  }
  if(type == "ft"){
    file <- here("data-cache", hake_catch_ft_file)
  }else if(type == "ss"){
    file <- here("data-cache", hake_catch_ss_file)
  }else if(type == "jv"){
    file <- here("data-cache", hake_catch_jv_file)
  }else{
    stop("type must be one of 'ft', 'ss', or 'jv'", call. = FALSE)
  }
  if(overwrite_file || !file.exists(file)){
    sql <- inject_fishery_filter(read_sql(spatial_catch_sql_file), type)
    d <- mutate(as_tibble(run_sql("GFFOS", sql)), fishery = type)
    saveRDS(d, file)
  }
  readRDS(file)
}
