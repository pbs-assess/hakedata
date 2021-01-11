#' Fetch the DMP catch from the FOS database
#'
#' @param end_date The end date of the date extraction
#' @param overwrite Logical. Overwrite the RDS files for DMP and LOGS data if they exist
#'
#' @return Nothing
#' @export
#'
#' @importFrom gfdata run_sql
#' @importFrom here here
fetch_catch_data <- function(end_date = format(Sys.Date(), "%d/%m/%Y"),
                             overwrite = FALSE){

  if(overwrite || !file.exists(here("data", dmp_catch_data_raw_file))){
    dmp <- run_sql("GFBIOSQL", read_sql(dmp_catch_sql_file))
    saveRDS(dmp, here("data", dmp_catch_data_raw_file))
  }
  if(overwrite || !file.exists(here("data", logs_catch_data_raw_file))){
    logs <- run_sql("GFBIOSQL", read_sql(logs_catch_sql_file))
    saveRDS(logs, here("data", logs_catch_data_raw_file))
  }
}

#' Loads spatial catch data from RDS files found in the data directory
#'
#' @return The tibble with the data for the fishery given
#' @export
load_spatial_catch_data <- function(fishery = "ft"){
  if(fishery == "ft"){
    return(readRDS(here("data-cache", hake_catch_ft_file)))
  }else if(fishery == "ss"){
    return(readRDS(here("data-cache", hake_catch_ss_file)))
  }else if(fishery == "jv"){
    return(readRDS(here("data-cache", hake_catch_jv_file)))
  }else{
    stop("fishery must be one of 'ft', 'ss', or 'jv'", call. = FALSE)
  }
}

#' Loads data from RDS files found in the data directory
#'
#' @param min_date Earliest date to include in the data returned
#'
#' @description The function [fetch_catch_data()] must be run first to extract the data from the database into RDS files
#' @return A list of three dataframes, `[[1]]` for the DMP data, `[[2]]` for the LOGS data, and `[[3]]` for the area 4B
#'   data which were not included. `[[3]]` is returned as a convenience for you to see if any other data from area 4B
#'   should be included
#' @export
#' @importFrom readr read_csv
#' @importFrom purrr map reduce
#' @importFrom here here
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom lubridate as_date
#' @importFrom dplyr rename
load_catch_data <- function(min_date = as.Date("2007-04-01")){

  # DMP LANDINGS
  dmp <- readRDS(here("data", dmp_catch_data_raw_file)) %>%
    as_tibble() %>%
    rename(LANDING_DATE = LANDING_DTT) %>%
    filter(!is.na(LANDING_DATE))

  dmp$LANDING_DATE <- as.Date(dmp$LANDING_DATE, "%B %d %Y")
  dmp <- dmp %>%
    filter(LANDING_DATE >= min_date)
  dmp <- remove_trip_data(dmp,
                          types = c("GULF", "OPT B"),
                          trip_type_colname = "LICENCE_TRIP_TYPE")

  # LOGS LANDINGS
  data_path <- here("data")
  logs_files <- dir(data_path, pattern = logs_pattern)
  map(logs_files, ~strip_lines(file.path(data_path, .)))
  logs <- map(logs_files, ~read_csv(file.path(data_path, .)))
  logs <- reduce(logs, rbind)
  # Replace all spaces in column names with underscores
  names(logs) <- gsub(" +", "_", names(logs))
  names(logs) <- gsub("/", "_", names(logs))
  logs$LANDING_DATE <- as.Date(logs$LANDING_DATE, "%B %d %Y")
  logs <- remove_trip_data(logs,
                           types = c("GULF", "OPT B"),
                           trip_type_colname = "TRIP_TYPE")
  area4b <- NULL
  if(any(logs$AREA == "4B")){
    area4b <- filter(logs, AREA == "4B")
    logs <- filter(logs,
                   LANDING_PORT != "FRENCH CREEK",
                  !(AREA == "4B" && month < 6))
    message("Some of the LOGS landings are in the 4B area. They ",
            "are stored in the third list element returned from this function for manual checking. ",
            "All landings from French Creek were removed, and those landed ",
            "before June in 4B, but you may want to remove some other records.")
  }
  list(dmp, logs, area4b)
}

#' Get the catch data from the DMP and LOGS data frames
#'
#' @param d a list of length 2 with elements being (1) The Dockside Monitoring Program
#'  data frame as extracted by [load_catch_data()] and (2) the Logbook data frame
#'  as extracted by [load_catch_data()]
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
      filter(!grepl("JV", LICENCE_TRIP_TYPE))
    logs <- logs %>%
      filter(!grepl("JV", TRIP_TYPE))
  }
  # At Sea Observer Program records only (all of those are discards in the LOGS data), for all fleet types
  discards <- logs %>%
    filter(SOURCE == "ASOP",
           !is.na(RELEASED_WT))
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
      filter(grepl("JV", LICENCE_TRIP_TYPE))
    discards <- discards %>%
      filter(grepl("JV", TRIP_TYPE))
  }

  dmp <- dmp %>%
    complete(LANDING_DATE = seq.Date(min(LANDING_DATE), max(LANDING_DATE), by = "day")) %>%
    mutate(year = year(LANDING_DATE),
           month = month(LANDING_DATE),
           day = day(LANDING_DATE),
           CONVWT = as.numeric(CONVWT)) %>%
    select(year, month, day, CONVWT) %>%
    group_by(year, month, day) %>%
    summarize(landings = sum(CONVWT) * lbs_to_kilos,
              count =  n()) %>%
    ungroup() %>%
    mutate(landings = ifelse(is.na(landings), 0, landings))

  discards <- discards %>%
    complete(LANDING_DATE = seq.Date(min(LANDING_DATE), max(LANDING_DATE), by = "day")) %>%
    mutate(year = year(LANDING_DATE),
           month = month(LANDING_DATE),
           day = day(LANDING_DATE)) %>%
    select(year, month, day, RELEASED_WT) %>%
    group_by(year, month, day) %>%
    summarize(landings = sum(RELEASED_WT) * lbs_to_kilos,
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

#' Create a CSV file with catch by year and month for a particular fishery
#'
#' @param df The data frame to reduce into a CSV. This will be output from the [get_catch()] function
#' @param fn Filename for the CSV
#'
#' @export
#' @importFrom reshape2 dcast
#' @importFrom dplyr group_by summarize
#' @importFrom readr write_csv
create_catch_csv_file <- function(df,
                                  fn = NA){
  stopifnot(!is.na(fn))
  d <- df %>%
    group_by(year, month) %>%
    summarize(catch = sum(landings) / 1000)
  d <- dcast(d, year ~ month )
  d[is.na(d)] <- 0
  write_csv(d, fn)
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

