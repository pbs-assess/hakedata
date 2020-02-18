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
fetch_spatial_catch_data <- function(type, overwrite_file = FALSE){
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

#' Merge all dataframes into one spatial dataframe. See [sf] package
#'
#' @description This function converts all supplied data frames into a single [sf] data frame. It also
#'   removes points found on land or way out of the fishing area. The Fishing IDs (FIDs) for
#'   this filtration were found using [mapview::mapview()].
#' @param ... Dataframes to merge. They must all have the columns `X` and `Y`
#'
#' @return a spatial data frame as described in the [sf] package
#' @export
#' @importFrom dplyr bind_rows filter
#' @importFrom sf st_as_sf st_distance st_coordinates st_transform
conv_spatial <- function(...){
  dfs <- list(...)
  lapply(seq_len(length(dfs)), function(x){
    browser()
    if(!"X" %in% names(dfs[[x]]) || !"Y" %in% names(dfs[[x]])){
      stop("Data frame ", x, " does not have columns named 'X' and 'Y'.",
           call. = FALSE)
    }
    if(nrow(dfs[[x]]) == 0){
      stop("Data frame ", x, " does not have any rows.",
           call. = FALSE)
    }
  })
  d <- bind_rows(dfs)
  dsf <- st_as_sf(d,
                  coords = c("X", "Y"),
                  crs = 4326,
                  na.fail = FALSE) %>%
    st_transform(crs = 3347)
  # Remove fishing events farther than 1,000km away from the most lower right FID (FID = 859834)
  # See the bottom of pbs-asses/hakedata/data-raw/generate-package-constants.R for details
  # far_point <- (dsf %>% filter(FID == 859834))$geometry
  dsf <- dsf %>%
    filter(as.numeric(st_distance(geometry, far_point)) < 1000000)
  # Remove fishing events below 49th parallel
  dsf <- dsf %>%
    filter(st_coordinates(geometry)[, 2] > 1930000)
  # Remove fishing events that appear on land. These were found by running mapview(hake) and
  # clicking points and reading off the FIDs.
  fid <- c(1792670, 2089167, 1052516, 1990665, 2089167, 1253522, 1260747, 1266165,  849652,
           849653,  849654,  880806, 1065997,  862371, 1065996, 1065490, 1059848,  978697,
           1810578, 1252855, 2615517, 1536631, 1965023,  859876, 1252360, 1266160, 1252806,
           849590, 1266176, 1065963, 1252850,  849711, 2589054, 1266177, 1052863, 1383616,
           1255488, 1052530,  864029, 2098357, 2414067, 2430126, 2464618,  870455, 1907167,
           865280, 1961875, 2105498, 2483676, 1979337, 1907165, 1068788, 1810803, 1068789,
           2482719, 2480395, 2643290, 1068790,  849031,  863598, 2643289, 2493572, 2436651,
           1565046, 1829211, 1971205, 1971204, 1971203, 1997038, 2320215, 1261214, 1253526,
           1059191, 1959005,  790192, 2740463, 2926792, 2774109, 2751936, 2906109, 2877133,
           2926341, 2796739, 2923734)
  filter(dsf, !FID %in% fid)
}

