#' Convert output from the [grDevices::contourLines()] function to an [sf::sfc] object
#'
#' @param lst List as returned from the [grDevices::contourLines()] function
#' @param vals A vector of depth values to use. Each will be filtered and the returned list will
#'   hakev one element for each depth
#' @param crs Coordinate reference system represented by an integer
#'
#' @details Creates a list of `LINESTRING` objects which represent contours on the BC coast.
#'   The data are transformed to NAD83 for congruence with the rest of this package.
#' @return An [sf::sfc] object
#' @export
#' @importFrom sf st_linestring st_sfc st_set_crs st_transform
contours_as_sfg <- function(lst, vals, crs = 4326){
  ret_lst <- NULL
  for(i in seq_along(vals)){
    lines <- lapply(lst, function(j){
      st_linestring(as.matrix(data.frame(x = j$x, y = j$y)))
    })
    ret_lst[[i]] <- st_sfc(lines) %>%
      st_set_crs(crs)
  }
  ret_lst
}

#' Create a grid based on fishing event data.
#'
#' @description Creates a grid of cells for which fishing event data exist. The cells with zero fishing
#'   events and cells which have less than `min_num_fids` will be removed.
#'
#' @param d An `sf` `POINT` object
#' @param min_num_fids Minimum number of FIDs in a cell. If less than this, they will be set to zero
#' @param cell_size Cell side length in meters
#' @param data_col A vector of column names found in `d` to be summed and appended to the return data frame
#' @param data_fncs A vector of functions to use for summarization of the `data_col` columns. Must be the same
#'   length as the `data_col` vector
#'
#' @return A list of length 2 with the first element being an `sf` `MULTIPOLYGON` object with
#'   columns added: `cell` and `num_fids`, and the second element being the number of cells removed
#'   due to the filtering of data to accomodate the rule of `min_num_fids`
#' @export
#' @importFrom sf st_make_grid st_cast st_as_sf st_transform st_within st_intersects st_crs<-
#' @importFrom dplyr mutate filter left_join summarize group_by summarize_at ungroup
#' @importFrom tibble as_tibble
make_grid <- function(d,
                      cell_size = 10000,
                      min_num_fids = 3,
                      data_col = NA,
                      data_fncs = NA){
  stopifnot("sf" %in% class(d),
            length(min_num_fids) == 1,
            is.numeric(min_num_fids),
            min_num_fids >= 0,
            length(data_col) == length(data_fncs))

  j <- st_make_grid(d, cellsize = cell_size)
  wth <- st_within(d, j)
  # Within grid cells TRUE/FALSE vector
  wth_tf <- as.logical(lengths(wth))
  # Only allow points found within grid cells
  d <- d[wth_tf, ]
  # Convert from 'Sparse geometry binary predicate (sgbp)' object to tibble
  wth_tbl <- as_tibble(wth)
  d <- d %>%
    mutate(cell = wth_tbl$col.id)
  # Compute number of fishing events and join them into the main table
  num_in_cells <- d %>%
    as_tibble() %>%
    group_by(cell) %>%
    summarize(num_fids = n()) %>%
    ungroup()
  jj <- st_cast(j, "MULTIPOLYGON")
  jj_tbl <- as_tibble(jj) %>%
    mutate(cell = 1:n()) %>%
    left_join(num_in_cells, by = "cell")
  if(!is.na(data_col[1])){
    # Compute summaries of data columns and join them into the main table
    vals_in_cells <- d %>%
      as_tibble() %>%
      group_by(cell) %>%
      summarize_at(data_col, data_fncs) %>%
      ungroup()
    jj_tbl <- jj_tbl %>%
      left_join(vals_in_cells, by = "cell")
  }
  num_cells_removed <- nrow(jj_tbl %>% filter(num_fids < min_num_fids))
  message("Number of cells removed due to privacy restrictions: ", num_cells_removed)
  # Filter for the number of fishing events required per cell before returning
  # Note this may not be enough for some species, vessel ID should be looked at and
  # if there are not at least `min_num_fids` of them the cell should be rejected
  list(jj_tbl %>%
         mutate(num_fids = ifelse(is.na(num_fids), 0, num_fids))%>%
         filter(num_fids >= min_num_fids) %>%
         st_as_sf(),
       num_cells_removed)
}

#' Merge all dataframes into one spatial dataframe. See [sf] package
#'
#' @description This function merges all data frames into a single [sf] data frame. It also
#'   removes points founds on land or way out of the fishing area. The Fishing IDs (FIDs) for
#'   this filtration were found using [mapview::mapview()].
#' @param ... Dataframes to merge. They must all have the columns `X` and `Y`
#'
#' @return a spatial data frame as described in the [sf] package
#' @export
#' @importFrom dplyr bind_rows filter
#' @importFrom sf st_as_sf st_distance st_coordinates st_transform
merge_into_spatial <- function(...){
  dfs <- list(...)
  lapply(1:length(dfs), function(x){
    if(!"X" %in% names(dfs[[x]]) || !"Y" %in% names(dfs[[x]])){
      stop("Data frame ", x, " does not have columns named 'X' and 'Y'.",
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
