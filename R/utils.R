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
  wth_tbl <- wth %>% as.data.frame() %>% as_tibble()
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

