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
      st_set_crs(crs) #%>%
      #st_transform(crs = 3347)
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
#'
#' @return An `sf` `MULTIPOLYGON` object with columns added: `cell` and `num_fids`
#' @export
#' @importFrom sf st_make_grid st_cast st_as_sf st_transform st_within st_intersects st_crs<-
#' @importFrom dplyr mutate filter left_join summarize group_by
#' @importFrom tibble as_tibble
make_grid <- function(d,
                      cell_size = 10000,
                      min_num_fids = 3){
  stopifnot("sf" %in% class(d),
            length(min_num_fids) == 1,
            is.numeric(min_num_fids),
            min_num_fids >= 0)

  j <- st_make_grid(d, cellsize = cell_size) %>%
    st_transform(crs = 3347)
  wth <- st_within(d, j)
  wth_tf <- lengths(wth)
  d <- d[wth_tf, ]
  wth_tbl <- as_tibble(wth)
  d <- d %>%
    mutate(cell = wth_tbl$col.id)
  num_in_cells <- d %>%
    as_tibble() %>%
    group_by(cell) %>%
    summarize(num_fids = n())
  jj <- st_cast(j, "MULTIPOLYGON")
  jj_tbl <- as_tibble(jj) %>%
    mutate(cell = 1:n()) %>%
    left_join(num_in_cells, by = "cell")
  message("Number of cells removed due to privacy restrictions: ", nrow(jj_tbl %>% filter(num_fids < min_num_fids)))
  jj_tbl %>%
    mutate(num_fids = ifelse(is.na(num_fids), 0, num_fids))%>%
    filter(num_fids >= min_num_fids) %>%
    st_as_sf()
  # if(cut_coast){
  #   coast <- coast_10m
  #   coast <- coast %>%
  #     `st_crs<-`(4326) %>%
  #     st_transform(crs = 3347)
  #   k <- st_within(jj, coast)
  #   k_tbl <- as_tibble(k)
  #   cut_cells <- unique(k_tbl$col.id)
  #   jj_tbl <- as_tibble(jj)
  #   jj <- jj_tbl %>%
  #     filter(!cell %in% cut_cells) %>%
  #     st_as_sf()
  # }
}
