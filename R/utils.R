#' Convert output from the [grDevices::contourLines()] function to an [sf::sfc] object
#'
#' @param lst List as returned from the [grDevices::contourLines()] function
#' @param vals A vector of depth values to use. Each will be filtered and the returned list will
#'   hakev one element for each depth
#'
#' @details Creates a list of `LINESTRING` objects which represent contours on the BC coast.
#'   The data are transformed to NAD83 for congruence with the rest of this package.
#' @return An [sf::sfc] object
#' @export
#' @importFrom sf st_linestring st_sfc st_set_crs st_transform
contours_as_sfg <- function(lst, vals){
  ret_lst <- NULL
  for(i in seq_along(vals)){
    lines <- lapply(lst, function(j){
      st_linestring(as.matrix(data.frame(x = j$x, y = j$y)))
    })
    ret_lst[[i]] <- st_sfc(lines) %>%
      st_set_crs(4326) %>%
      st_transform(crs = 3347)
  }
  ret_lst
}
