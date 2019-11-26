#' Constants for the hakedata package
#'
#'
#' A dataset containing the major statistical area codes and descriptions.
#'
#' Name of the species
#' @format A character string
"species_name"
#' Major PFMA areas used when assessing hake
#'
#' @format A character vector
"major_hake_areas"
#' Name of the Dockside Monitoring Program data file (CSV file)
#'
#' @format A character string
"dmp_file"
#' Data frame with the freexer trawler names and unique IDs by database
#'
#' @format A data frame:
#' \describe{
#'   \item{Vessel}{vessel name}
#'   \item{FOS.ID}{Unique ID value in the FOS database}
#'   \item{GFBIO.ID}{Unique ID value in the GFBIO database}
#' }
"freezer_trawlers"
#' Conversion factor from pounds to kilograms
#'
#' @format A double
"lbs_to_kilos"
#' Pattern for regular expressions to use for LOGS data files
#'
#' @format A character string
"logs_pattern"
#' Name of the spatial catch SQL template file
#'
#' @format A character string
"spatial_catch_sql_file"
