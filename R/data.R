#' Constants for the hakedata package
#'
#' Name of the Dockside Monitoring Program data file (CSV file)
#'
#' @format A character string
"dmp_file"
#' Name of the spatial catch SQL template file
#'
#' @format A character string
"spatial_catch_sql_file"
#' Name of the DMP catch SQL file
#'
#' @format A character string
"dmp_catch_sql_file"
#' Name of the LOGS catch SQL file
#'
#' @format A character string
"logs_catch_sql_file"
#' Name of the commerical samples rds file
#'
#' @format A character string
"sample_data_raw_file"
#' Name of the survey data rds file
#'
#' @format A character string
"survey_data_file"
#' Name of the DMP catch rds file
#'
#' @format A character string
"dmp_catch_data_raw_file"
#' Name of the LOGS catch rds file
#'
#' @format A character string
"logs_catch_data_raw_file"
#' Name of the freezer trawler catch rds file
#'
#' @format A character string
"hake_catch_ft_file"
#' Name of the shoreside catch rds file
#'
#' @format A character string
"hake_catch_ss_file"
#' Name of the joint venture catch rds file
#'
#' @format A character string
"hake_catch_jv_file"
#' Name of the species
#' @format A character string
"species_name"
#' Major PFMA areas used when assessing hake
#'
#' @format A character vector
"major_hake_areas"
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
#' A list of length-3 lists containing points which can be used to generate contour lines on the BC coast
#'
#' @format A list
"bc_bathymetry"
#' An sf LINESTRING object which contains data for 10m resolution for the world coastline. Data source:
#' #' http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip
#'
#' @format An sf LINESTRING object
"coast_10m"
#' An sf class POINT which represents a point at the lower rightmost part of the fishery distribution
#' to be used to measure disctance to other point for removal of outlier points
#'
#' @format An sf POINT object
"far_point"
