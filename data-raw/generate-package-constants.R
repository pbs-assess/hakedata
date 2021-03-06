## This code is not exported, but the constants it creates are located in RDA files in the data directory
## Add new package constants here, and re-source this file to re-generate
## the constants.rda file.

species_name <- "pacific hake"
spatial_catch_sql_file <- "spatial-catch.sql"
dmp_catch_sql_file <- "dmp-catch-stored-proc.sql"
dmp_catch_data_raw_file <- "dmp-catch-data.rds"
survey_data_file <- "survey-data.rds"
logs_catch_sql_file <- "logs-catch-stored-proc.sql"
logs_catch_data_raw_file <- "logs-catch-data.rds"
sample_data_raw_file <- "sample-data.rds"
hake_catch_ft_file <- "pacific-hake-catch-ft.rds"
hake_catch_ss_file <- "pacific-hake-catch-ss.rds"
hake_catch_jv_file <- "pacific-hake-catch-jv.rds"
dmp_file <- "LandingsSpeciesDateDMP.csv"
logs_pattern <- "^LogCatchReport[0-9]{4}\\.csv$"

major_hake_areas <- c("03", "04", "05", "06", "07", "08", "09")

freezer_trawlers <-
  tibble::tibble(Vessel = c("Viking Enterprise",
                            "Northern Alliance",
                            "Osprey #1",
                            "Raw Spirit",
                            "Pacific Legacy #1",
                            "Sunderoey",
                            "Viking Alliance"),
                 FOS.ID = c(310913,
                            312275,
                            310988,
                            312405,
                            313334,
                            313464,
                            313224),
                 GFBIO.ID = c(568,
                              592,
                              569,
                              595,
                              608,
                              NA,
                              1727))


shoreside_trip_type <- 12764
jv_trip_type <- 12766
lbs_to_kilos <- 0.4535924

usethis::use_data(dmp_file, overwrite = TRUE)
usethis::use_data(spatial_catch_sql_file, overwrite = TRUE)
usethis::use_data(dmp_catch_sql_file, overwrite = TRUE)
usethis::use_data(dmp_catch_data_raw_file, overwrite = TRUE)
usethis::use_data(sample_data_raw_file, overwrite = TRUE)
usethis::use_data(survey_data_file, overwrite = TRUE)
usethis::use_data(logs_catch_sql_file, overwrite = TRUE)
usethis::use_data(logs_catch_data_raw_file, overwrite = TRUE)
usethis::use_data(hake_catch_ft_file, overwrite = TRUE)
usethis::use_data(hake_catch_ss_file, overwrite = TRUE)
usethis::use_data(hake_catch_jv_file, overwrite = TRUE)

usethis::use_data(species_name, overwrite = TRUE)
usethis::use_data(freezer_trawlers, overwrite = TRUE)
usethis::use_data(major_hake_areas, overwrite = TRUE)
usethis::use_data(logs_pattern, overwrite = TRUE)
usethis::use_data(lbs_to_kilos, overwrite = TRUE)
#usethis::use_data(far_point, overwrite = TRUE)

# Note the far_point.rda data is a single point in the sf class representing the lowest-right point in the hake fishery spatially
# across all fisheries (all FIDs). This was recorded using code found in the pbs-assess/hake/R/get-data.R and saved in the data
# directory here using: save(far_point, "../hakedata/data/far_point.rda")
