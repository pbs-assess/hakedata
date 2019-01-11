## This code is not exported, but the constants it creates are located in RDA files in the data directory
## Add new package constants here, and re-source this file to re-generate
## the constants.rda file.

species_name <- "pacific hake"

major_hake_areas <- c("03", "04", "05", "06", "07", "08", "09")

## Viking Alliance - new vessel
freezer_trawlers <-
  tibble::tibble(Vessel = c("Viking Enterprise",
                            "Northern Alliance",
                            "Osprey #1",
                            "Raw Spirit",
                            "Viking Alliance"),
                 FOS.ID = c(310913,
                            312275,
                            310988,
                            312405,
                            313224),
                 GFBIO.ID = c(568,
                              592,
                              569,
                              595,
                              1727))

usethis::use_data(species_name)
usethis::use_data(freezer_trawlers)
usethis::use_data(major_hake_areas)