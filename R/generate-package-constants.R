## This code is not exported, but the constants it creates are located in RDA files in the data directory
## Add new package constants here, and re-source this file to re-generate
## the constants.rda file.

library(tibble)
species_name <- "pacific hake"

major_hake_areas <- c("03", "04", "05", "06", "07", "08", "09")

## Viking Alliance - new vessel
ft <- tibble(Vessel = c("Viking Enterprise",
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

save(species_name, file = "data/species_name.rda")
save(ft, major_hake_areas, file = "data/ft.rda")
save(major_hake_areas, file = "data/major_hake_areas.rda")
