## This code is not exported, but the constants it creates are located in data/constants.rda
## Add new package constants here, and re-source this file to re-generate
## the constants.rda file.

library(tibble)
species_name <- "pacific hake"

major_hake_areas <- c("03", "04", "05", "06", "07", "08", "09")

ft <- tibble(Vessel = c("Viking Enterprise",
                        "Northern Alliance",
                        "Osprey #1",
                        "Raw Spirit"),
             ID = c(310913,
                    312275,
                    310988,
                    312405))

save(species_name, ft, file = "data/constants.rda")
