species_name <- "pacific hake"

rootd <- here::here()
datad <- file.path(rootd, "data")
rds_file <- file.path(datad, paste0(gsub(" ", "-", species_name), ".rds"))


