fetch_data <- function(file = rds_file,
                       species = species_name){
  d <- gfplot::get_commercial_samples(species)
  saveRDS(d, file)
}

load_data <- function(file = rds_file){
  readRDS(file)
}

