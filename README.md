# hakedata
Data extraction for Pacific hake annual assessment

## How to use this package
To re-acquire data from the database into a local RDS file (must be on DFO intranet):
`fetch_data()`

To load the data from the RDS file into an R workspace:
`d <- load_data()`

To get the Freezer Trawler catch:
`ft_catch <- get_catch()`

To get the Shoreside catch:
`ss_catch <- get_catch(vessels.include = FALSE)`


If you want to add a new constant to the package, edit **R/generate-package-constants.R**
and source the file. Doing this will re-create the package data file **data/constants.rda**.
