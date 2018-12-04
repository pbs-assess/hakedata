# hakedata
Data extraction for Pacific hake annual assessment

## How to use this package
Make sure the species is correct at the top of **R/globals.R**

To re-acquire data from the database into a local RDS file (must be on DFO intranet):
`fetch_data()`

To load the data from the RDS file into an R workspace:
`d <- load_data()`
