# hakedata
Data extraction for Pacific hake assessment

## How to use this package

# Fetch data from the server

**You must be on the DFO intranet and have read access to the GFBIOSQL and GFFOS databases to fetch the data**

Run the following code to create rds files in the `data` subdirectory of the working directory:

```
library(hakedata)
fetch_catch_data()
fetch_spatial_catch_data()
fetch_sample_data()
```

The sample data from `fetch_sample_data()` is filtered for major stat areas 03-09 and 01 with 
minor stat area 20 which represents the Strait of Juan De Fuca.

The catch data from `fetch_catch_data()` do not need to be filtered because they are based on landings from FOS and the stored procedure on the database server deals with the filtering.

The spatial catch data from `fetch_spatial_catch_data()` do not need to be filtered either. These come from SQL code
within the package which is injected with fishery types. These data are used for spatial maps only.

Once those have completed and the files exist, you can use the other functions in the package.

# Loading catch data into variables

`d <- load_catch_data()` will load two RDS files representing catch data:

1. DMP - Dockside monitoring. These records represent landings as recorded dockside.
2. LOGS - Observer logs. Discards are extracted from these records and added to the landings from the DMP data.

The function will return a list of three elements, the first two are dataframes representing DMP and LOGS catch,
the third is a dataframe containing LOGS data found in the 4B major area (Strait of Georgia). These should be manually checked for landing port and time of year. There have been several landings in Strait of Georgia ports which were
actually catch from outside, and these should be included in the total.

To get the catch by fishery type, run the following three lines. `"ft"` represents Freezer trawlers which carry up to
500 tonnes of frozen product and operate mainly out of Vancouver. For a list of these vessels, look at the `freezer_trawlers` dataframe. `"ss"` represents Shoreside, which are the small vessels based mostly based out of Ulcluelet. `"jv"` represents the Joint venture fishery which is operated by the Shoreside vessels delivering single catches directly to large foreign catcher-processors for processing at sea.

```
ct_ft <- get_catch(d, type = "ft")
ct_ss <- get_catch(d, type = "ss")
ct_jv <- get_catch(d, type = "jv")
```

To load spatial catch by fishery type and convert into spatial *sf* dataframes, run the following:

```
spct_ft <- get_spatial_catch_sql("ft")
spct_ss <- get_spatial_catch_sql("ss")
spct_jv <- get_spatial_catch_sql("jv")

spct_ft <- conv_spatial(spct_ft)
spct_ss <- conv_spatial(spct_ss)
spct_jv <- conv_spatial(spct_jv)
spct_all <- conv_spatial(spct_ft, spct_ss, spct_jv)
```

# Loading sample data into variables

To create weighted age proportions, a summary of the data was looked at:
```
ss <- sample_summary()
```

```ss[[1]]``` contains a summary dataframe of the various biological data by ```year```.

```ss[[2]]``` contains a summary dataframe of the various biological data by ```year``` and ```sample_id```.

Overall (all sectors) age proportions are extracted like this:
```
ap <- get_age_props()
```
See the R help ```?get_age_props``` for the algorithm details.




