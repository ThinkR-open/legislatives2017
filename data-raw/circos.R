library(readr)
library(dplyr)
library(geojsonio)

circos <- geojson_read( "data-raw/france-circonscriptions-legislatives-2012.json", method = "local", what = "sp")
use_data( circos, overwrite = TRUE )
