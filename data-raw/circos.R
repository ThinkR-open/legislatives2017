library(readr)
library(dplyr)
library(rgdal)

circos <- readOGR( "data-raw", "circosSHP_v3")
use_data(circos, overwrite = TRUE)
