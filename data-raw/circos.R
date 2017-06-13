library(readr)
library(dplyr)
library(geojsonio)

circos <- geojson_read( "data-raw/france-circonscriptions-legislatives-2012.json", method = "local", what = "sp")

circos@data <- mutate( circos@data,
  num_circ = as.numeric(num_circ),
  code_dpt = as.character(code_dpt),
  ID = as.character(ID),
  nom_reg = as.character(nom_reg),
  code_reg = as.character(code_reg)
)
use_data( circos, overwrite = TRUE )
