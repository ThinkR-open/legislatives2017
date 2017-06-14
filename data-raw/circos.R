library(readr)
library(dplyr)
library(geojsonio)
library(tibble)
library(stringr)

circos <- geojson_read( "data-raw/france-circonscriptions-legislatives-2012.json", method = "local", what = "sp")

circos@data <- as_tibble(mutate( circos@data,
  num_circ = as.numeric(as.character(num_circ)),
  code_dpt = as.character(code_dpt),
  ID = as.character(ID),
  nom_reg = as.character(nom_reg),
  code_reg = as.character(code_reg),
  nom_dpt = str_to_title(nom_dpt)
))
use_data( circos, overwrite = TRUE )
