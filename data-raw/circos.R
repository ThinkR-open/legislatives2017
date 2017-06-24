library(readr)
library(dplyr)
library(sf)
library(tibble)
library(stringr)

circonscriptions <- read_sf( "data-raw/france-circonscriptions-legislatives-2012.json" ) %>%
  mutate(
    num_circ = as.numeric(num_circ),
    nom_dpt = str_to_title(nom_dpt),
    nom_reg = str_to_title(nom_reg)
  )

use_data( circonscriptions, overwrite = TRUE )

circos <- geojson_read( "data-raw/france-circonscriptions-legislatives-2012.json", method = "local", what = "sp")

circos@data <- as_tibble(mutate( circos@data,
  num_circ = as.numeric(as.character(num_circ)),
  code_dpt = as.character(code_dpt),
  ID = as.character(ID),
  nom_reg = as.character(nom_reg),
  code_reg = as.character(code_reg),
  nom_dpt = str_to_title(nom_dpt),
  nom_reg = str_to_title(nom_reg)
))
use_data( circos, overwrite = TRUE )

expat <- data_frame(
  code_dpt = "99",
  nom_dpt = "Français établis hors de France",
  code_reg = "99",
  nom_reg = "Français établis hors de France"
)

regions <- circos@data %>%
  select( code_dpt, nom_dpt, nom_reg, code_reg ) %>%
  distinct(code_dpt, code_reg, .keep_all = TRUE) %>%
  bind_rows( expat ) %>%
  mutate(
    nom_reg = str_replace_all( nom_reg, "[-]", " "),
    nom_dpt = str_replace_all( nom_dpt, "[-]", " ")
  )

use_data( regions, overwrite = TRUE )

