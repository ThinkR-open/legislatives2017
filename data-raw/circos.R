library(readr)
library(dplyr)
library(sf)
library(tibble)
library(stringr)

circonscriptions <- read_sf( "data-raw/france-circonscriptions-legislatives-2012.json" )

regions <- circonscriptions %>%
  select( code_dpt, nom_dpt, nom_reg, code_reg ) %>%
  distinct(code_dpt, code_reg, .keep_all = TRUE) %>%
  mutate(
    nom_reg = str_replace_all( nom_reg, "[-]", " "),
    nom_dpt = str_replace_all( nom_dpt, "[-]", " ")
  )

circonscriptions <- select( circonscriptions, ID, geometry )

use_data( circonscriptions, overwrite = TRUE )
use_data( regions, overwrite = TRUE )
