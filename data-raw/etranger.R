library(purrr)
library(jsonlite)

f <- function( circ = 1, lng1, lng2, lat1, lat2){
  lng <- rep( c(lng1, lng2), each = 2 )

  lng <- c(lng1, lng1, lng2, lng2, lng1 )
  lat <- c(lat1, lat2, lat2, lat1, lat1 )

  data <- tibble(
    type = "Feature",
    geometry = list( list(
      type = "Polygon",
      coordinates = list( map2( lng, lat, c ))
    ) ),
    properties = list( list(
      ID = sprintf( "99%03d", circ ),
      code_dpt = "99",
      nom_dpt = "Français établis hors de France",
      nom_reg = "Français établis hors de France",
      num_circ = sprintf( "%d", circ ),
      code_reg = "99"
    )
  ))
  data
}

data <- list(
  f( 1, lng1 = -4.4, lng2 = -4, lat1 = 45.7, lat2 = 46  ),
  f( 2, lng1 = -4.4, lng2 = -4, lat1 = 45.3, lat2 = 45.6),
  f( 3, lng1 = -4.4, lng2 = -4, lat1 = 44.9, lat2 = 45.2),
  f( 4, lng1 = -4.4, lng2 = -4, lat1 = 44.5, lat2 = 44.8),

  f( 5, lng1 = -3.9, lng2 = -3.5, lat1 = 45.7, lat2 = 46  ),
  f( 6, lng1 = -3.9, lng2 = -3.5, lat1 = 45.3, lat2 = 45.6),
  f( 7, lng1 = -3.9, lng2 = -3.5, lat1 = 44.9, lat2 = 45.2),
  f( 8, lng1 = -3.9, lng2 = -3.5, lat1 = 44.5, lat2 = 44.8),

  f( 9, lng1  = -3.4, lng2 = -3, lat1 = 45.7, lat2 = 46  ),
  f( 10, lng1 = -3.4, lng2 = -3, lat1 = 45.3, lat2 = 45.6),
  f( 11, lng1 = -3.4, lng2 = -3, lat1 = 44.9, lat2 = 45.2)
)

map( data , toJSON) %>%
  map_chr( as.character) %>%
  gsub( "^[[]", "", . ) %>%
  gsub( "[]]$", "", . ) %>%
  gsub( '[[]"', '"', .) %>%
  gsub( '"[]]', '"', .) %>%
  paste( collapse = ", \n" ) %>%
  cat

