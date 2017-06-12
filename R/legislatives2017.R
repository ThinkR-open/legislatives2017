
#' @import shiny
#' @import leaflet
#' @export
app <- function(...){
  runApp( system.file("shiny", package = "legislatives2017"), ... )
}

