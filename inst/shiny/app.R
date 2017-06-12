library(shiny)
library(leaflet)
library(legislatives2017)

ui <- fluidPage(
  div( style = "position: fixed; top: 0; bottom: 0; right:0; left:0; ",
    leafletOutput( "carte", width="100%", height="100%" )
  )
)

server <- shinyServer(function(input, output){
  output$carte <- renderLeaflet({
    leaflet(circos) %>%
      addTiles( urlTemplate = 'http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png' ) %>%
      setView(lng = 5, lat= 45, zoom=5) %>%
      addPolygons( color = "black", weight = 1, fillColor = "black", fill = TRUE, fillOpacity = 0.6,
        highlightOptions = highlightOptions(weight = 2, fillOpacity = 1, bringToFront = TRUE),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      )
  })
})

shinyApp( ui = ui, server = server )
