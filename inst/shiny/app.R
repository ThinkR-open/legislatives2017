library(shiny)
library(leaflet)
library(legislatives2017)
library(dplyr)
library(stringr)
library(htmltools)
library(purrr)

ui <- navbarPage( "Legislatives 2017", theme = "legislatives.css",

  tabPanel("Abstention",
    div( class = "fullpage",
      leafletOutput( "carte_abstention", width="100%", height="100%" )
    )
  )

)

server <- shinyServer(function(input, output){

  data_abstention <- premier_tour %>%
    distinct(dpt, circ, .keep_all = TRUE) %>%
    select(dpt, circ, Inscrits:Exprimes) %>%
    left_join( circos@data, ., by = c( code_dpt = "dpt", num_circ = "circ")) %>%
    mutate( p_abstention = Abstentions / Inscrits )

  output$carte_abstention <- renderLeaflet({
    abst <- data_abstention$p_abstention
    col <- gray( 1 - ( abst - min(abst) ) / ( max(abst) - min(abst) ) )

    labels <- with( data_abstention, sprintf( "%s (circonscription %d) <hr/>%d inscrits<br/>%d abstentions (%4.2f %%)", str_to_title(nom_dpt), num_circ, Inscrits, Abstentions, round(100*p_abstention, 2 ) )) %>%
      map(HTML)

    leaflet(circos) %>%
      addTiles( urlTemplate = 'http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png' ) %>%
      setView(lng = 5, lat= 47, zoom=6) %>%
      addPolygons( color = "black", weight = 1, fillColor = col, fill = TRUE, fillOpacity = 1, label = labels,
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
