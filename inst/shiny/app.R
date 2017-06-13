library(shiny)
library(leaflet)
library(legislatives2017)
library(dplyr)
library(stringr)
library(htmltools)
library(purrr)

couleurs <- c(
  REM  = "purple",
  MDM = "purple",

  LR = "blue",
  FN = "black",
  FI = "red",
  SOC = "pink",
  UDI = "blue",
  DVD = "blue",
  DVG = "pink",
  RDG = "pink",
  EXD = "black",
  DLF = "blue",

  COM = "gray",
  DIV = "gray",
  REG = "gray",
  ECO = "gray"

)


ui <- navbarPage( "Legislatives 2017", theme = "legislatives.css",

  tabPanel("Abstention",
    div( class = "fullpage",
      leafletOutput( "carte_abstention", width="100%", height="100%" )
    ),
    absolutePanel( class = "panel panel-default panel-side",
      fixed = TRUE, draggable = TRUE,
      top = 60, left = "auto", right = 20, bottom = "auto",
      width = 400, height= "auto",

      DT::dataTableOutput("data_abstention")
    )
  ),

  tabPanel("Premier",

    div( class = "fullpage",
      leafletOutput( "carte_premier", width="100%", height="100%" )
    ),

    absolutePanel( class = "panel panel-default panel-side",
      fixed = TRUE, draggable = TRUE,
      top = 60, left = "auto", right = 20, bottom = "auto",
      width = 500, height= "auto",

      DT::dataTableOutput("data_premier")
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
    # col <- gray( 1 - abst)

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

  output$data_abstention <- DT::renderDataTable({
    data <- select(data_abstention, nom_dpt, num_circ, p_abstention ) %>%
      arrange( desc(p_abstention) ) %>%
      mutate(
        p_abstention = round(100*p_abstention,2),
        nom_dpt = str_to_title(nom_dpt)
      )
    DT::datatable( data )
  })


  data_premier <- premier_tour %>%
    left_join( circos@data, ., by = c( code_dpt = "dpt", num_circ = "circ")) %>%
    filter( resultat %in% c("ballotage", "elu") ) %>%
    mutate( summary = sprintf( "%s (%s) :: %4.2f %%", candidat, Nuances, round(100 * Voix / Exprimes, 2 ) )  ) %>%
    group_by(code_dpt,num_circ) %>%
    summarise(
      Score  =  round(100*max(Voix / Exprimes), 2),
      Nuances = Nuances[ Voix == max(Voix)][1],
      candidat = candidat[Voix == max(Voix)][1],
      summary = paste( str_to_title(nom_dpt), "(", num_circ, ")<hr/>", paste( summary, collapse = "<br/>"), sep = "" )[1]
    ) %>%
    left_join( circos@data, ., by = c("code_dpt", "num_circ"))

  output$carte_premier <- renderLeaflet({
    col  <- unname(couleurs[ as.character(data_premier$Nuances) ])
    col[is.na(col)] <- "white"

    labels <- data_premier$summary %>% map(HTML)

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


  output$data_premier <- DT::renderDataTable({
    data <- data_premier %>%
      select(nom_dpt, num_circ,candidat, Nuances, Score) %>%
      mutate( nom_dpt = str_to_title(nom_dpt) ) %>%
      arrange( desc(Score) )
    DT::datatable( data )
  })


})

shinyApp( ui = ui, server = server )
