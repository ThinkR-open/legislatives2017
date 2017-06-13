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

  ECO = "green",

  COM = "red",

  DIV = "gray",
  REG = "gray"

)

thinkr_link <- function(){
  absolutePanel( class = "panel panel-default panel-side",
    style = "z-index: 1000",
    fixed = TRUE, draggable = TRUE,
    top  = 0, left = "auto", right = 20,

    width = "200px",
    div(
      tags$a( href = "http://www.thinkr.fr", tags$img(src="thinkR.png", height = "30px") ),
      tags$a( href = "https://github.com/ThinkRstat/legislatives2017", tags$img(src="https://cdn0.iconfinder.com/data/icons/octicons/1024/mark-github-256.png", height = "30px") ),
      tags$a( href = "https://twitter.com/thinkR_fr", tags$img(src="https://cdn3.iconfinder.com/data/icons/social-icons-5/128/Twitter.png", height = "30px") ),
      tags$a( href = "https://www.facebook.com/ThinkR-1776997009278055/", tags$img(src="https://cdn4.iconfinder.com/data/icons/social-messaging-ui-color-shapes-2-free/128/social-facebook-circle-128.png", height = "30px") )
    )

  )
}


nuances <- premier_tour %>% filter( resultat == "ballotage" ) %$% Nuances %>% as.character %>% unique

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
    ),
    thinkr_link()
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
    ),
    thinkr_link()
  ),

  tabPanel("Ballotages par parti",

    div( class = "fullpage",
      leafletOutput( "carte_ballotage", width="100%", height="100%" )
    ),

    absolutePanel( class = "panel panel-default panel-side",
      fixed = TRUE, draggable = TRUE,
      top = 60, left = "auto", right = 20, bottom = "auto",
      width = 500, height= "auto",

      selectInput("sel_ballotage", label = "Nuance", choices = nuances, selected = "FI"),
      textOutput("n_ballotage"),
      tags$hr(),
      DT::dataTableOutput("data_ballotage")
    ),
    thinkr_link()
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
    DT::datatable( data, filter = "top", options = list(pageLength = 20, scrollY = "200px"))
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

  data_ballotage <- reactive({
    nuance <- input$sel_ballotage

    filter( premier_tour, Nuances == nuance, resultat == "ballotage" ) %>%
      left_join( circos@data, by = c( dpt = "code_dpt", circ = "num_circ" ) ) %>%
      mutate( Score = round( 100* Voix / Exprimes, 2) )

  })


  output$carte_ballotage <- renderLeaflet({
    nuance <- input$sel_ballotage
    data <- data_ballotage()

    col  <- unname(couleurs[nuance])

    labels <- with(data, sprintf("%s (%4.2f)", candidat, Score )) %>% map(HTML)

    circos <- circos[ na.omit(match( data$ID, circos@data$ID)),  ]

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

  output$data_ballotage <- DT::renderDataTable({
    data <- data_ballotage() %>%
      select( candidat, nom_dpt, circ, Score )
    DT::datatable( data )
  })

  output$n_ballotage <- renderText({
    nuance <- input$sel_ballotage
    sprintf( "%s. %d candidat(e)s en ballotage", nuance, nrow(data_ballotage())  )
  })

})

shinyApp( ui = ui, server = server )
