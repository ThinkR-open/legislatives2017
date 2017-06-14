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


nuances_ballotage <- premier_tour %>% filter( resultat == "ballotage" ) %$% Nuances %>% as.character %>% unique

rightPanel <- function(...){
  absolutePanel( class = "panel panel-default panel-side",
    fixed = TRUE, draggable = TRUE,
    top = 60, left = "auto", right = 20, bottom = "auto",
    width = 500, height= "auto",
    ...
  )
}

tabPanel_elections <- function( title, carte, ... ){
  tabPanel(title,
    div( class = "fullpage",
      leafletOutput( carte, width="100%", height="100%" )
    ),
    ...,
    thinkr_link()
  )
}

ui <- navbarPage( "Legislatives 2017", theme = "legislatives.css",

  tabPanel_elections("Abstention", "carte_abstention",
    rightPanel(
      DT::dataTableOutput("data_abstention")
    )
  ),

  tabPanel_elections("Premier", "carte_premier",
    rightPanel(
      DT::dataTableOutput("data_premier"),

      hr(),

      textOutput("selected_circonscription"),
      br(),
      DT::dataTableOutput("data_premier_details")
    )
  ),

  tabPanel_elections("Ballotages", "carte_ballotage",
    rightPanel(
      selectInput("sel_ballotage", label = "Nuance", choices = nuances_ballotage, selected = "FI"),
      textOutput("n_ballotage"),
      tags$hr(),
      DT::dataTableOutput("data_ballotage"),

      hr(),

      textOutput("selected_ballotage_circonscription"),
      br(),
      DT::dataTableOutput("data_ballotage_details")
    )
  )


)

server <- shinyServer(function(input, output){

  data_abstention <- premier_tour %>%
    distinct(dpt, circ, .keep_all = TRUE) %>%
    select(dpt, circ, Inscrits:Exprimes, p_abstentions) %>%
    left_join( circos@data, ., by = c( code_dpt = "dpt", num_circ = "circ"))

  output$carte_abstention <- renderLeaflet({
    abst <- data_abstention$p_abstentions / 100
    col <- gray( 1 - ( abst - min(abst) ) / ( max(abst) - min(abst) ) )

    labels <- with( data_abstention, sprintf( "%s (circonscription %d) <hr/>%d inscrits<br/>%d abstentions (%4.2f %%)", nom_dpt, num_circ, Inscrits, Abstentions, p_abstentions )) %>%
      map(HTML)

    leaflet(circos) %>%
      addTiles( urlTemplate = 'http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png' ) %>%
      setView(lng = 5, lat= 47, zoom=6) %>%
      addPolygons( color = "black", weight = .5, fillColor = col, fill = TRUE, fillOpacity = .5, label = labels,
        highlightOptions = highlightOptions(weight = 2, fillOpacity = .5, bringToFront = TRUE),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      )
  })

  output$data_abstention <- DT::renderDataTable({
    data <- select(data_abstention, nom_dpt, num_circ, p_abstentions ) %>%
      arrange( desc(p_abstentions) ) %>%
      mutate( p_abstentions = round(p_abstentions, 2)) %>%
      DT::datatable( filter = "top", options = list(pageLength = 20, scrollY = "350px"))
  })


  data_premier <- premier_tour %>%
    left_join( circos@data, ., by = c( code_dpt = "dpt", num_circ = "circ")) %>%
    filter( resultat %in% c("ballotage", "elu") ) %>%
    mutate( summary = sprintf( "%s (%s) :: %4.2f %%", candidat, Nuances, round(100 * Voix / Exprimes, 2 ) )  ) %>%
    group_by(code_dpt,num_circ) %>%
    summarise(
      Score  =  max(Score),
      Nuances = Nuances[ Voix == max(Voix)][1],
      candidat = candidat[Voix == max(Voix)][1],
      summary = paste( nom_dpt, "(", num_circ, ")<hr/>", paste( summary, collapse = "<br/>"), sep = "" )[1]
    ) %>%
    left_join( circos@data, ., by = c("code_dpt", "num_circ"))

  output$carte_premier <- renderLeaflet({
    col  <- unname(couleurs[ as.character(data_premier$Nuances) ])
    col[is.na(col)] <- "white"

    labels <- data_premier$summary %>% map(HTML)

    leaflet(circos) %>%
      addTiles( urlTemplate = 'http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png' ) %>%
      setView(lng = 5, lat= 47, zoom=6) %>%
      addPolygons( color = "black", weight = .5, fillColor = col, fill = TRUE, fillOpacity = .5, label = labels,
        highlightOptions = highlightOptions(weight = 2, fillOpacity = .5, bringToFront = TRUE),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        layerId = paste(circos@data$code_dpt, "-", circos@data$num_circ, sep = "")
      )
  })

  selected <- eventReactive(input$carte_premier_shape_click, {
    click <- strsplit(input$carte_premier_shape_click$id, "-")[[1]]
    list( dpt_ = click[1], circ_ = click[2])
  })

  data_premier_details <- reactive({
    sel <- selected()

    data <- premier_tour %>%
      filter( dpt == sel$dpt_, circ == sel$circ_ ) %>%
      select( candidat, Nuances, Voix, Score, resultat ) %>%
      mutate( Score = round(Score, 2)) %>%
      arrange( desc(Voix) )

    data

  })

  output$data_premier_details <- DT::renderDataTable({
    DT::datatable( data_premier_details(), options = list(pageLength = 5) )
  })

  output$selected_circonscription <- renderText({
    sel <- selected()
    dpt <- filter( circos@data, code_dpt == sel$dpt_ ) %>%
      head(1) %$%
      nom_dpt

    data <- premier_tour %>%
      filter( dpt == sel$dpt_, circ == sel$circ_ ) %>%
      head(1)

    sprintf( "%s (%s). %d inscrits, %d exprimés. abstention: %4.2f",
      dpt, sel$circ_, data$Inscrits, data$Exprimes, round(100*data$Abstentions/data$Inscrits, 2) )
  })


  output$data_premier <- DT::renderDataTable({
    data <- data_premier %>%
      select(nom_dpt, num_circ,candidat, Nuances, Score) %>%
      mutate( Score = round(Score, 2)) %>%
      arrange( desc(Score) ) %>%
      DT::datatable( filter = "top", options = list(pageLength = 5, scrollY = "200px") )
  })

  data_ballotage <- reactive({
    nuance <- input$sel_ballotage
    if( is.null(nuance) ) nuance <- "FI"

    filter( premier_tour, Nuances == nuance, resultat == "ballotage" ) %>%
      left_join( circos@data, by = c( dpt = "code_dpt", circ = "num_circ" ) ) %>%
      mutate( Score = round(Score, 2)) %>%
      arrange( desc(Score) )

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
      addPolygons( color = "black", weight = .5, fillColor = col, fill = TRUE, fillOpacity = .5, label = labels,
        highlightOptions = highlightOptions(weight = 2, fillOpacity = .5, bringToFront = TRUE),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        layerId = paste(circos@data$code_dpt, "-", circos@data$num_circ, sep = "")
      )
  })

  output$data_ballotage <- DT::renderDataTable({
    data <- data_ballotage() %>%
      select( candidat, nom_dpt, circ, Score )
    DT::datatable( data , filter = "top", options = list(pageLength = 5, scrollY = "200px") )
  })

  output$n_ballotage <- renderText({
    nuance <- input$sel_ballotage
    sprintf( "%s. %d candidat(e)s en ballotage", nuance, nrow(data_ballotage())  )
  })

  selected_ballotage <- eventReactive(input$carte_ballotage_shape_click, {
    click <- strsplit(input$carte_ballotage_shape_click$id, "-")[[1]]
    list( dpt_ = click[1], circ_ = click[2])
  })

  data_ballotage_details <- reactive({
    sel <- selected_ballotage()
    data <- premier_tour %>%
      filter( dpt == sel$dpt_, circ == sel$circ_ ) %>%
      select( candidat, Nuances, Voix, Score, resultat ) %>%
      mutate( Score = round(Score, 2)) %>%
      arrange( desc(Voix) )
  })

  output$data_ballotage_details <- DT::renderDataTable({
    DT::datatable( data_ballotage_details(), options = list(pageLength = 5) )
  })

  output$selected_ballotage_circonscription <- renderText({
    sel <- selected_ballotage()
    dpt <- filter( circos@data, code_dpt == sel$dpt_ ) %>%
      head(1) %$%
      nom_dpt

    data <- premier_tour %>%
      filter( dpt == sel$dpt_, circ == sel$circ_ ) %>%
      head(1)

    sprintf( "%s (%s). %d inscrits, %d exprimés. abstention: %4.2f",
      dpt, sel$circ_, data$Inscrits, data$Exprimes, round(100*data$Abstentions/data$Inscrits, 2) )
  })

})

shinyApp( ui = ui, server = server )
