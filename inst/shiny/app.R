library(shiny)
library(leaflet)
library(legislatives2017)
library(dplyr)
library(stringr)
library(htmltools)
library(purrr)
library(ggplot2)
library(magrittr)
library(sp)

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

miniplot <- function(Score, Nuances){
  x <- cumsum(c(0, Score ))
  left <- head(x,-1)
  right <- tail(x,-1)

  col <- couleurs[ as.character(Nuances) ]
  col[ is.na(col) ] <- "gray"

  par( mar = rep(0, 4) )
  plot( 0, 0, type = "n", xlim = c(0,100), ylim = c(0,1), axes = F )
  rect(
    xleft = left, xright = right,
    ybottom = 0, ytop = 1, lwd = .5,
    col = col
  )
}


nuances_ballotage <- premier_tour %>% filter( resultat == "ballotage" ) %$% Nuances %>% as.character %>% unique
regions <- unique( premier_tour$nom_reg )
departements <- unique( premier_tour$nom_dpt )

rightPanel <- function(...){
  absolutePanel( class = "panel panel-default panel-side",
    fixed = TRUE, draggable = TRUE,
    top = 60, left = "auto", right = 20, bottom = "auto",
    width = 500, height= "auto",
    ...
  )
}

leftPanel <- function(...){
  absolutePanel(  class = "panel panel-default panel-side",
    fixed = TRUE, draggable = TRUE,
    top = 100, right = "auto", left = 20, bottom = "auto",
    width = 300, height= "auto",
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

  navbarMenu("Premier tour",

    tabPanel_elections("Abstention", "carte_abstention",

      leftPanel(
        sliderInput( "abstention_pourcentage", label = "Pourcentage d'abstention", value = c(0,100), min = 0, max = 100, step = 1, width = "100%" ),

        selectizeInput( "abstention_region", label = "regions", multiple=TRUE, choices = regions, selected=NULL, width = "100%",
          options = list( plugins = list( "remove_button" ), create = FALSE )
        ),

        selectizeInput( "abstention_dpt", label = "departements", multiple=TRUE, choices = departements, selected=NULL, width = "100%",
          options = list( plugins = list( "remove_button" ), create = FALSE )
        )
      ),

      rightPanel(
        h4("Distribution de l'abstention"),
        plotOutput("hist_abstention", height = 200 ),
        h4("Détail"),
        DT::dataTableOutput("data_abstention")
      )
    ),

    tabPanel_elections("Premier", "carte_premier",

      leftPanel(
        sliderInput( "premier_pourcentage_inscrits", label = "Score du candidat en tête", value = c(9, 64), min = 9, max = 64, step = 1, width = "100%" ),

        selectizeInput( "premier_region", label = "regions", multiple=TRUE, choices = regions, selected=NULL, width = "100%",
          options = list( plugins = list( "remove_button" ), create = FALSE )
        ),

        selectizeInput( "premier_dpt", label = "departements", multiple=TRUE, choices = departements, selected=NULL, width = "100%",
          options = list( plugins = list( "remove_button" ), create = FALSE )
        ),

        selectizeInput( "premier_nuance", label = "Nuances (du candidat en tête)", multiple=TRUE, choices = nuances_ballotage, selected=NULL, width = "100%",
          options = list( plugins = list( "remove_button" ), create = FALSE )
        )
      ),
      rightPanel(
        DT::dataTableOutput("data_premier"),

        hr(),

        textOutput("selected_circonscription"),
        br(),
        plotOutput("miniplot_details", height = "20", width = "100%"),
        br(),
        DT::dataTableOutput("data_premier_details")
      )
    ),

    tabPanel_elections("Ballotages", "carte_ballotage",
      rightPanel(
        selectInput("sel_ballotage",
          label = "Nuance",
          choices = nuances_ballotage,
          selected = "FI"
        ),
        textOutput("n_ballotage"),
        tags$hr(),
        tabsetPanel(
          tabPanel("Distribution", plotOutput("hist_ballotage", height = "300px")),
          tabPanel("Detail", DT::dataTableOutput("data_ballotage") )
        ),
        hr(),

        h4(textOutput("selected_ballotage_circonscription")),
        br(),
        DT::dataTableOutput("data_ballotage_details")
      )
    )
    )

)

server <- shinyServer(function(input, output, session){

  data_abstention_all <- premier_tour %>%
    distinct(code_dpt, num_circ, .keep_all = TRUE) %>%
    select(ID, code_dpt, num_circ, nom_dpt, nom_reg, Inscrits:Exprimes, p_abstentions)

  abst_selected_region <- reactive({
    region_ <- input$abstention_region
    if(is.null(region_)) region_ <- regions
    region_
  })

  abst_selected_departement <- reactive({
    dpt_ <- input$abstention_dpt
    if( is.null(dpt_)) dpt_ <- departements
    dpt_
  })

  abst_pourcentage <- reactive(input$abstention_pourcentage)

  data_abstention <- reactive({
      data_abstention_all %>%
        filter(
          nom_reg %in% abst_selected_region(),
          nom_dpt %in% abst_selected_departement(),
          p_abstentions >= abst_pourcentage()[1],
          p_abstentions <= abst_pourcentage()[2]
        )
  })

  output$carte_abstention <- renderLeaflet({
    data <- filter( data_abstention(), ID %in% circos@data$ID )

    abst <- data$p_abstentions / 100
    abst[is.na(abst)] <- 0
    val <- 1 - ( abst - .18 ) / ( .91 - .18 )
    col <- gray(1-val)

    labels <- with( data, sprintf( "%s (circonscription %d) <hr/>%d inscrits<br/>%d abstentions (%4.2f %%)", nom_dpt, num_circ, Inscrits, Abstentions, p_abstentions )) %>%
      map(HTML)

    circos <- circos[ match( data$ID, circos@data$ID ), ]

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
    data <- select(data_abstention(), nom_reg, nom_dpt, num_circ, p_abstentions ) %>%
      arrange( desc(p_abstentions) ) %>%
      mutate( p_abstentions = round(p_abstentions, 2)) %>%
      rename( region = nom_reg, departement = nom_dpt, circ = num_circ ) %>%
      DT::datatable( options = list(pageLength = 20, scrollY = "350px"))
  })

  output$hist_abstention <- renderPlot({
    ggplot( data_abstention() ) +
      aes( x = p_abstentions ) +
      geom_histogram(binwidth = 1) +
      xlab( "% Abstention" ) +
      ylab("# circonscriptions")
  })



  ### tab premier

  data_premier_all <- premier_tour %>%
    filter( resultat %in% c("ballotage", "elu") ) %>%
    mutate( summary = sprintf( "%s (%s) :: %4.2f %%", candidat, Nuances, round(100 * Voix / Exprimes, 2 ) )  ) %>%
    group_by(code_dpt,num_circ) %>%
    summarise(
      ID   = first(ID),
      nom_dpt = first(nom_dpt),
      nom_reg = first(nom_reg),
      Score  =  max(Score),
      Nuances = Nuances[ Voix == max(Voix)][1],
      candidat = candidat[Voix == max(Voix)][1],
      summary = paste( nom_dpt, "(", num_circ, ")<hr/>", paste( summary, collapse = "<br/>"), sep = "" )[1]
    )

  selected_region <- reactive({
    region_ <- input$premier_region
    if(is.null(region_)) region_ <- regions
    region_
  })

  selected_departement <- reactive({
    dpt_ <- input$premier_dpt
    if( is.null(dpt_)) dpt_ <- departements
    dpt_
  })

  selected_nuance <- reactive({
    nuance_ <- input$premier_nuance
    if(is.null(nuance_)) nuance_ <- nuances_ballotage
    nuance_
  })

  observeEvent( input$premier_region, {

      all_dpt <- filter(data_premier_all, nom_reg %in% selected_region() ) %>% distinct(nom_dpt) %$% nom_dpt
      sel_dpt <- intersect(all_dpt, input$premier_dpt )

      updateSelectizeInput( "premier_dpt", session = session,
        choices = all_dpt, selected = sel_dpt
      )
  })

  data_premier <- reactive({

      data_premier_all %>%
        filter(
          Score >= input$premier_pourcentage_inscrits[1],
          Score <= input$premier_pourcentage_inscrits[2],
          nom_dpt %in% selected_departement() ,
          nom_reg %in% selected_region() ,
          Nuances %in% selected_nuance()
         )
  })

  output$data_premier <- DT::renderDataTable({
    data <- data_premier() %>%
      select( nom_reg, nom_dpt, num_circ,candidat, Nuances, Score) %>%
      mutate( Score = round(Score, 2)) %>%
      rename( region = nom_reg, departement = nom_dpt, circ = num_circ ) %>%
      DT::datatable( options = list(pageLength = 5, scrollY = "200px", searching = FALSE), selection = "single" )
  })

  output$carte_premier <- renderLeaflet({
    data <- filter( data_premier(), ID %in% circos@data$ID )
    col  <- unname(couleurs[ as.character(data$Nuances) ])
    labels <- data$summary %>% map(HTML)

    circos <- circos[ match( data$ID, circos@data$ID )  , , drop = FALSE]

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
        layerId = paste(data$code_dpt, "-", data$num_circ, sep = "")
      )
  })


  premier_selected <- reactiveValues( data = list( dpt_ = "34", circ_ = 2 ) )
  selected <- reactive(premier_selected$data)

  observeEvent( input$carte_premier_shape_click, {
    click <- strsplit(input$carte_premier_shape_click$id, "-")[[1]]
    premier_selected$data <- list( dpt_ = click[1], circ_ = click[2] )
  })

  observeEvent( input$data_premier_rows_selected, {
      data <- slice( data_premier(), input$data_premier_rows_selected )
      premier_selected$data <- list( dpt_ = data$code_dpt, circ_ = data$num_circ )
  })

  data_premier_details <- reactive({
    sel <- selected()
    data <- premier_tour %>%
      filter( code_dpt == sel$dpt_, num_circ == sel$circ_ ) %>%
      select( candidat, Nuances, Voix, Score, resultat ) %>%
      mutate( Score = round(Score, 2)) %>%
      arrange( desc(Voix) )

    data

  })

  output$miniplot_details <- renderPlot({
    with( data_premier_details(), miniplot( Score, Nuances ) )
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
      filter( code_dpt == sel$dpt_, num_circ == sel$circ_ ) %>%
      head(1)

    sprintf( "%s (%s). %d inscrits, %d exprimés. abstention: %4.2f",
      dpt, sel$circ_, data$Inscrits, data$Exprimes, round(100*data$Abstentions/data$Inscrits, 2) )
  })

  data_ballotage <- reactive({
    nuance <- input$sel_ballotage

    filter( premier_tour, Nuances == nuance, resultat == "ballotage" ) %>%
      mutate( Score = round(Score, 2)) %>%
      arrange( desc(Score) )

  })


  output$carte_ballotage <- renderLeaflet({
    nuance <- input$sel_ballotage
    data <- data_ballotage()

    col  <- unname(couleurs[nuance])

    labels <- with(data, sprintf("%s (%4.2f)", candidat, Score )) %>% map(HTML)

    circos <- circos[ na.omit(match( data$ID, circos@data$ID)), , drop = FALSE ]

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
      select( candidat, nom_reg, nom_dpt, circ, Score ) %>%
      rename( region = nom_reg, departement = nom_dpt ) %>%
      DT::datatable( options = list(pageLength = 5, scrollY = "200px") )
  })

  output$hist_ballotage <- renderPlot({
    col <- couleurs[input$sel_ballotage]
    ggplot( data_ballotage() ) +
      aes( x = Score ) +
      geom_histogram(binwidth = 1, fill = col ) +
      xlab( "Score au premier tour" ) +
      ylab( "# circonscriptions")
  })

  output$n_ballotage <- renderText({
    sprintf( "%d candidat(e)s", nrow(data_ballotage())  )
  })

  selected_ballotage <- eventReactive(input$carte_ballotage_shape_click, {
    click <- strsplit(input$carte_ballotage_shape_click$id, "-")[[1]]
    list( dpt_ = click[1], circ_ = click[2])
  })

  data_ballotage_details <- reactive({
    sel <- selected_ballotage()
    data <- premier_tour %>%
      filter( code_dpt == sel$dpt_, num_circ == sel$circ_ ) %>%
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
      filter( code_dpt == sel$dpt_, num_circ == sel$circ_ ) %>%
      head(1)

    sprintf( "%s (%s). %d inscrits, %d exprimés. abstention: %4.2f",
      dpt, sel$circ_, data$Inscrits, data$Exprimes, round(100*data$Abstentions/data$Inscrits, 2) )
  })

})

shinyApp( ui = ui, server = server )
