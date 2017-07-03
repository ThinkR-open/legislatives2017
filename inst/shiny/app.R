library(shiny)
library(leaflet)
library(legislatives2017)
library(dplyr)
library(stringr)
library(htmltools)
library(purrr)
library(ggplot2)
library(magrittr)
library(sf)

couleurs <- c(
  REM  = "orange",
  MDM = "darkorange2",

  LR = "blue",
  FN = "black",

  FI = "red",
  SOC = "pink",
  UDI = "purple",
  DVD = "lightblue",
  DVG = "pink2",
  RDG = "pink3",
  EXD = "black",
  DLF = "darkblue",

  ECO = "green",

  COM = "darkred",

  DIV = "gray",
  REG = "gray2"

)
html_couleurs <- apply( col2rgb( couleurs ), 2, function(.) paste0( "rgb(", paste(., collapse = ", "), ")" ) )


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
  par( mar = rep(0, 4) )
  plot( 0, 0, type = "n", xlim = c(0,100), ylim = c(0,1), axes = F )

  if( length(Score) ){
    x <- cumsum(c(0, Score ))
    left <- head(x,-1)
    right <- tail(x,-1)

    col <- couleurs[ as.character(Nuances) ]
    col[ is.na(col) ] <- "gray"

    rect(
      xleft = left, xright = right,
      ybottom = 0, ytop = 1, lwd = .5,
      col = col
    )
  }

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

electionsLeaflet <- function(data){
  data %>%
    right_join( circonscriptions, ., by = "ID") %>%
    leaflet() %>%
    addTiles( urlTemplate = 'http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png' ) %>%
    setView(lng = 5, lat= 47, zoom=7) %>%
    addPolygons( color = "black", weight = .5, fillColor = ~col, fill = TRUE, fillOpacity = .7, label = ~label,
      highlightOptions = highlightOptions(weight = 2, fillOpacity = .7, bringToFront = TRUE),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      ),
      layerId = ~ID
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
  ),

  navbarMenu("Second tour",

    tabPanel_elections("Résultats", "carte_second",

      leftPanel(
        sliderInput( "second_pourcentage_inscrits", label = "Score du député élu", value = c(36, 100), min = 36, max = 100, step = 1, width = "100%" ),

        selectizeInput( "second_region", label = "regions", multiple=TRUE, choices = regions, selected=NULL, width = "100%",
          options = list( plugins = list( "remove_button" ), create = FALSE )
        ),

        selectizeInput( "second_dpt", label = "departements", multiple=TRUE, choices = departements, selected=NULL, width = "100%",
          options = list( plugins = list( "remove_button" ), create = FALSE )
        ),

        selectizeInput( "second_nuance", label = "Nuances", multiple=TRUE, choices = nuances_ballotage, selected=NULL, width = "100%",
          options = list( plugins = list( "remove_button" ), create = FALSE )
        )
      ),
      rightPanel(
        DT::dataTableOutput("data_second"),

        hr(),

        textOutput("second_selected_circonscription"),
        br(),
        plotOutput("second_miniplot_details", height = "20", width = "100%"),
        br(),
        DT::dataTableOutput("data_second_details")
      )
    )
  ),

  navbarMenu("Assemblee",

    tabPanel_elections("Résultats", "carte_assemblee",

      leftPanel(
        sliderInput( "assemblee_pourcentage_inscrits", label = "Score du député élu", value = c(36, 100), min = 36, max = 100, step = 1, width = "100%" ),

        selectizeInput( "assemblee_region", label = "regions", multiple=TRUE, choices = regions, selected=NULL, width = "100%",
          options = list( plugins = list( "remove_button" ), create = FALSE )
        ),

        selectizeInput( "assemblee_dpt", label = "departements", multiple=TRUE, choices = departements, selected=NULL, width = "100%",
          options = list( plugins = list( "remove_button" ), create = FALSE )
        ),

        selectizeInput( "assemblee_nuance", label = "Nuances", multiple=TRUE, choices = nuances_ballotage, selected=NULL, width = "100%",
          options = list( plugins = list( "remove_button" ), create = FALSE )
        )
      ),
      rightPanel(
        DT::dataTableOutput("data_assemblee"),

        hr(),

        textOutput("assemblee_selected_circonscription"),
        br(),
        "Premier tour",
        plotOutput("assemblee_miniplot_details_tour1", height = "20", width = "100%"),
        "Second tour",
        plotOutput("assemblee_miniplot_details_tour2", height = "20", width = "100%"),
        br(),
        tabsetPanel( selected = "Second tour",
          tabPanel( "Premier tour", DT::dataTableOutput("data_assemblee_details_tour1") ),
          tabPanel( "Second tour", DT::dataTableOutput("data_assemblee_details_tour2") )
        )

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
        ) %>%
        mutate(
          col = gray( 1 - ( p_abstentions/100-.18 ) / (.91-.18) ),
          label = map( sprintf( "%s (circonscription %d) <hr/>%d inscrits<br/>%d abstentions (%4.2f %%)", nom_dpt, num_circ, Inscrits, Abstentions, p_abstentions ), HTML )
        )
  })

  output$carte_abstention <- renderLeaflet( electionsLeaflet( data_abstention() ) )

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
        ) %>%
        mutate(
          col = unname(html_couleurs[ as.character(Nuances) ]),
          label = map(summary, HTML)
        )
  })

  output$data_premier <- DT::renderDataTable({
    data <- data_premier() %>%
      select( nom_reg, nom_dpt, num_circ,candidat, Nuances, Score) %>%
      mutate( Score = round(Score, 2)) %>%
      rename( region = nom_reg, departement = nom_dpt, circ = num_circ ) %>%
      DT::datatable( options = list(pageLength = 5, scrollY = "200px", searching = FALSE), selection = "single" )
  })

  output$carte_premier <- renderLeaflet( electionsLeaflet(data_premier()) )

  premier_selected <- reactiveValues( data = "34002" )
  selected <- reactive(premier_selected$data)

  observeEvent( input$carte_premier_shape_click, {
    premier_selected$data <- input$carte_premier_shape_click$id
  })

  observeEvent( input$data_premier_rows_selected, {
      data <- slice( data_premier(), input$data_premier_rows_selected )
      premier_selected$data <- sprintf( "%s%03d", data$code_dpt,data$num_circ )
  })

  data_premier_details <- reactive({
    premier_tour %>%
      filter( ID == selected() ) %>%
      select( candidat, Nuances, Voix, Score, resultat ) %>%
      mutate( Score = round(Score, 2)) %>%
      arrange( desc(Voix) )
  })

  output$miniplot_details <- renderPlot({
    with( data_premier_details(), miniplot( Score, Nuances ) )
  })

  output$data_premier_details <- DT::renderDataTable({
    DT::datatable( data_premier_details(), options = list(pageLength = 5) )
  })

  output$selected_circonscription <- renderText({
    premier_tour %>%
      filter( ID == selected() ) %>%
      head(1) %>%
      mutate(
        text =  sprintf( "%s (%s). %d inscrits, %d exprimés. abstention: %4.2f",
          nom_dpt, num_circ, Inscrits, Exprimes, p_abstentions )
      ) %>%
      pull()
  })


  ## tab ballotage

  data_ballotage <- reactive({
    nuance <- input$sel_ballotage

    filter( premier_tour, Nuances == nuance, resultat == "ballotage" ) %>%
      mutate(
        Score = round(Score, 2),
        col = html_couleurs[nuance],
        label = map( sprintf("%s (%4.2f)", candidat, Score ), HTML )
      ) %>%
      arrange( desc(Score) )


  })


  output$carte_ballotage <- renderLeaflet( electionsLeaflet(data_ballotage()) )

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
    input$carte_ballotage_shape_click$id
  })

  data_ballotage_details <- reactive({
    sel <- selected_ballotage()
    data <- premier_tour %>%
      filter( ID == sel ) %>%
      select( candidat, Nuances, Voix, Score, resultat ) %>%
      mutate( Score = round(Score, 2)) %>%
      arrange( desc(Voix) )
  })

  output$data_ballotage_details <- DT::renderDataTable({
    DT::datatable( data_ballotage_details(), options = list(pageLength = 5) )
  })

  output$selected_ballotage_circonscription <- renderText({
    sel <- selected_ballotage()

    data <- premier_tour %>%
      filter( ID == sel) %>%
      head(1) %>%
      mutate( text = sprintf( "%s (%s). %d inscrits, %d exprimés. abstention: %4.2f",
        nom_dpt, num_circ, Inscrits, Exprimes, p_abstentions )
      ) %>%
      pull()

  })



  # 2nd tour

  ### tab premier

  data_second_all <- second_tour %>%
    filter( resultat == "elu" ) %>%
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

  second_selected_region <- reactive({
    region_ <- input$second_region
    if(is.null(region_)) region_ <- regions
    region_
  })

  second_selected_departement <- reactive({
    dpt_ <- input$second_dpt
    if( is.null(dpt_)) dpt_ <- departements
    dpt_
  })

  second_selected_nuance <- reactive({
    nuance_ <- input$second_nuance
    if(is.null(nuance_)) nuance_ <- nuances_ballotage
    nuance_
  })

  observeEvent( input$second_region, {

    all_dpt <- filter(data_second_all, nom_reg %in% second_selected_region() ) %>% distinct(nom_dpt) %$% nom_dpt
    sel_dpt <- intersect(all_dpt, input$second_dpt )

    updateSelectizeInput( "second_dpt", session = session,
      choices = all_dpt, selected = sel_dpt
    )
  })

  data_second <- reactive({

    data_second_all %>%
      filter(
        Score >= input$second_pourcentage_inscrits[1],
        Score <= input$second_pourcentage_inscrits[2],
        nom_dpt %in% second_selected_departement() ,
        nom_reg %in% second_selected_region() ,
        Nuances %in% second_selected_nuance()
      ) %>%
      mutate(
        col = unname(html_couleurs[ as.character(Nuances) ]),
        label = map( summary, HTML)
      )
  })

  output$data_second <- DT::renderDataTable({
    data <- data_second() %>%
      select( nom_reg, nom_dpt, num_circ,candidat, Nuances, Score) %>%
      mutate( Score = round(Score, 2)) %>%
      rename( region = nom_reg, departement = nom_dpt, circ = num_circ ) %>%
      DT::datatable( options = list(pageLength = 5, scrollY = "200px", searching = FALSE), selection = "single" )
  })

  output$carte_second <- renderLeaflet( electionsLeaflet(data_second()) )

    second_selected <- reactiveValues( data = "34002" )
  selected2 <- reactive(second_selected$data)

  observeEvent( input$carte_second_shape_click, {
    second_selected$data <- input$carte_second_shape_click$id
  })

  observeEvent( input$data_second_rows_selected, {
    data <- slice( data_second(), input$data_second_rows_selected )
    second_selected$data <- sprintf( "%s%03d", data$code_dpt, data$num_circ )
  })

  data_second_details <- reactive({
    sel <- selected2()
    data <- second_tour %>%
      filter( ID == sel ) %>%
      select( candidat, Nuances, Voix, Score, resultat ) %>%
      mutate( Score = round(Score, 2)) %>%
      arrange( desc(Voix) )

    data

  })

  output$second_miniplot_details <- renderPlot({
    with( data_second_details(), miniplot( Score, Nuances ) )
  })

  output$data_second_details <- DT::renderDataTable({
    DT::datatable( data_second_details(), options = list(pageLength = 5) )
  })

  output$second_selected_circonscription <- renderText({
    second_tour %>%
      filter( ID == selected2() ) %>%
      head(1) %>%
      mutate(
        text =  sprintf( "%s (%s). %d inscrits, %d exprimés. abstention: %4.2f",
          nom_dpt, num_circ, Inscrits, Exprimes, p_abstentions )
      ) %>%
      pull()
  })


  # Assemblee

  data_assemblee_all <- assemblee %>%
    filter( resultat == "elu" ) %>%
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

  assemblee_selected_region <- reactive({
    region_ <- input$assemblee_region
    if(is.null(region_)) region_ <- regions
    region_
  })

  assemblee_selected_departement <- reactive({
    dpt_ <- input$assemblee_dpt
    if( is.null(dpt_)) dpt_ <- departements
    dpt_
  })

  assemblee_selected_nuance <- reactive({
    nuance_ <- input$assemblee_nuance
    if(is.null(nuance_)) nuance_ <- nuances_ballotage
    nuance_
  })

  observeEvent( input$assemblee_region, {

    all_dpt <- filter(data_assemblee_all, nom_reg %in% assemblee_selected_region() ) %>% distinct(nom_dpt) %$% nom_dpt
    sel_dpt <- intersect(all_dpt, input$assemblee_dpt )

    updateSelectizeInput( "assemblee_dpt", session = session,
      choices = all_dpt, selected = sel_dpt
    )
  })

  data_assemblee <- reactive({

    data_assemblee_all %>%
      filter(
        Score >= input$assemblee_pourcentage_inscrits[1],
        Score <= input$assemblee_pourcentage_inscrits[2],
        nom_dpt %in% assemblee_selected_departement() ,
        nom_reg %in% assemblee_selected_region() ,
        Nuances %in% assemblee_selected_nuance()
      ) %>%
      mutate(
        col = unname(html_couleurs[ as.character(Nuances) ]),
        label = map( summary, HTML)
      )
  })

  output$data_assemblee <- DT::renderDataTable({
    data <- data_assemblee() %>%
      select( nom_reg, nom_dpt, num_circ,candidat, Nuances, Score) %>%
      mutate( Score = round(Score, 2)) %>%
      rename( region = nom_reg, departement = nom_dpt, circ = num_circ ) %>%
      DT::datatable( options = list(pageLength = 5, scrollY = "200px", searching = FALSE), selection = "single" )
  })

  output$carte_assemblee <- renderLeaflet(electionsLeaflet( data_assemblee() ))

  assemblee_selected <- reactiveValues( data = "34002" )
  selected3 <- reactive(assemblee_selected$data)

  observeEvent( input$carte_assemblee_shape_click, {
    assemblee_selected$data <- input$carte_assemblee_shape_click$id
  })

  observeEvent( input$data_assemblee_rows_selected, {
    data <- slice( data_assemblee(), input$data_assemblee_rows_selected )
    assemblee_selected$data <- sprintf( "%s%03d", data$code_dpt, data$num_circ )
  })

  data_assemblee_details_tour1 <- reactive({
    premier_tour %>%
      filter( ID == selected3()  ) %>%
      select( candidat, Nuances, Voix, Score, resultat ) %>%
      mutate( Score = round(Score, 2)) %>%
      arrange( desc(Voix) )
  })

  data_assemblee_details_tour2 <- reactive({
    second_tour %>%
      filter( ID == selected3() ) %>%
      select( candidat, Nuances, Voix, Score, resultat ) %>%
      mutate( Score = round(Score, 2)) %>%
      arrange( desc(Voix) )
  })

  output$assemblee_miniplot_details_tour1 <- renderPlot({
    with( data_assemblee_details_tour1(), miniplot( Score, Nuances ) )
  })

  output$assemblee_miniplot_details_tour2 <- renderPlot({
    with( data_assemblee_details_tour2(), miniplot( Score, Nuances ) )
  })

  output$data_assemblee_details_tour1 <- DT::renderDataTable({
    DT::datatable( data_assemblee_details_tour1(), options = list(pageLength = 5) )
  })

  output$data_assemblee_details_tour2 <- DT::renderDataTable({
    DT::datatable( data_assemblee_details_tour2(), options = list(pageLength = 5) )
  })

  output$assemblee_selected_circonscription <- renderText({
    assemblee %>%
      filter( ID == selected3() ) %>%
      head(1) %>%
      mutate(
        text = sprintf( "%s (%s). %d inscrits, %d exprimés. abstention: %4.2f",
          nom_dpt, num_circ, Inscrits, Exprimes, p_abstentions )
      ) %>%
      pull()
  })


})

shinyApp( ui = ui, server = server )
