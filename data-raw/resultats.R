library(rvest)
library(stringr)
library(purrr)
library(tibble)
library(dplyr)
library(forcats)
library(tidyr)


fix_dpt <- function(dpt){
  dpt[ dpt == "971" ] <- "ZA"
  dpt[ dpt == "972" ] <- "ZB"
  dpt[ dpt == "973" ] <- "ZC"
  dpt[ dpt == "974" ] <- "ZD"
  dpt[ dpt == "976" ] <- "ZM"
  dpt[ dpt == "988" ] <- "ZN"
  dpt[ dpt == "987" ] <- "ZP"
  dpt[ dpt == "975" ] <- "ZS"
  dpt[ dpt == "986" ] <- "ZW"
  dpt[ dpt == "977" ] <- "ZX"

  dpt <- str_replace( dpt, "^0", "")

  dpt
}

fix_circ <- function(circ){
  circ %>%
    str_replace( "^.*[AB]", "" ) %>%
    as.numeric()
}

.get_resultats <- function( table ){
  table %>%
    html_table() %>%
    rename(
      p_inscrits = `% Inscrits`,
      p_exprimes = `% Exprimés`,
      candidat = `Liste des candidats`,
      resultat = `Elu(e)`
    ) %>%
    mutate(
      Voix = as.numeric(str_replace_all(Voix, " ", "")),
      p_inscrits = as.numeric(str_replace_all(p_inscrits, ",", ".")),
      p_exprimes = as.numeric(str_replace_all(p_exprimes, ",", ".")),
      resultat = ifelse(resultat=="Oui", "elu", ifelse( resultat == "Ballotage*", "ballotage","elimine") )
    )
}

get_circonscriptions <- function(dpt, pb = NULL){
  if(!is.null(pb)) setTxtProgressBar( pb, getTxtProgressBar(pb) + 1)

  url <- sprintf( "http://elections.interieur.gouv.fr/legislatives-2017/%s/index.html", dpt )
  circ <- read_html(url) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_subset( "^.*/[0-9]{2}[0-9AB][0-9]{2}[.]html$" ) %>%
    str_replace( ".*/.*([0-9]{2})[.]html$", "\\1" )

  url <- sprintf("http://elections.interieur.gouv.fr/legislatives-2017/%s/%s%s.html", dpt, dpt, circ )

  data_frame( dpt = dpt, circ = circ, page = map( url, read_html ) )
}

get_resultats_tour_1 <- function( page, pb = NULL){
  if(!is.null(pb)) setTxtProgressBar( pb, getTxtProgressBar(pb) + 1)

  page %>%
    html_nodes(".tableau-resultats-listes-ER") %>%
    last() %>%
    .get_resultats()
}

get_resultats_tour_2 <- function( page, pb = NULL){
  if(!is.null(pb)) setTxtProgressBar( pb, getTxtProgressBar(pb) + 1)

  tables <- html_nodes(page, ".tableau-resultats-listes-ER")

  if( length(tables) == 2){
    .get_resultats(tables[[1]])
  }
}

.get_mentions <- function(table){
  data <- html_table( table )

  Nombre <- as.numeric(str_replace( data$Nombre, " ", "" ))
  data_frame(
    Inscrits = Nombre[1],
    Abstentions = Nombre[2],
    Votants = Nombre[3],
    Blancs = Nombre[4],
    Nuls = Nombre[5],
    Exprimes = Nombre[6]
  )
}

get_mentions_tour_1 <- function(page, pb = NULL){
  if( !is.null(pb)) setTxtProgressBar( pb, getTxtProgressBar(pb) + 1)

  html_nodes(page, ".tableau-mentions") %>%
    last() %>%
    .get_mentions()

}

get_mentions_tour_2 <- function(page, pb = NULL){
  if( !is.null(pb)) setTxtProgressBar( pb, getTxtProgressBar(pb) + 1)

  tables <- html_nodes(page, ".tableau-mentions")

  if( length(tables) == 2){
    .get_mentions(tables[[1]])
  }

}

get_resultats_tour <- function( departements, circonscriptions, resultats_fun, mentions_fun){

  pb <- txtProgressBar(min = 0, max = 2*nrow(circonscriptions), style = 3)
  resultats_tour <- map( circonscriptions$page, resultats_fun, pb = pb )
  mentions_tour <- bind_rows( map( circonscriptions$page, mentions_fun, pb = pb ) )

  circonscriptions %>%
    rename( code_dpt = dpt, num_circ = circ ) %>%
    select(-page) %>%
    mutate( code_dpt = fix_dpt(code_dpt) ) %>%
    left_join( regions, by = "code_dpt") %>%
    mutate(
      num_circ = as.numeric(num_circ),
      resultats = resultats_tour
    ) %>%
    filter( map_lgl(resultats,~!is.null(.) ) ) %>%
    bind_cols( mentions_tour ) %>%
    unnest() %>%
    mutate(
      civilite = ifelse(grepl("^M[.]", candidat), "M.","Mme"),
      candidat = str_replace_all(candidat, "^Mm?e?[.]? ", ""),
      Score = 100 * Voix /  Exprimes,
      p_abstentions = 100 * Abstentions / Inscrits,
      ID = sprintf("%s%03d", code_dpt, num_circ)
    )
}

departements <- read_html("http://elections.interieur.gouv.fr/legislatives-2017/") %>%
  html_nodes( "option" ) %>%
  html_attr("value") %>%
  str_subset( "index[.]html$" ) %>%
  str_replace( "/.*$", "")

message("circonscriptions")
pb <- txtProgressBar(min = 0, max = length(departements), style = 3)
circonscriptions <- departements %>%
  map(get_circonscriptions, pb = pb) %>%
  bind_rows() %>%
  distinct(dpt, circ, .keep_all = TRUE) %>%
  bind_rows( data_frame(
    dpt = "986", circ = "01",
    page = list( read_html("http://elections.interieur.gouv.fr/legislatives-2017/986/98601.html") )
  ))

## resultats 1er tour
message("resultats tour 1")
premier_tour <- get_resultats_tour( departements, circonscriptions, get_resultats_tour_1, get_mentions_tour_1 )


## resultats 2nd tour

message("resultats tour 2")
second_tour <- get_resultats_tour( departements, circonscriptions, get_resultats_tour_2, get_mentions_tour_2 )


use_data( premier_tour, overwrite = TRUE )
use_data( second_tour, overwrite = TRUE )

assemblee <- bind_rows(
  filter( premier_tour, resultat == "elu") %>% mutate( tour = 1),
  filter( second_tour, resultat == "elu") %>% mutate( tour = 2)
)
use_data( assemblee, overwrite = TRUE)

