library(rvest)
library(stringr)
library(purrr)
library(tibble)
library(dplyr)
library(forcats)
library(tidyr)

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

  dpt[ grepl("^\\d$", dpt) ] <- sprintf("%02d", as.numeric(dpt[ grepl("^\\d$", dpt ) ]))

  dpt
}

fix_circ <- function(circ){
  circ %>%
    str_replace( "^.*[AB]", "" ) %>%
    as.numeric()
}

get_resultats_tour_2 <- function( page, pb = NULL){
  if(!is.null(pb)) setTxtProgressBar( pb, getTxtProgressBar(pb) + 1)

  tables <- page %>%
    html_nodes(".tableau-resultats-listes-ER")

  if( length(tables) == 1) return(NULL)

  res <- tables[[1]] %>%
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
      resultat = ifelse(resultat=="Oui", "elu", "elimine")
    )

  res
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
  bind_rows()

message("resultats tour 2")
pb <- txtProgressBar(min = 0, max = nrow(circonscriptions), style = 3)
resultats_tour_2 <- circonscriptions$page %>%
  map( get_resultats_tour_2 )

