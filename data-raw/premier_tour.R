library(rvest)
library(stringr)
library(purrr)
library(tibble)
library(dplyr)
library(forcats)
library(tidyr)

get_circonscriptions <- function(dpt, pb){
  setTxtProgressBar( pb, getTxtProgressBar(pb) + 1)
  url <- sprintf( "http://elections.interieur.gouv.fr/legislatives-2017/%s/index.html", dpt )
  circ <- read_html(url) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_subset( "^.*/C2[0-9]{2}[0-9AB][0-9]{2}[.]html$" ) %>%
    str_replace( ".*/.*([0-9]{2})[.]html$", "\\1" )
  data_frame( dpt = dpt, circ = circ )
}

get_mentions <- function(dpt, circ, pb){
  setTxtProgressBar( pb, getTxtProgressBar(pb) + 1)
  url <- sprintf("http://elections.interieur.gouv.fr/legislatives-2017/%s/C2%s%s.html", dpt, dpt, circ )

  data <- read_html(url) %>%
    html_node(".tableau-mentions") %>%
    html_table()

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

get_resultats <- function(dpt, circ, pb){
  setTxtProgressBar( pb, getTxtProgressBar(pb) + 1)

  url <- sprintf("http://elections.interieur.gouv.fr/legislatives-2017/%s/C2%s%s.html", dpt, dpt, circ )
  html <- read_html(url)
  # candidat_2nd_tour <- html %>%
  #   html_nodes("table") %>%
  #   extract2(2) %>%
  #   html_table %>%
  #   mutate( Candidat = str_replace(Candidat, "^[0-9] [-] ", "") ) %$%
  #   Candidat

  res <- html %>%
    html_node(".tableau-resultats-listes-ER") %>%
    html_table() %>%
    rename(
      p_inscrits = `% Inscrits`,
      p_exprimes = `% Exprimés`,
      candidat = `Liste des candidats`
    ) %>%
    mutate(
      Voix = as.numeric(str_replace_all(Voix, " ", "")),
      p_inscrits = as.numeric(str_replace_all(p_inscrits, ",", ".")),
      p_exprimes = as.numeric(str_replace_all(p_exprimes, ",", ".")),
      resultat = ifelse(p_inscrits > 50, "elu", ifelse(p_inscrits>12.5, "ballotage", "elimine") )
    )


  res
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

departements <- read_html("http://elections.interieur.gouv.fr/legislatives-2017/") %>%
  html_nodes( "option" ) %>%
  html_attr("value") %>%
  str_subset( "index[.]html$" ) %>%
  str_replace( "/.*$", "")

message("circonscriptions")
pb <- txtProgressBar(min = 0, max = length(departements), style = 3)
data <- departements %>%
  map(get_circonscriptions, pb = pb) %>%
  bind_rows()

message("participation")
pb <- txtProgressBar(min = 0, max = nrow(data), style = 3)
data <- data %>%
  mutate( res = map2(dpt, circ, get_mentions, pb = pb) ) %>%
  unnest()

message("resultats")
pb <- txtProgressBar(min = 0, max = nrow(data), style = 3)
data <- data %>%
  mutate( res = map2(dpt, circ, get_resultats, pb = pb) ) %>%
  unnest() %>%
  mutate(
    Nuances = as_factor(Nuances),
    dpt = str_replace( dpt, "^0+", "" ),
    circ = str_replace( circ, "^\\d{3}0*", "" )
  )

premier_tour <- data %>%
  mutate(
    dpt = fix_dpt(dpt),
    circ = fix_circ(circ),
    Score = 100 * Voix / Exprimes,
    civilite = ifelse(grepl("^M[.]", candidat), "M.","Mme"),
    candidat = str_replace_all(candidat, "^Mm?e?[.]? ", ""),
    p_abstentions = 100*Abstentions / Inscrits
  ) %>%
  select( dpt:Exprimes, civilite, candidat, Nuances, Voix, p_inscrits, p_exprimes, p_abstentions, Score, resultat )

devtools::use_data( premier_tour, overwrite = TRUE )
