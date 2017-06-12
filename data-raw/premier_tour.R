library(rvest)
library(stringr)
library(purrr)
library(tibble)
library(dplyr)
library(forcats)

departements <- read_html("http://elections.interieur.gouv.fr/legislatives-2017/") %>%
  html_nodes( "option" ) %>%
  html_attr("value") %>%
  str_subset( "^\\d+/" ) %>%
  str_replace( "/.*$", "")

get_circonscriptions <- function(dpt){
  url <- sprintf( "http://elections.interieur.gouv.fr/legislatives-2017/%s/index.html", dpt )
  circ <- read_html(url) %>%
    html_nodes(".pub-index-communes") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_subset( "^.*/\\d{5}[.]html$" ) %>%
    str_replace( ".*/(.*)[.]html$", "\\1" )

  data_frame( dpt = dpt, circ = circ )
}

get_resultats <- function(dpt, circ, pb){
  setTxtProgressBar( pb, getTxtProgressBar(pb) + 1)
  url <- sprintf("http://elections.interieur.gouv.fr/legislatives-2017/%s/%s.html", dpt, circ )
  read_html(url) %>%
    html_node(".tableau-resultats-listes-ER") %>%
    html_table() %>%
    rename(
      p_inscrits = `% Inscrits`,
      p_exprimes = `% Exprimés`,
      resultat   = `Elu(e)`,
      candidat = `Liste des candidats`
    ) %>%
    mutate(
      Voix = as.numeric(str_replace_all(Voix, " ", "")),
      p_inscrits = as.numeric(str_replace_all(p_inscrits, ",", ".")),
      p_exprimes = as.numeric(str_replace_all(p_exprimes, ",", ".")),
      resultat   = str_replace(resultat, "[*]", "" )
    )
}

data <- departements %>%
  map(get_circonscriptions) %>%
  bind_rows()

pb <- txtProgressBar(min = 0, max = nrow(data), style = 3)
data <- data %>%
  mutate( res = map2(dpt, circ, get_resultats, pb = pb) ) %>%
  unnest() %>%
  mutate(
    Nuances = as_factor(Nuances),
    dpt = str_replace( dpt, "^0+", "" ),
    circ = str_replace( circ, "^\\d{3}0*", "" ),
    resultat = fct_recode( as_factor(resultat), elu = "Oui", ballotage = "Ballotage", elimine = "Non" )
  )

premier_tour <- data
use_data( premier_tour, overwrite = TRUE )

