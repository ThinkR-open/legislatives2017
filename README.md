# legislatives2017
Résultat des elections legislatives 2017

Install 

```
install_github( "romainfrancois/legislatives2017")
```

Pour l'instant il y a le data frame `premier_tour` : 

```
> library("legislatives2017")
> premier_tour
# A tibble: 7,837 x 8
     dpt  circ                    candidat Nuances  Voix p_inscrits p_exprimes  resultat
   <chr> <chr>                       <chr>  <fctr> <dbl>      <dbl>      <dbl>    <fctr>
 1     1     1           M. Laurent MALLET     MDM 13534      16.37      33.89 ballotage
 2     1     1            M. Xavier BRETON      LR 10693      12.93      26.78 ballotage
 3     1     1           M. Jérôme BUISSON      FN  6174       7.47      15.46   elimine
 4     1     1   Mme Fabrine MARTIN ZEMLIK      FI  3874       4.68       9.70   elimine
 5     1     1 Mme Florence BLATRIX-CONTAT     SOC  3687       4.46       9.23   elimine
 6     1     1         M. Jacques FONTAINE     COM   656       0.79       1.64   elimine
 7     1     1        Mme Laurane RAIMONDO     ECO   562       0.68       1.41   elimine
 8     1     1          Mme Maude LÉPAGNOT     EXG   293       0.35       0.73   elimine
 9     1     1           Mme Marie CARLIER     DIV   247       0.30       0.62   elimine
10     1     1           M. Gilbert BONNOT     DIV   211       0.26       0.53   elimine
# ... with 7,827 more rows
```

Les elus du premier tour: 

```
> premier_tour %>% filter( resultat == "elu" )
# A tibble: 4 x 8
    dpt  circ            candidat Nuances  Voix p_inscrits p_exprimes resultat
  <chr> <chr>               <chr>  <fctr> <dbl>      <dbl>      <dbl>   <fctr>
1    56     4       M. Paul MOLAC     REM 30166      28.70      54.00      elu
2    75     1 M. Sylvain MAILLARD     REM 24037      29.76      50.80      elu
3    80     5 M. Stéphane DEMILLY     UDI 21505      26.40      53.85      elu
4   986     1  M. Napole POLUTELE     DVG  3436      40.52      50.24      elu
```

Les candidats FI en ballotage: 

```
> filter( premier_tour, Nuances == "FI", resultat == "ballotage" ) %>% arrange( desc(p_exprimes) ) %>% select(-resultat, -Nuances)
# A tibble: 67 x 6
     dpt  circ              candidat  Voix p_inscrits p_exprimes
   <chr> <chr>                 <chr> <dbl>      <dbl>      <dbl>
 1    93    11 Mme Clémentine AUTAIN  7977      12.73      37.21
 2    13     4 M. Jean-Luc MÉLENCHON  8460      14.17      34.31
 3    93     2       M. Stéphane PEU  4785       8.84      27.69
 4    80     1    M. François RUFFIN  9545      11.32      24.32
 5    34     2 Mme Muriel RESSIGUIER  5886       9.74      22.81
 6    92    11 Mme Yasmine BOUDJENAH  7957      11.49      22.01
 7    93     7    M. Alexis CORBIÈRE  7226       9.50      21.60
 8    31     4    M. Liem HOANG NGOC  6832       9.85      21.02
 9    75    16     Mme Sarah LEGRAIN  7350      10.44      20.84
10     9     1 Mme Bénédicte TAURINE  6125      10.80      20.26
# ... with 57 more rows
```
