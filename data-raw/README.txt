## Nom de l'archive : CIRCOS
## Description : Fond de carte vectoriel des circonscription législatives de France métropolitaine, hors Corse et DOM (redécoupage 2010)
## Version : 3 - septembre 2013
## Auteurs : Toxicode, Adrien di Pasquale et Joël Gombin, Jonathan Chibois
## Licence : ODBL (attribution and share-alike)

##### À SAVOIR ###

Les circonscriptions sont identifées par un numéro unique : l'IDEN .
Celui ci se compose au format DDCC, où DD est le numéro de département et CC celui de circonscriptions. 
Exemple : le numéro 7501 correspond à la première circonscription de Paris.

Les surfaces indiquées ont été calculée à partir des polygones définis, et n'ont été que sommairement vérifiées. Elles doivent par conséquent être lues comme ordre de grandeur réaliste, et non comme données topographiques mesurées. Pour en savoir plus sur la nature desapproximations, voir les notes de précision de découpage de Toxicode (http://www.toxicode.fr/circonscriptions).

##### CONTENU DE L'ARCHIVE #####

* CIRCOS_V3.SHP + CIRCOS_0.2.SHX + CIRCOS_0.2.DBF : attention, la coprésence des trois fichiers est nécessaire l'utilisation du fond de carte.

* CIRCOS_V3.RDATA : source de données pour R, prête à l'emploi. 
	La variable 'fdc' contient l'assemblage des polygones consitutant les circonscriptions.
	* « summary(fdc) » : informations sur la table de données (identique à circos_v3_attributs.csv)
	* « plot(fdc) : génère le fond de carte.

* CIRCOS_V3_ATTRIBUTS.CSV : table des attributs incluses dans le fond de carte, exportées pour facilité d'utilisation. Surfaces en m².


##### CHANGELOG #####

V1 par Toxicode 
URL : http://www.toxicode.fr/circonscriptions
	* Contours des circonscriptions au format CSV construites à partir des contours de communes fournis par l'IGN/GEOFLA et des correspondances entre circonscriptions et communes, fournies par le Ministère de l'intérieur.

V2 par Adrien di Pasquale et Joël Gombin (juin 2012)
URL : http://www.joelgombin.fr/un-fonds-de-carte-vectoriel-pour-les-circonscriptions-legislatives/
	* Conversion au format SHP, KML et SVG

V3 par Jonathan Chibois
URL : http://www.laspic.eu/circos-shp (septembre 2013)
	* Géoréférencement, projection Lambert93 (EPSG:2154) :
		* translation (650 000 mètres en x ; 6 800 000 mètres en y)
		* homothétie (rapport de 2002,01 en x ; 1851 en y)
        	* ajustement par vecteur sur fond de carte IGN (10 points).
    	* Ajout des données de surface de toutes les circonscriptions (en m²).
	* Transformation des circonscriptions doubles en une seule (32 fusions).
    	* Corrections diverses :
      		* ajout de la circonscription 6903 qui était inexistante ;
        	* correction d'un sommet mal ajusté dans la circonscription 6901 ;


