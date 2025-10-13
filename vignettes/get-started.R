## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(valorisationQE)

## ----examples-f_graph_pluviometrie--------------------------------------------

if (interactive()) {
  config_path <- "C://workspace//gwilenalim//yaml//config.yml"
  if (file.exists(config_path)) {
    library(RPostgres)
    library(meteo4Vilaine)
    config <- yaml::read_yaml(config_path)
    con <- tryCatch({
      DBI::dbConnect(
        RPostgres::Postgres(),
        host = config$host,
        port = config$port,
        user = config$user,
        password = config$password,
        dbname = config$dbname
      )
    }, error = function(e) NULL)
    
    
    
    if (!is.null(con)) {
      
      triangle_sf <- sf::st_sf(
        geometry = sf::st_sfc(
          sf::st_polygon(list(rbind(
            c(-1.6794, 48.1147),  # Rennes
            c(-1.2100, 48.1230),  # Vitré
            c(-1.5025, 47.6833),  # Derval
            c(-1.6794, 48.1147)   # Retour à Rennes
          )))
        ),
        crs = 4326
      )
      
      g<-f_graph_pluviometrie(triangle_sf, date = as.Date("2025-09-11"), con)
      DBI::dbDisconnect(con)
      print(g)
    } else {
      message("Connexion à la base impossible, exemple non exécuté.")
    }
  } else {
    message("Fichier de configuration introuvable.")
  }
}

knitr::include_graphics(system.file("extdata", "graph_pluvio.png", package = "valorisationQE"))



## ----example-f_fond_de_carte--------------------------------------------------

   triangle_sf <- sf::st_sf(
        geometry = sf::st_sfc(
          sf::st_polygon(list(rbind(
            c(-1.6794, 48.1147),  # Rennes
            c(-1.2100, 48.1230),  # Vitré
            c(-1.5025, 47.6833),  # Derval
            c(-1.6794, 48.1147)   # Retour à Rennes
          )))
        ),
        crs = 4326
      )

f_fond_de_carte(triangle_sf, zoom=9)

## ----example-f_cartographie_concentrations------------------------------------

library(sf)
library(dplyr)

# Coordonnées approximatives des centres des communes
coords <- data.frame(
  code = c("A", "B", "C"),
  commune = c("Châteaugiron", "Janzé", "Thourie"),
  lon = c(-1.499, -1.498, -1.4833),
  lat = c(48.052, 47.960, 47.8550)
)

# Création de l'objet sf
points_sf <- st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)


# bassin versant
   triangle_sf <- sf::st_sf(
        geometry = sf::st_sfc(
          sf::st_polygon(list(rbind(
            c(-1.6794, 48.1147),  # Rennes
            c(-1.2100, 48.1230),  # Vitré
            c(-1.5025, 47.6833),  # Derval
            c(-1.6794, 48.1147)   # Retour à Rennes
          )))
        ),
        crs = 4326
      )

fond_carte<-f_fond_de_carte(triangle_sf, zoom=10)

# Création du data.frame
df_valeurs <- data.frame(
  code = c("A", "B", "C"),
  valeur = c(0.23, 0.71, 3.52)
)

f_cartographie_concentrations(fond_carte,
                              shp_staq=points_sf,
                              data_carte=df_valeurs,
                              col_stations="code",
                              col_valeurs="valeur",
                              titre="Bilan des concentrations en pesticides",
                              sous_titre="Date de la campagne",
                              breaks=c(0, 0.5, 2, Inf),
                              values = c("cyan", "yellow", "red", "black"),
                              pts_size=6,
                              text_size=5,
                              unite="µg/L",
                              nom_legende="Concentrations",
                              nb_decimal_text=1)


## ----example-importe_ref_pestibase--------------------------------------------
base_pesticides<-importe_ref_pestibase()

head(base_pesticides)

## ----example-calcule_somme_pesticides-----------------------------------------
data <- data.frame(
   DatePrel = Sys.Date() + rep(sort(sample(1:500, 10)), 3),
   RsAna = round(runif(60, 0, 0.5), 2),
   LqAna = 0.1,
   CdStationMesureEauxSurface = c("A", "B", "C"),
   CdParametre = c("1200", "1506"),
   CdUniteMesure = "133"
 )
 data$CdRqAna <- ifelse(data$RsAna >= data$LqAna, "1", "10")
 calcule_somme_pesticides(data)

## ----example-analyse_fonctions_pesticides-------------------------------------

ref_pestibase <- importe_ref_pestibase()

data <- data.frame(
   DatePrel = Sys.Date() + rep(sort(sample(1:500, 10)), 3),
   RsAna = round(runif(60, 0, 0.5), 2),
   LqAna = 0.1,
   CdStationMesureEauxSurface = c("A", "B", "C"),
   CdParametre = sample(ref_pestibase$liste_phytos_autorises$SA_CodeSANDRE,
                        size=10,
                        replace=TRUE),
   CdUniteMesure = "133"
 )
 data$CdRqAna <- ifelse(data$RsAna >= data$LqAna, "1", "10")

analyse_fonctions_pesticides(data, 
                             code_remarque="CdRqAna",
                             code_parametre="CdParametre",
                             liste_pesticides=ref_pestibase$liste_phytos_autorises)



## ----example-fiche_usages_pesticides------------------------------------------
ref_pestibase <- importe_ref_pestibase()

fiche_usages_pesticides(liste_cd_sandre=c("2009", "1221", "1882", "5817", "5617", "1678", "1859"),
                                        liste_pesticides=ref_pestibase$liste_pesticides,
                        legend_position = "top",
                        cultures_a_exclure=c("Vignes", "Fourrages"))



