# Installation : https://inseefrlab.github.io/melodi/
library(melodi)
library(tidyverse)
library(openxlsx)
é#remotes::install_github("InseeFrLab/melodi", build_vignettes = TRUE)
# Lister les jeux de données proposés par l’Insee via Melodi
get_catalog()

# Récupérer toutes les données d’un jeu de données par son identifiant
data <- get_all_data("DS_POPULATIONS_REFERENCE")

# Filtrer sur un territoire
get_local_data(
  ds_name = "DS_POPULATIONS_REFERENCE",
  geo = "01001",
  geo_object = "COM"
)
test = get_data(
  url = "https://api.insee.fr/melodi/data/DS_RP_MENAGES_COMP?GEO=ZE2020&TIME_PERIOD=2022&PREFPH=_T&TPH=_T&PCS=_T"
)

test |> filter(GEO=="4423")

test = get_local_data(
  ds_name = "DS_RP_MENAGES_COMP",
  geo_object = "ZE2020",
  filter = "TIME_PERIOD=2022&PREFPH=_T"
)

write.xlsx(test, file = "popZE.xlsx")
