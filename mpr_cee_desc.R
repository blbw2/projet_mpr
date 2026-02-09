library(tidyverse)
library(openxlsx)

inseegrid_init <- read.xlsx("projet_mpr/grille_densite_7_niveaux_2021.xlsx", sheet= "Grille_Densite")

inseegrid <- inseegrid_init |> 
  set_names(nm = inseegrid_init |> slice(1)) |> 
  slice(-1) |> 
  mutate(dens_3lvl = case_when(DENS %in% c("2","3","4") ~ "2",
                               DENS %in% c("5","6","7") ~ "3",
                               TRUE ~ "1"))
  
rm(inseegrid_init)
