library(tidyverse)
library(data.table)
library(glue)
library(openxlsx)
library(sf)


subtypename_select <- c("Pompe à chaleur air / eau", #treated
                        "Chaudière à granulés", #treated
                        #"Chaudière gaz THPE",
                        "Chauffe-eau thermodynamique",
                        "Chauffe-eau solaire individuel",
                        "Dépose de cuve à fioul",
                        "Ventilation double flux",
                        "Réseau de chaleur et de froid",
                        "Partie thermique d'un panneau hybride")
subtypes_treated <- c("cag","pcae")
subtypes_control <- c("ces","cet","cuve","ventil2","réseau","panneau")

# CASD Data Import --------------------------------------------------------



#Variables sélectionnées (fichiers "dossier") 
# NB : ces variables de montants ("TTMT....") se rapportent à l'ensemble du dossier MPR (i.e. potentiellement plusieurs gestes)
col_select <- stringr::str_to_lower(c("PYIDDOS", "CODEINSEE", "DEPT","DTDEPOTDOSSIER", "HOMETYPE","RFRSUM", "TYPELGT","WORKAREACODE", 
                                      "TTMTWP", "TTMTTTCPLANFINSOLDE", "MTWPAVANCE","TTMTWPSOLDE", "TTMTCEESOLDE","PYSTATUSWORK" , "NBSUBTYPESOLDERT",
                                      "LATITUDELGT", "LATITUDELGT_GEO", "LONGITUDELGT","LONGITUDELGT_GEO", "RESULTAT_GEO",
                                      "MTSUBISDOTHERSOLDE","MTSUBSIDALSOLDE","MTSUBSIDPENSIONSOLDE","MTSUBSIDTERRITCOMSOLDE","REMAINSDEPENDSOLDE"))

#Variables sélectionnées (fichier gestes) 

col_gestes <- c("pyiddos",  "dtfacture","dtemissiondevis",
                "mtttcplanfininit", "mtttcplanfin","mtttcplanfinsoldeinit", "mtttcplanfinsolde","mtwpinit", "mtwpsolde",
                "familyname","subtypename","siretsct","socialreasonsct", "wpequipement",
                "wpsurface")

#Variables sélectionnées (après join des fichiers Dossier,Geste,CEE) 

col_selectf <- c("pyiddos", "dtdepotdossier", "dtemissiondevis", "dept","codeinsee", "latitudelgt","latitudelgt_geo","longitudelgt","longitudelgt_geo","hometype","rfrsum","typelgt","workareacode", "familyname", "subtypename",
                 "mtttcplanfininit", "mtttcplanfin","mtttcplanfinsoldeinit", "mtttcplanfinsolde", #montants travaux déclarés
                 "mtwpinit","mtwpsolde","mtceeinit", "mtceesolde","mtwpavance",
                 "ttmtwpsolde","ttmtceesolde","nbsubtypesoldert", "pystatuswork", "siretsct","socialreasonsct",
                 "wpsurface", "dtfacture", str_to_lower(c("TTMTTTCPLANFINSOLDE","MTSUBISDOTHERSOLDE","MTSUBSIDALSOLDE","MTSUBSIDPENSIONSOLDE","MTSUBSIDTERRITCOMSOLDE","REMAINSDEPENDSOLDE"))) # totaux subvention/prime sur ensemble du dossier (i.e. potentiellement >1 geste)


## function to remove obs with 0s and NAs for some variables (subvention CEE, prix TTC, type of improvement...)

clean_subsvars <- function(dt){
  dtout<- dt[! (mtceesolde==0|is.na(mtceesolde)|mtttcplanfinsolde == 0 | is.na(mtttcplanfinsolde))] %>% # remove all obs with 0/NA CEE or price
    .[!(is.na(subtypename)| subtypename=="")] %>%  # obs where improvement type is NA or left blank
    left_join(dt[! (mtceesolde==0|is.na(mtceesolde)|mtttcplanfinsolde == 0 | is.na(mtttcplanfinsolde))] %>% #join with a temporary version of the dataset; i.e filtering as above then summarising to get N = count variable ;
                .[!(is.na(subtypename)| subtypename=="")] %>%
                .[, .N , by = c("pyiddos","subtypename","siretsct","latitudelgt","longitudelgt")] %>% # this to check if there are duplicate entries
                .[, .(pyiddos,subtypename,siretsct,latitudelgt,longitudelgt,duplicate= ifelse(N<2, 0, 1))],
              by =c("pyiddos","subtypename","siretsct","latitudelgt","longitudelgt")) %>%
    .[, mtceesolde := case_when(nbsubtypesoldert==1 & (is.na(mtceesolde | mtceesolde==0))~ ttmtceesolde, TRUE ~ mtceesolde)] #si un seul geste dans le dossier, assigner la valeur totale renseignée dans le fichier dossier

  return(dtout)
}

# Same function, but keeps obs with 0 or NA CEE. Used only for descriptive graphs.
# 
# clean_subsvars <- function(dt){
#   dtout<- dt[! (mtttcplanfinsolde == 0 | is.na(mtttcplanfinsolde))] %>% # remove all obs with 0/NA price
#     .[!(is.na(subtypename)| subtypename=="")] %>%  # obs where improvement type is NA or left blank
#     left_join(dt[! (mtttcplanfinsolde == 0 | is.na(mtttcplanfinsolde))] %>% #join with a temporary version of the dataset; i.e filtering as above then summarising to get N = count variable ;
#                 .[!(is.na(subtypename)| subtypename=="")] %>%
#                 .[, .N , by = c("pyiddos","subtypename","siretsct","latitudelgt","longitudelgt")] %>% # this to check if there are duplicate entries
#                 .[, .(pyiddos,subtypename,siretsct,latitudelgt,longitudelgt,duplicate= ifelse(N<2, 0, 1))],
#               by =c("pyiddos","subtypename","siretsct","latitudelgt","longitudelgt")) %>%
#     .[, mtceesolde := case_when(nbsubtypesoldert==1 & (is.na(mtceesolde | mtceesolde==0))~ ttmtceesolde, TRUE ~ mtceesolde)] #si un seul geste dans le dossier, assigner la valeur totale renseignée dans le fichier dossier
# 
#   return(dtout)
# }




#### 2020 ####

mpr_2020 <- lapply(c("2","3","4"),
                   function(x){
                     fread(file = glue("Ma Prime Rénov_MPR Instruction Nationale_2020/2020T{x}/InstructionNat_engt_2020T{x}_Dossier_CASD.csv"),
                           encoding = "UTF-8") %>% 
                       .[,..col_select]
                     
                   }) %>% 
  rbindlist()

mpr_2020gestes <- lapply(c("2","3","4"),
                         function(x){fread(file = glue("Ma Prime Rénov_MPR Instruction Nationale_2020/2020T{x}/InstructionNat_engt_2020T{x}_Geste_CASD.csv"),
                                           encoding="UTF-8")%>%
                             .[subtypename %in% subtypename_select,..col_gestes] #c("pyiddos","siretsct","mtwpsolde")
                         }
) %>% 
  rbindlist()

mpr_2020cee <- lapply(c("2","3","4"),
                      function(x){fread(file = glue("Ma Prime Rénov_MPR Instruction Nationale_2020/2020T{x}/InstructionNat_engt_2020T{x}_CEE_CASD.csv"),
                                        encoding = "UTF-8")%>%
                          .[subtypenamecee %in% subtypename_select,c("pyiddos","subtypenamecee","mtceeinit","mtcee","mtceesoldeinit", "mtceesolde")] #c("pyiddos","siretsct","mtwpsolde")
                      }
) %>% 
  rbindlist() %>% 
  rename(subtypename = subtypenamecee)


mpr_2020f <- mpr_2020gestes %>% 
  left_join(mpr_2020cee, by = c("pyiddos","subtypename")) %>% 
  merge(mpr_2020, by = "pyiddos") %>% 
  select(all_of(col_selectf)) %>% 
  clean_subsvars()

rm(mpr_2020,mpr_2020cee,mpr_2020gestes)
#### 2021 ####

mpr_2021 <- lapply(c("1","2","3","4"),
                   function(x){
                     fread(file = glue("Ma Prime Rénov_MPR Instruction Nationale_2021/2021T{x}/InstructionNat_engt_2021T{x}_Dossier_CASD.csv"),
                           encoding ="UTF-8") %>% 
                       .[,..col_select]
                     
                   }) %>% 
  rbindlist() 

mpr_2021gestes <- lapply(c("1","2","3","4"),
                         function(x){fread(file = glue("Ma Prime Rénov_MPR Instruction Nationale_2021/2021T{x}/InstructionNat_engt_2021T{x}_Geste_CASD.csv"),
                                           encoding="UTF-8")%>%
                             .[subtypename %in% subtypename_select,..col_gestes] #c("pyiddos","siretsct","mtwpsolde")
                         }
) %>% 
  rbindlist()

mpr_2021cee <- lapply(c("1","2","3","4"),
                      function(x){fread(file = glue("Ma Prime Rénov_MPR Instruction Nationale_2021/2021T{x}/InstructionNat_engt_2021T{x}_CEE_CASD.csv"),
                                        encoding = "UTF-8")%>%
                          .[subtypenamecee %in% subtypename_select,c("pyiddos","subtypenamecee","mtceeinit","mtcee","mtceesoldeinit", "mtceesolde")] #c("pyiddos","siretsct","mtwpsolde")
                      }
) %>% 
  rbindlist() %>% 
  rename(subtypename = subtypenamecee)


mpr_2021f <- mpr_2021gestes %>% 
  left_join(mpr_2021cee, by = c("pyiddos","subtypename")) %>% 
  merge(mpr_2021, by = "pyiddos") %>% 
  select(all_of(col_selectf)) %>% 
  clean_subsvars()

rm(mpr_2021,mpr_2021cee,mpr_2021gestes)

#### 2022 ####

mpr_2022 <- lapply(c("1","2","3","4"),
                   function(x){
                     fread(file = glue("Ma Prime Rénov_MPR Instruction Nationale_2022/2022T{x}/InstructionNat_engt_2022T{x}_Dossier_CASD.csv"),
                           encoding ="UTF-8") %>% 
                       .[,..col_select]
                     
                   }) %>% 
  rbindlist()

mpr_2022gestes <- lapply(c("1","2","3","4"),
                         function(x){fread(file = glue("Ma Prime Rénov_MPR Instruction Nationale_2022/2022T{x}/InstructionNat_engt_2022T{x}_Geste_CASD.csv"),
                                           encoding="UTF-8")%>%
                             .[subtypename %in% subtypename_select,..col_gestes] #c("pyiddos","siretsct","mtwpsolde")
                         }
) %>% 
  rbindlist()

mpr_2022cee <- lapply(c("1","2","3","4"),
                      function(x){fread(file = glue("Ma Prime Rénov_MPR Instruction Nationale_2022/2022T{x}/InstructionNat_engt_2022T{x}_CEE_CASD.csv"),
                                        encoding = "UTF-8")%>%
                          .[subtypenamecee %in% subtypename_select,c("pyiddos","subtypenamecee","mtceeinit","mtcee","mtceesoldeinit", "mtceesolde")] #c("pyiddos","siretsct","mtwpsolde")
                      }
) %>% 
  rbindlist() %>% 
  rename(subtypename = subtypenamecee)


mpr_2022f <- mpr_2022gestes %>% 
  left_join(mpr_2022cee, by = c("pyiddos","subtypename")) %>% 
  merge(mpr_2022, by = "pyiddos") %>% 
  select(all_of(col_selectf)) %>% 
  clean_subsvars()

#mpr_2022f[mtwpsolde==0] -> 25 obs

check_duplicate_rows = mpr_2022f[duplicate==1] # here 937 obs (out of total 221640 for 2022)


rm(mpr_2022,mpr_2022cee,mpr_2022gestes)

#### Collect ####

listze <- read.xlsx("ZE2020.xlsx", sheet = "codesZE") %>% 
  rename(codeinsee= CODGEO) %>% 
  select(codeinsee,ZE2020)


popcom = read.xlsx("Fichier_poplegale_2020.xlsx", sheet = "Communes") %>% 
  select("codeinsee" = "Code.commune",codedpt = "Code.département", poptot = "Population.totale") %>% 
  mutate(codeinsee = str_c(codedpt,codeinsee),
         popweight = poptot/sum(poptot))

popze=listze %>% 
  left_join(popcom) %>%
  summarise(pop=sum(poptot,na.rm=TRUE), .by = "ZE2020") %>% 
  mutate(popweight=pop/sum(pop))

listze = listze %>% 
  left_join(popcom)


# ZE outre-mer (à retirer de l'échantillon)
omze <- str_pad(c("101","102","103","104","105",
                  "201","202","203","204","205","206",
                  "301","302","303",
                  "401","402","403","404",
                  "601"),
                4, side = "left", pad = "0")


#duplicated rows (total = 1095 out of total 473172 over 2020-2022)
# the "duplicate" variable is defined in cleanvars() function, i.e. if N >1 with identical pyiddos (ID), address (longitude+latitude), improvement type
duplicates =mpr_2020f %>% 
  bind_rows(mpr_2021f) %>% 
  bind_rows(mpr_2022f) %>% .[duplicate==1] %>% 
  distinct(dtdepotdossier, pyiddos,latitudelgt,longitudelgt,
           subtypename,siretsct,mtceesolde,mtttcplanfinsolde) %>% # this removes "true duplicates" (675/1095), i.e. identical ID, address, improvement type, SIRET, MPR...
  left_join(mpr_2020f %>% 
              bind_rows(mpr_2021f) %>% 
              bind_rows(mpr_2022f) %>% .[duplicate==1]%>%  # but there are still "duplicates", i.e. identical across all variables except CEE or MPR -> entry errors ?
              distinct(dtdepotdossier, pyiddos,latitudelgt,longitudelgt,
                       subtypename,siretsct,mtceesolde,mtttcplanfinsolde) %>% 
              .[,.N, by = c("dtdepotdossier","pyiddos","latitudelgt","longitudelgt","subtypename","siretsct")]) %>% 
  .[! N>1] %>% 
  select(-N) # removes further 178 obs with conflicting CEE or MPR values

mpr_data <- mpr_2020f %>% 
  bind_rows(mpr_2021f) %>% 
  bind_rows(mpr_2022f) %>% 
  left_join(listze, by = "codeinsee") %>% 
  filter(!mtwpsolde ==0) %>% #filter out prime MPR =0
  filter(duplicate==0) %>% # only keep non-duplicated obs
  bind_rows(duplicates) %>% 
  filter (!ZE2020 %in% omze) %>% 
  drop_na(ZE2020) %>% 
  mutate(subtypename=ifelse(subtypename=="Isolation des murs par l’extérieur", "Isolation murs extérieur", subtypename),
         subtype = case_when(grepl("air / eau", subtypename)~ "pcae",
                             grepl("géothermique", subtypename)~ "pcg",
                             grepl("THPE", subtypename) ~ "cgthpe",
                             grepl("combiné", subtypename) ~ "solaire",
                             grepl("Remplacement", subtypename) ~ "remp_fenetre",
                             grepl("intérieur", subtypename) ~ "iso_mursint",
                             grepl("granulés", subtypename) ~ "cag",
                             grepl("bûches", subtypename) ~ "cab",
                             grepl("en parties communes", subtypename) ~ "iso_mursext_comm",
                             grepl("pente", subtypename) ~ "iso_combles",
                             grepl("terrasse", subtypename) ~ "iso_toiture",
                             grepl("Protection", subtypename) ~ "prot_solaire",
                             grepl("Isolation murs extérieur", subtypename) ~ "iso_mursext",
                             grepl("thermodynamique", subtypename) ~"cet",
                             grepl("Poêle à bûches", subtypename) ~ "pab",
                             grepl("Poêle à granulés", subtypename) ~ "pag",
                             grepl("Dépose", subtypename) ~ "cuve",
                             grepl("insert", subtypename) ~ "insert",
                             grepl("double flux", subtypename) ~ "ventil2",
                             grepl("Chauffe-eau solaire", subtypename) ~ "ces",
                             grepl("Réseau", subtypename) ~ "réseau",
                             grepl("panneau", subtypename) ~ "panneau",
                             grepl("ouvrage", subtypename) ~ "maitrise",
                             TRUE ~ subtypename
         ))%>%
  .[, typelgt := ifelse(typelgt=="1",1,0)] %>%  # recode as numeric for computing shares at local level
  mutate(date = as.Date(dtdepotdossier),
         t = as.numeric(date - as.Date("2022-04-15")), ### for 14/07/2022 t= 90 -> rel_month =3 ; for 15/01/2022 t = -90 -> rel_month = -3
         rel_month = t %/%30,
         period = rel_month+1,
         rel_bimonth = t%/%60,
         periodbi = rel_bimonth+1,
         prepost = ifelse(date < "2022-04-15",1,0),
         mpr_pc = mtwpsolde/mtttcplanfinsolde,
         cee_pc = mtceesolde/mtttcplanfinsolde,
         subv_pc = (mtceesolde+mtwpsolde)/mtttcplanfinsolde,
         seuil_ecretement = ifelse(subv_pc >= 0.9, "1","0"), # NB : seuil d'écrêtement à 90% pour ménages TMO, mais plus bas pour les autres
         seuil_init = ifelse((mtceeinit+mtwpinit)/mtttcplanfininit>= 0.9,"1","0"),
         distance_seuil = mtceesolde+mtwpsolde-(0.9*mtttcplanfinsolde),
         risk_seuil = ifelse(distance_seuil >=-1000,1,0),
         period_pre =  fct_rev(ifelse(rel_month <0,"pre","post")),
         cee_reduced = ifelse(mtceesolde<mtceeinit,"1","0"),
         mpr_reduced = ifelse(mtwpsolde < mtwpinit, "1","0"),
         price_reduced = ifelse(mtttcplanfinsolde < mtttcplanfininit, "1", "0"))%>% # 472077 rows
  .[!(cee_pc +mpr_pc>=1)] # removes CEE + MPR >= price of improvement, i.e. 18 obs


# Checking for outliers in prices
# The cutoff at Q999 is ad hoc, so to be refined (also winsorising rather than trimming the sample..)
# The obs > Q999 are filtered out when aggregating into a panel dataset
price_quantiles <- mpr_data[date < "2022-07-15"& date >= "2021-01-01" ] %>% 
  group_by(subtype,hometype) %>% 
  summarise(q01= quantile(mtttcplanfinsolde, 0.001),
            q1= quantile(mtttcplanfinsolde, 0.01),
            q10= quantile(mtttcplanfinsolde, 0.1),
            q25  = quantile(mtttcplanfinsolde, 0.25),
            q50 = quantile(mtttcplanfinsolde, 0.5),
            q9 = quantile(mtttcplanfinsolde, 0.9),
            q95 = quantile(mtttcplanfinsolde, 0.95),
            q99 = quantile(mtttcplanfinsolde, 0.99),
            q999 = quantile(mtttcplanfinsolde, 0.999),
            max = max(mtttcplanfinsolde)) %>% 
  arrange(hometype,subtype)

cols_summarise <- c("mtwpsolde","mtceesolde","reste","mpr_pc", "cee_pc","mtttcplanfinsolde","reste", "reste_pc",
                    "typelgt","mpr_reduced","cee_reduced","price_reduced","seuil_ecretement", "distance_seuil",
                    "risk_seuil",#"popweight",
                    str_to_lower(c("TTMTTTCPLANFINSOLDE","MTSUBISDOTHERSOLDE","MTSUBSIDALSOLDE","MTSUBSIDPENSIONSOLDE","MTSUBSIDTERRITCOMSOLDE","REMAINSDEPENDSOLDE")))
names_summarised = c("mpr","cee","reste","mpr_pc", "cee_pc","price","reste_pc",
                     "home_share","mpr_reduced","cee_reduced","price_reduced","seuil_ecretement", "distance_seuil",
                     "risk_seuil",#"popweight",
                     str_to_lower(c("TTMTTTCPLANFINSOLDE","MTSUBISDOTHERSOLDE","MTSUBSIDALSOLDE","MTSUBSIDPENSIONSOLDE","MTSUBSIDTERRITCOMSOLDE","REMAINSDEPENDSOLDE")) )

#### Monthly x ZE x improvement panel ####

mpr_monthly <- mpr_data %>%
  left_join(price_quantiles, by = c("subtype","hometype")) %>% 
  filter(!mtttcplanfinsolde > q999 ) %>% 
  mutate(reste = mtttcplanfinsolde-(mtwpsolde+mtceesolde),
         reste_pc = reste/mtttcplanfinsolde) %>% 
  summarise(across(cols_summarise, ~ mean(as.numeric(.), na.rm=TRUE)),
            rfrsum= median(rfrsum),
            .by = c("rel_month","ZE2020","hometype","subtypename")) %>% 
  rename_with(~names_summarised, all_of(cols_summarise)) %>% 
  # .[, .(lapply(.SD, function(x) (mean(as.numeric(x), na.rm=TRUE))), rfr = median(rfrsum, na.rm = TRUE)),
  #   by = c("rel_month","ZE2020","hometype","subtypename"), .SDcols = cols_summarise] #%>% 
  mutate(subtypename=ifelse(subtypename=="Isolation des murs par l’extérieur", "Isolation murs extérieur", subtypename),
         subtype = case_when(grepl("air / eau", subtypename)~ "pcae",
                             grepl("géothermique", subtypename)~ "pcg",
                             grepl("THPE", subtypename) ~ "cgthpe",
                             grepl("combiné", subtypename) ~ "solaire",
                             grepl("Remplacement", subtypename) ~ "remp_fenetre",
                             grepl("intérieur", subtypename) ~ "iso_mursint",
                             grepl("granulés", subtypename) ~ "cag",
                             grepl("bûches", subtypename) ~ "cab",
                             grepl("en parties communes", subtypename) ~ "iso_mursext_comm",
                             grepl("pente", subtypename) ~ "iso_combles",
                             grepl("terrasse", subtypename) ~ "iso_toiture",
                             grepl("Protection", subtypename) ~ "prot_solaire",
                             grepl("Isolation murs extérieur", subtypename) ~ "iso_mursext",
                             grepl("thermodynamique", subtypename) ~"cet",
                             grepl("Poêle à bûches", subtypename) ~ "pab",
                             grepl("Poêle à granulés", subtypename) ~ "pag",
                             grepl("Dépose", subtypename) ~ "cuve",
                             grepl("insert", subtypename) ~ "insert",
                             grepl("double flux", subtypename) ~ "ventil2",
                             grepl("Chauffe-eau solaire", subtypename) ~ "ces",
                             grepl("Réseau", subtypename) ~ "réseau",
                             grepl("panneau", subtypename) ~ "panneau",
                             grepl("ouvrage", subtypename) ~ "maitrise",
                             TRUE ~ subtypename
         ), period = rel_month+1,
         treated_month = ifelse(subtype%in%subtypes_treated,1,0 ),
         geste_indic = case_when(subtype=="pcae"~ 1, #used only for DCDH's estimator
                                 subtype == "cag" ~ 2,
                                 subtype == "cgthpe" ~ 3,
                                 subtype == "pcg" ~ 4,
                                 subtype == "cab" ~ 5,
                                 subtype == "solaire" ~ 6,
                                 subtype == "iso_combles" ~ 7,
                                 subtype == "iso_mursint" ~ 8,
                                 subtype == "iso_toiture"~ 9,
                                 subtype == "remp_fenetre" ~ 10,
                                 subtype=="iso_mursext"~ 11,
                                 subtype == "cet" ~ 12,
                                 subtype == "cuve"~13,
                                 subtype == "insert"~14,
                                 subtype == "ventil2" ~ 15,
                                 subtype == "ces"~ 16,
                                 subtype == "réseau" ~17,
                                 subtype == "panneau"~18,
                                 subtype=="maitrise"~19),
         treated_group = ifelse(subtype%in%subtypes_treated,1,0),
         treated_month = ifelse(subtype%in%subtypes_treated,1,0 ),
         zegeste = as.numeric(str_c(ZE2020,geste_indic)),
         zegeste_str = as.character(zegeste)) %>% 
  mutate(rel_price = price/mean(price[period>-11 & -5>period]), .by = c("hometype","subtype")) %>% 
  left_join(popze)




#### Bimonthly x ZE x improvement panel ####
mpr_bimonthly =  mpr_data %>%
  left_join(price_quantiles, by = c("subtype","hometype")) %>% 
  filter(!mtttcplanfinsolde > q999 ) %>% 
  mutate(reste = mtttcplanfinsolde-(mtwpsolde+mtceesolde),
         reste_pc = reste/mtttcplanfinsolde) %>% 
  summarise(across(cols_summarise, ~ mean(as.numeric(.), na.rm=TRUE)),
            rfrsum= median(rfrsum),
            .by = c("rel_bimonth","ZE2020","hometype","subtypename")) %>% 
  rename_with(~names_summarised, all_of(cols_summarise)) %>% 
  # .[, .(lapply(.SD, function(x) (mean(as.numeric(x), na.rm=TRUE))), rfr = median(rfrsum, na.rm = TRUE)),
  #   by = c("rel_month","ZE2020","hometype","subtypename"), .SDcols = cols_summarise] #%>% 
  mutate(subtypename=ifelse(subtypename=="Isolation des murs par l’extérieur", "Isolation murs extérieur", subtypename),
         subtype = case_when(grepl("air / eau", subtypename)~ "pcae",
                             grepl("géothermique", subtypename)~ "pcg",
                             grepl("THPE", subtypename) ~ "cgthpe",
                             grepl("combiné", subtypename) ~ "solaire",
                             grepl("Remplacement", subtypename) ~ "remp_fenetre",
                             grepl("intérieur", subtypename) ~ "iso_mursint",
                             grepl("granulés", subtypename) ~ "cag",
                             grepl("bûches", subtypename) ~ "cab",
                             grepl("en parties communes", subtypename) ~ "iso_mursext_comm",
                             grepl("pente", subtypename) ~ "iso_combles",
                             grepl("terrasse", subtypename) ~ "iso_toiture",
                             grepl("Protection", subtypename) ~ "prot_solaire",
                             grepl("Isolation murs extérieur", subtypename) ~ "iso_mursext",
                             grepl("thermodynamique", subtypename) ~"cet",
                             grepl("Poêle à bûches", subtypename) ~ "pab",
                             grepl("Poêle à granulés", subtypename) ~ "pag",
                             grepl("Dépose", subtypename) ~ "cuve",
                             grepl("insert", subtypename) ~ "insert",
                             grepl("double flux", subtypename) ~ "ventil2",
                             grepl("Chauffe-eau solaire", subtypename) ~ "ces",
                             grepl("Réseau", subtypename) ~ "réseau",
                             grepl("panneau", subtypename) ~ "panneau",
                             grepl("ouvrage", subtypename) ~ "maitrise",
                             TRUE ~ subtypename
         ), period = rel_bimonth+1,
         treated_month = ifelse(subtype%in%subtypes_treated,1,0 ),
         geste_indic = case_when(subtype=="pcae"~ 1,
                                 subtype == "cag" ~ 2,
                                 subtype == "cgthpe" ~ 3,
                                 subtype == "pcg" ~ 4,
                                 subtype == "cab" ~ 5,
                                 subtype == "solaire" ~ 6,
                                 subtype == "iso_combles" ~ 7,
                                 subtype == "iso_mursint" ~ 8,
                                 subtype == "iso_toiture"~ 9,
                                 subtype == "remp_fenetre" ~ 10,
                                 subtype=="iso_mursext"~ 11,
                                 subtype == "cet" ~ 12,
                                 subtype == "cuve"~13,
                                 subtype == "insert"~14,
                                 subtype == "ventil2" ~ 15,
                                 subtype == "ces"~ 16,
                                 subtype == "réseau" ~17,
                                 subtype == "panneau"~18,
                                 subtype=="maitrise"~19),
         treated_group = ifelse(subtype%in%subtypes_treated,1,0),
         treated_month = ifelse(subtype%in%subtypes_treated,1,0 ),
         zegeste = as.numeric(str_c(ZE2020,geste_indic)),
         zegeste_str = as.character(zegeste)) %>% 
  mutate(rel_price = price/mean(price[period>-11 & -5>period]), .by = c("hometype","subtype")) %>% 
  left_join(popze)



#### Monthly panel, # of improvements ####


mpr_monthly_ngestes <- mpr_data %>%
  filter(hometype=="TMO") %>% 
  left_join(price_quantiles, by = c("subtype","hometype")) %>% 
  #filter(t >= -90 & t < 91) %>% 
  filter(!mtttcplanfinsolde > q999 ) %>%
  #left_join(check_multiplerows, by = c("pyiddos","subtypename"))# %>%  
  .[, .(ngestes = .N, home_share = mean(typelgt, na.rm= TRUE),
        median_rfr = median(rfrsum, na.rm= TRUE)),
    by = .(rel_month,ZE2020,hometype,subtypename)] %>% 
  mutate(subtypename=ifelse(subtypename=="Isolation des murs par l’extérieur", "Isolation murs extérieur", subtypename),
         subtype = case_when(grepl("air / eau", subtypename)~ "pcae",
                             grepl("géothermique", subtypename)~ "pcg",
                             grepl("THPE", subtypename) ~ "cgthpe",
                             grepl("combiné", subtypename) ~ "solaire",
                             grepl("Remplacement", subtypename) ~ "remp_fenetre",
                             grepl("intérieur", subtypename) ~ "iso_mursint",
                             grepl("granulés", subtypename) ~ "cag",
                             grepl("bûches", subtypename) ~ "cab",
                             grepl("en parties communes", subtypename) ~ "iso_mursext_comm",
                             grepl("pente", subtypename) ~ "iso_combles",
                             grepl("terrasse", subtypename) ~ "iso_toiture",
                             grepl("Protection", subtypename) ~ "prot_solaire",
                             grepl("Isolation murs extérieur", subtypename) ~ "iso_mursext",
                             grepl("thermodynamique", subtypename) ~"cet",
                             grepl("Poêle à bûches", subtypename) ~ "pab",
                             grepl("Poêle à granulés", subtypename) ~ "pag",
                             grepl("Dépose", subtypename) ~ "cuve",
                             grepl("insert", subtypename) ~ "insert",
                             grepl("double flux", subtypename) ~ "ventil2",
                             grepl("Chauffe-eau solaire", subtypename) ~ "ces",
                             grepl("Réseau", subtypename) ~ "réseau",
                             grepl("panneau", subtypename) ~ "panneau",
                             grepl("ouvrage", subtypename) ~ "maitrise",
                             TRUE ~ subtypename
         ),
         period = rel_month+1,
         treated_month = ifelse(subtype%in%subtypes_treated,1,0 ),
         geste_indic = case_when(subtype=="pcae"~ 1,
                                 subtype == "cag" ~ 2,
                                 subtype == "cgthpe" ~ 3,
                                 subtype == "pcg" ~ 4,
                                 subtype == "cab" ~ 5,
                                 subtype == "solaire" ~ 6,
                                 subtype == "iso_combles" ~ 7,
                                 subtype == "iso_mursint" ~ 8,
                                 subtype == "iso_toiture"~ 9,
                                 subtype == "remp_fenetre" ~ 10,
                                 subtype=="iso_mursext"~ 11,
                                 subtype == "cet" ~ 12,
                                 subtype == "cuve"~13,
                                 subtype == "insert"~14,
                                 subtype == "ventil2" ~ 15,
                                 subtype == "ces"~ 16,
                                 subtype == "réseau" ~17,
                                 subtype == "panneau"~18,
                                 subtype=="maitrise"~19),
         zegeste = as.numeric(str_c(ZE2020,geste_indic)),
         zegeste_str = as.character(zegeste)) %>% 
  complete(ZE2020,rel_month,subtype)%>% 
  mutate(ngestes=replace_na(ngestes,0)) %>% 
  data.table()

temp <- copy(mpr_monthly_ngestes)
temp[, rel_month:= rel_month+12]
setnames(temp, "ngestes", "Nl12")
mpr_monthly_ngestes[temp, on= .(zegeste_str,subtype,hometype, rel_month), Nl12 := i.Nl12  ]
mpr_monthly_ngestes[, relngestes := (ngestes/Nl12)-1] %>% 
  .[, dngestes := ngestes-Nl12]




mpr_bimonthly_ngestes <- mpr_data %>%
  filter(hometype=="TMO") %>% 
  left_join(price_quantiles, by = c("subtype","hometype")) %>% 
  filter(!mtttcplanfinsolde > q999 ) %>%
  .[, .(ngestes = .N, home_share = mean(typelgt, na.rm= TRUE),
        rfrsum = median(rfrsum, na.rm= TRUE)),
    by = .(rel_bimonth,ZE2020,hometype,subtypename)] %>% 
  mutate(subtypename=ifelse(subtypename=="Isolation des murs par l’extérieur", "Isolation murs extérieur", subtypename),
         subtype = case_when(grepl("air / eau", subtypename)~ "pcae",
                             grepl("géothermique", subtypename)~ "pcg",
                             grepl("THPE", subtypename) ~ "cgthpe",
                             grepl("combiné", subtypename) ~ "solaire",
                             grepl("Remplacement", subtypename) ~ "remp_fenetre",
                             grepl("intérieur", subtypename) ~ "iso_mursint",
                             grepl("granulés", subtypename) ~ "cag",
                             grepl("bûches", subtypename) ~ "cab",
                             grepl("en parties communes", subtypename) ~ "iso_mursext_comm",
                             grepl("pente", subtypename) ~ "iso_combles",
                             grepl("terrasse", subtypename) ~ "iso_toiture",
                             grepl("Protection", subtypename) ~ "prot_solaire",
                             grepl("Isolation murs extérieur", subtypename) ~ "iso_mursext",
                             grepl("thermodynamique", subtypename) ~"cet",
                             grepl("Poêle à bûches", subtypename) ~ "pab",
                             grepl("Poêle à granulés", subtypename) ~ "pag",
                             grepl("Dépose", subtypename) ~ "cuve",
                             grepl("insert", subtypename) ~ "insert",
                             grepl("double flux", subtypename) ~ "ventil2",
                             grepl("Chauffe-eau solaire", subtypename) ~ "ces",
                             grepl("Réseau", subtypename) ~ "réseau",
                             grepl("panneau", subtypename) ~ "panneau",
                             grepl("ouvrage", subtypename) ~ "maitrise",
                             TRUE ~ subtypename
         ),
         period = rel_bimonth+1,
         treated_month = ifelse(subtype%in%subtypes_treated,1,0 ),
         geste_indic = case_when(subtype=="pcae"~ 1,
                                 subtype == "cag" ~ 2,
                                 subtype == "cgthpe" ~ 3,
                                 subtype == "pcg" ~ 4,
                                 subtype == "cab" ~ 5,
                                 subtype == "solaire" ~ 6,
                                 subtype == "iso_combles" ~ 7,
                                 subtype == "iso_mursint" ~ 8,
                                 subtype == "iso_toiture"~ 9,
                                 subtype == "remp_fenetre" ~ 10,
                                 subtype=="iso_mursext"~ 11,
                                 subtype == "cet" ~ 12,
                                 subtype == "cuve"~13,
                                 subtype == "insert"~14,
                                 subtype == "ventil2" ~ 15,
                                 subtype == "ces"~ 16,
                                 subtype == "réseau" ~17,
                                 subtype == "panneau"~18,
                                 subtype=="maitrise"~19),
         zegeste = as.numeric(str_c(ZE2020,geste_indic)),
         zegeste_str = as.character(zegeste)) %>% 
  complete(ZE2020,rel_bimonth,subtype)%>% 
  mutate(ngestes=replace_na(ngestes,0)) %>% 
  data.table()


temp <- copy(mpr_bimonthly_ngestes)
temp[, rel_bimonth:= rel_bimonth+12]
setnames(temp, "ngestes", "Nl12")
mpr_bimonthly_ngestes[temp, on= .(zegeste_str,subtype,hometype, rel_bimonth), Nl12 := i.Nl12  ]
mpr_bimonthly_ngestes[, relngestes := (ngestes/Nl12)-1] %>% 
  .[, dngestes := ngestes-Nl12]

mpr_bimonthly_ngestes = mpr_bimonthly_ngestes %>% 
  left_join(popze)
unique(mpr_bimonthly_ngestes$treated_month)

mpr_bimonthly_ngestes[period==1]
