library(tidyverse)
library(data.table)
library(glue)
library(did) #CSA (2021)
library(ggtext)
library(HonestDiD)
source("data_prep.R")
source("HonestDID_did.R")
### Reminder : 30-day periods relative to 15/04/2022 (all dates being application submission dates)
# mpr_data %>% filter(rel_month==5) %>% summarise(min = min(date), max = max(date)) -> from 12/09 to 11/10
# -> so previous default sample (rel_month in [-6;4]) covers 17/10/2021 to 11/09/2022
# period and rel_month variables are identical except rel_month = period+1 (iirc due to some syntax details of CSA's did() and the DCDH functions)

folder_write_prefix <- "graphs_04_02_2026/" ### CHANGE MANUALLY

subtypes_control = c("ces","cet","ventil2")
subtypes_control_exventil = c("ces","cet")
subtypes_control_full = c("cgthpe","ces","cet","cuve","ventil2","réseau","panneau")

# Estimates ---------------------------------------------------------------



#### MPR ####
did_pcae_mpr <- att_gt(data= mpr_monthly %>%
                             filter(subtype == "pcae" | subtype %in% c(subtypes_control_exventil)) %>% 
                             filter(hometype=="TMO")%>% 
                             filter(rel_month > -12 & rel_month < 13),
                             yname = "mpr",
                             tname = "period",
                             idname = "zegeste",
                             gname = "treated_month",
                             allow_unbalanced_panel = TRUE,
                             xformla = ~rfrsum,
                       weightsname = "popweight",
                       base_period = "universal")

did_pcae_mpr

graph_did_pcae_mpr <- ggdid(did_pcae_mpr)+
  theme(plot.caption = element_markdown())+
  labs(title = "Pompe à chaleur air/eau, ménages très modestes",
       subtitle = "Moyenne ZE, Prime MPR (en euros)\n Groupe de contrôle : autres gestes (excl. isolation)",
       caption = glue("t = 0 correspond aux 30 jours avant le 15/04/2022.<br> t=1 correspond aux 30 jours à partir de la date d'intervention (incluse).<br>**P-value pre-trends : {did_pcae_mpr$Wpval}**"))

graph_did_pcae_mpr

# ggsave("graphs_25_07_2025/did_pcae_mpr.pdf")

#### CEE ####


# mutate(logcee=log(cee),
#        logrfrsum=log(rfrsum))%>%
#   filter(!rfrsum==0, !cee==0)

did_pcae_tmo_cee<- att_gt(data= mpr_monthly %>%
                            filter(subtype == "pcae" | subtype %in% c(subtypes_control_exventil) ) %>% 
                            filter(hometype=="TMO")%>% 
                            filter(rel_month > -12 & rel_month < 13),
                          yname = "cee",
                          tname = "period",
                          idname = "zegeste",
                          gname = "treated_month",
                          allow_unbalanced_panel = TRUE,
                          xformla = ~rfrsum,
                          weightsname = "popweight",
                          base_period = "universal")

did_pcae_tmo_cee

graph_did_pcae_tmo_cee <- ggdid(did_pcae_tmo_cee)+
  theme(plot.caption = element_markdown())+
  labs(title = "Pompe à chaleur air/eau, ménages très modestes",
       subtitle = "Moyenne ZE, Prime CEE (euros)\n",
       caption = glue("t = 0 correspond aux 30 jours avant le 15/04/2022.<br>t=1 correspond aux 30 jours à partir de la date d'intervention (incluse).<br>**P-value pre-trends : {did_pcae_tmo_cee$Wpval}**"))

graph_did_pcae_tmo_cee

ggsave(paste0(folder_write_prefix,"graph_did_pcae_cee.png"))



aggte_cee = aggte(
  did_pcae_tmo_cee,
  type = "dynamic"
)

hdid_cee_RM = honest_did(aggte_cee,
                      type="relative_magnitude",
                      Mbarvec=seq(from = 0.5, to = 2, by = 0.5))

hdid_cee_SD =  honest_did(aggte_cee,
                         e= 6,
                         type="smoothness",
                         biasDirection = "negative",
                         monotonicity = "decreasing")

ceehdid_plot_SD = createSensitivityPlot(hdid_cee_SD$robust_ci,
                                     hdid_cee_SD$orig_ci)+
  labs(title = "Effet CEE (euros), t =6",
       subtitle = "Sensibilité aux déviations de pre-trend à une tendance linéaire",
       caption = "Fréquence mensuelle")
ceehdid_plot_SD
ggsave(paste0(folder_write_prefix,"HDID/cee_levels/HDID_SD.png"))


ceehdid_plot_RM = createSensitivityPlot_relativeMagnitudes(hdid_cee_RM$robust_ci,
                                        hdid_cee_RM$orig_ci)+
  labs(title = "Effet CEE (euros), t =6",
       subtitle = "Sensibilité aux déviations de pre-trend",
       caption = "Fréquence mensuelle")
ceehdid_plot_RM


ggsave(paste0(folder_write_prefix,"HDID/cee_levels/HDID_RM.png"))

did_pcae_tmo_cee_bimonth<- att_gt(data= mpr_bimonthly %>%
                                    filter(subtype == "pcae" | subtype %in% c(subtypes_control_exventil) ) %>% 
                                    filter(hometype=="TMO")%>% 
                                    filter(rel_bimonth > -7 & rel_bimonth < 5),
                                  yname = "cee",
                                  tname = "period",
                                  idname = "zegeste",
                                  gname = "treated_month",
                                  allow_unbalanced_panel = TRUE,
                                  xformla = ~rfrsum,
                                  weightsname = "popweight",
                                  base_period = "universal")
did_pcae_tmo_cee_bimonth


graph_did_pcae_tmo_cee_bimonth <- ggdid(did_pcae_tmo_cee_bimonth)+
  theme(plot.caption = element_markdown())+
  labs(title = "Pompe à chaleur air/eau, ménages très modestes",
       subtitle = "Moyenne ZE, Prime CEE\n",
       caption = glue("t = 0 correspond aux 60 jours avant le 15/04/2022.<br>t=1 correspond aux 60 jours à partir de la date d'intervention (incluse).<br>**P-value pre-trends : {did_pcae_tmo_cee_bimonth$Wpval}**"))

graph_did_pcae_tmo_cee_bimonth

ggsave(paste0(folder_write_prefix, "graph_did_pcae_tmo_cee_bimonthly.png"))

aggte_ceebimonthly = aggte(
  did_pcae_tmo_cee_bimonth,
  type = "dynamic"
)

hdid_cee_agg_SD = honest_did(aggte_ceebimonthly,
                          type="smoothness",
                          bias ="negative",
                          monotonicity = "decreasing",
                          e = 3)

hdid_cee_agg_RM = honest_did(aggte_ceebimonthly,
                          type="relative_magnitude",
                          e = 3,
                          Mbarvec=seq(from = 0.5, to = 2, by = 0.5))

hdidplot_cee_bimonthly_SD = createSensitivityPlot(hdid_cee_agg_SD$robust_ci,
                                               hdid_cee_agg_SD$orig_ci)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(title = "Effet CEE,t = 3",
       subtitle = "Sensibilité aux pre-trends estimées (SD)",
       caption = "Fréquence bimensuelle. Arguments : bias = negative, monotonicity = decreasing")

hdidplot_cee_bimonthly_SD
ggsave(paste0(folder_write_prefix,"HDID/cee_levels/graph_hdidSD_cee_bimonthly_t3.png"))


hdidplot_cee_bimonthly_RM = createSensitivityPlot_relativeMagnitudes(hdid_cee_agg_RM$robust_ci,
                                                  hdid_cee_agg_RM$orig_ci)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(title = "Effet CEE,t = 3",
       subtitle = "Sensibilité aux pre-trends estimées (RM)",
       caption = "Fréquence bimensuelle.")

debugonce(createSensitivityPlot_relativeMagnitudes)
hdidplot_cee_bimonthly_RM
ggsave(paste0(folder_write_prefix,"HDID/cee_levels/graph_hdidRM_cee_bimonthly_t3.png"))


#### Price ####

did_pcae_price <- att_gt(data= mpr_monthly %>%
                               filter(subtype == "pcae" | subtype %in% c(subtypes_control_exventil)) %>% 
                               filter(hometype=="TMO")%>% 
                               filter(rel_month > -12 & rel_month < 13) %>% 
                               filter(!is.na(price)),
                             yname = "price",
                             tname = "period",
                             idname = "zegeste",
                             gname = "treated_month",
                             allow_unbalanced_panel = TRUE,
                             weightsname = "popweight",
                             xformla = ~rfrsum,
                             base_period = "universal") # NB : point estimates and esp SEs are sensitive to risk_seuil control


did_pcae_price

graph_did_pcae_price <- ggdid(did_pcae_price)+
  theme(plot.caption = element_markdown())+
  labs(title = "PAC air/eau, ménages très modestes",
       subtitle = "Moyenne ZE, Prix TTC (euros)\n",
       caption = glue("t = 0 correspond aux 30 jours avant le 15/04/2022.<br> t=1 correspond aux 30 jours à partir de la date d'intervention (incluse).<br>**P-value pre-trends : {did_pcae_price$Wpval}**"))


graph_did_pcae_price

ggsave(paste0(folder_write_prefix, "graph_did_pcae_tmo_price_wtd_exventil.png"))


aggte_price = aggte(did_pcae_price,
                    type="dynamic")

hdid_price_SD = honest_did(aggte_price,
                        type = "smoothness",
                        bias= "positive",
                        e=4,
)


hdid_price_RM = honest_did(aggte_price,
                           type="relative_magnitude",
                           Mbarvec=seq(from = 0.5, to = 2, by = 0.5),
                        e=4,
)


hdidplot_price_SD = createSensitivityPlot(hdid_price_SD$robust_ci,
                                          hdid_price_SD$orig_ci)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(title = "Effet Prix TTC (euros),t = 4",
       subtitle = "Sensibilité aux déviations de pre-trends linéaires (SD)",
       caption = "Fréquence mensuelle")

hdidplot_price_SD

ggsave(paste0(folder_write_prefix,"HDID/price/graph_hdidSD_price.png"))

hdidplot_price_RM = createSensitivityPlot_relativeMagnitudes(hdid_price_RM$robust_ci,
                                                             hdid_price_RM$orig_ci
                                                             )+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(title = "Effet Prix TTC (euros),t = 5",
       subtitle = "Sensibilité aux déviations de pre-trends linéaires (RM)",
       caption = "Fréquence mensuelle")
hdidplot_price_RM
ggsave(paste0(folder_write_prefix,"HDID/price/graph_hdidRM_price.png"))


did_pcae_tmo_price_bimonthly <- att_gt(data= mpr_bimonthly %>%
                                         filter(subtype == "pcae" | subtype %in% c(subtypes_control_exventil)) %>% 
                                         filter(hometype=="TMO")%>% 
                                         filter(rel_bimonth > -7 & rel_bimonth < 5) %>% 
                                         filter(!is.na(price)) %>% 
                                         filter(!rfrsum==0) %>% 
                                         mutate(logprice=log(price),
                                                logrfrsum =log(rfrsum)),
                                       yname = "price",
                                       tname = "period",
                                       idname = "zegeste",
                                       gname = "treated_month",
                                       allow_unbalanced_panel = TRUE,
                                       weightsname = "popweight",
                                       xformla = ~rfrsum,
                                       base_period = "universal")


graph_did_pcae_tmo_price_bimonthly <- ggdid(did_pcae_tmo_price_bimonthly)+
  theme(plot.caption = element_markdown())+
  labs(title = "PAC air/eau, ménages très modestes",
       subtitle = "Moyenne ZE, Prix TTC\n",
       caption = glue("t = 0 correspond aux 60 jours avant le 15/04/2022.<br> t=1 correspond aux 60 jours à partir de la date d'intervention (incluse).<br>**P-value pre-trends : {did_pcae_tmo_price_bimonthly$Wpval}**"))


graph_did_pcae_tmo_price_bimonthly
ggsave(paste0(folder_write_prefix, "graph_did_pcae_tmo_logprice_bimonthly.png"))


aggte_price_bimonthly = aggte(did_pcae_tmo_price_bimonthly,
                              type="dynamic")

# hdid_price_bimonthly = make_HDID_sensitivity_plots(aggte_price_bimonthly,
#                                                    e_min=-4,
#                                                    e_max=4)

hdid_price_bimonthly_SD = honest_did(aggte_price_bimonthly,
                                  e =2,
                                  type = "smoothness",
                                  bias= "positive",
                                  #Mbarvec=seq(from = 0.5, to = 2, by = 0.5)
)

hdidplot_price_bimonthly_SD = createSensitivityPlot(hdid_price_bimonthly_SD$robust_ci,
                                                 hdid_price_bimonthly_SD$orig_ci)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(title = "Effet Prix TTC,t = 2",
       subtitle = "Sensibilité aux déviations négatives de pre-trends linéaires (SDB)",
       caption = "Fréquence bimensuelle")

hdidplot_price_bimonthly_SD

ggsave(paste0(folder_write_prefix,"HDID/price/graph_hdidSDB_price_bimonthly_t2.png"))


hdid_price_bimonthly_RM = honest_did(aggte_price,
                           type="relative_magnitude",
                           Mbarvec=seq(from = 0.5, to = 2, by = 0.5),
                           e=2,
)


hdidplot_price_bimonthly_RM = createSensitivityPlot_relativeMagnitudes(hdid_price_bimonthly_RM$robust_ci,
                                         hdid_price_bimonthly_RM$orig_ci)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(title = "Effet Prix TTC,t = 2",
       subtitle = "Sensibilité aux déviations de pre-trends linéaires (RM)",
       caption = "Fréquence bimensuelle")

hdidplot_price_bimonthly_RM
ggsave(paste0(folder_write_prefix,"HDID/graph_hdidRM_price_bimonthly_t2.png"))


#### Reste à charge (need HDID) ####

did_pcae_tmo_reste<- att_gt(data= mpr_monthly %>%
                              filter(subtype == "pcae" | subtype %in% c(subtypes_control_exventil)) %>% 
                              filter(hometype=="TMO")%>% 
                              filter(rel_month > -12 & rel_month < 13) %>% 
                              filter(!is.na(reste)),
                            yname = "reste",
                            tname = "period",
                            idname = "zegeste",
                            gname = "treated_month",
                            allow_unbalanced_panel = TRUE,
                            weightsname = "popweight",
                            base_period = "universal",
                            xformla = ~rfrsum)


did_pcae_tmo_reste


graph_did_pcae_tmo_reste <- ggdid(did_pcae_tmo_reste)+
  theme(plot.caption = element_markdown())+
  labs(title = "PAC air/eau, ménages très modestes",
       subtitle = "Moyenne ZE, Reste à charge (euros)\n",
       caption = glue("t = 0 correspond aux 30 jours avant le 15/04/2022.<br> t=1 correspond aux 30 jours à partir de la date d'intervention (incluse).<br>**P-value pre-trends : {did_pcae_tmo_reste$Wpval}**"))

graph_did_pcae_tmo_reste
mean(c(-243,-16,121,245,39))

ggsave(paste0(folder_write_prefix,"graph_did_pcae_reste.png"))


# Robustesse --------------------------------------------------------------

#### Leave one out function ####

did_loo = function(treated,controls,outcome,formula = "~rfrsum", df){
  
  didestimates = list()
  didplots = list()
  for( i in controls){
    didestimates[[i]] = att_gt(data=df %>% 
                                 filter(hometype=="TMO", rel_month > -12, rel_month < 13) %>% 
                                 filter(subtype %in% treated| (subtype %in% controls[!controls %in% i]
                                 ) ),
                               yname = outcome,
                               tname = "period",
                               idname = "zegeste",
                               gname = "treated_month",
                               allow_unbalanced_panel = TRUE,
                               xformla = as.formula(formula),
                               weightsname = "popweight",
                               base_period="universal"
                               
    )
    didplots[[i]] = ggdid(didestimates[[i]])+
      theme(plot.caption = element_markdown(),
            plot.subtitle = element_markdown())+
      labs(title = glue("Pompe à chaleur air/eau, ménages très modestes"),
           subtitle = glue("Dep. Var : {outcome} ;  **{i}** left out from control"),
           caption = glue("t = 0 correspond aux 30 jours avant le 15/04/2022.<br> t=1 correspond aux 30 jours à partir de la date d'intervention (incluse).<br>**P-value pre-trends : {didestimates[[i]]$Wpval}**"))
    
    
  }
  out = list(didestimates,
             didplots)
  return(out)
  
}


loo_cee= did_loo(treated = c("pcae"),
                 controls = subtypes_control,
                 outcome = "cee",
                 df = mpr_monthly)

graph_did_pcae_tmo_cee
loo_cee[[2]][[1]]
ggsave(paste0(folder_write_prefix,"graph_didrobustness_cee_exces.png"))

loo_cee[[2]][[2]]
ggsave(paste0(folder_write_prefix,"graph_didrobustness_cee_excet.png"))
loo_cee[[2]][[3]]
ggsave(paste0(folder_write_prefix,"graph_didrobustness_cee_exventil.png"))



loo_price = did_loo(treated = c("pcae"),
                    controls = subtypes_control,
                    outcome = "price",
                    df = mpr_monthly)

graph_did_pcae_price
loo_price[[2]][[1]]
ggsave(paste0(folder_write_prefix,"graph_didrobustness_price_exces.png"))

loo_price[[2]][[2]]
ggsave(paste0(folder_write_prefix,"graph_didrobustness_price_excet.png"))

loo_price[[2]][[3]]
ggsave(paste0(folder_write_prefix,"graph_didrobustness_price_exventil.png"))

loo_reste = did_loo(treated = c("pcae"),
                    controls = subtypes_control,
                    outcome = "reste",
                    df = mpr_monthly)

loo_reste[[2]][[1]]
ggsave(paste0(folder_write_prefix,"graph_didrobustness_reste_exces.png"))

loo_reste[[2]][[2]]
ggsave(paste0(folder_write_prefix,"graph_didrobustness_reste_excet.png"))

loo_reste[[2]][[3]]
ggsave(paste0(folder_write_prefix,"graph_didrobustness_reste_exventil.png"))
