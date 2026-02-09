library(patchwork)
library(tidyverse)
library(data.table)
# 
# mpr_rdd_export <-mpr_rdd %>%
#   select(dtdepotdossier,hometype,subtypename, mtwpsolde,mtceesolde,mtttcplanfinsolde)

mpr_rdd_export <- fread("projet_mpr/mpr_rdd_export.csv",
                        encoding = "Latin-1")

mpr_rdd_filter <- mpr_rdd_export%>% 
  #filter( (substr(dtdepotdossier,1,4) %in% c("2018","2019","2020","2021","2022", "2023", "2024")) ) %>% 
  drop_na(mtwpsolde) %>% 
  filter(!mtwpsolde == 0) %>% 
  mutate(hometype=case_when(hometype == "INT" ~ "Intermédiaires",
                            hometype == "MO" ~ "Modestes",
                            hometype == "TMO" ~ "Très modestes",
                            TRUE ~ hometype),
         year= lubridate::year(dtdepotdossier))

unique(mpr_rdd_filter$hometype)
income_table <- mpr_rdd_filter[ ,.(mean=mean(rfrsum),median=median(rfrsum)), by = .(hometype,year)] %>% 
  arrange(year)
# N= 1936 obs supprimées
mpr_rdd_filter[hometype==""]
unique(mpr_rdd$subtypename)


### Graphs MPR 

mpr_rdd_pompe <- mpr_rdd_filter %>% 
  filter(subtypename == "Pompe à chaleur air / eau") %>% 
  mutate(date = as.Date(dtdepotdossier))%>%
  group_by(date) %>% 
  summarise(mean_mprpaid = mean(mtwpsolde))

dates <- data.frame(date=unique(mpr_rdd$dtdepotdossier)) %>% 
  mutate(date2 = as.Date(date),
         date3 = str_replace(date2, "5021","2021"))

dates %>% filter(! (substr(date,1,4) %in% c("2018","2019","2020","2021","2022")) )


plotnames <- unique(mpr_rdd_export$subtypename)

yearbreaks <- seq(as.Date("2017-01-01"), as.Date("2025-01-01"), by ="year")

data_n_table <- mpr_rdd_export %>% 
  mutate(date = as.Date(dtdepotdossier))%>%
  filter(date < "2022-07-15" & date >"2022-01-01") %>% 
  mutate(group = ifelse(date <"2022-04-15", "pre", "post")) %>% 
  .[, .N, by =.(subtypename,group)] %>% 
  pivot_wider(names_from="group",values_from="N")
data_n_table


data_n_table_hometype <- mpr_rdd_export %>% 
  mutate(date = as.Date(dtdepotdossier))%>%
  filter(date < "2022-07-15" & date >"2022-01-01") %>% 
  mutate(group = ifelse(date <"2022-04-15", "pre", "post")) %>% 
  .[, .N, by =.(hometype,subtypename,group)] %>% 
  pivot_wider(names_from="group",values_from = "N")
data_n_table_hometype



mpr_rdd_export %>% 
  filter(subtypename == "Pompe à chaleur air / eau" )%>% 
  mutate(date = as.Date(dtdepotdossier))%>%
  filter(date < "2022-07-15" & date >"2022-01-01") %>% 
  group_by(date,hometype) %>% 
  summarise(mean_mprpaid = mean(mtwpsolde, na.rm=TRUE)) %>% 
  ungroup()

assignmentplot <- lapply(plotnames, function(x){
  ggplot(mpr_rdd_export %>% 
           filter(subtypename == x )%>% 
           mutate(date = as.Date(dtdepotdossier))%>%
           filter(date < "2022-07-15" & date >"2022-01-01") %>% 
           group_by(date,hometype) %>% 
           summarise(mean_mprpaid = mean(mtwpsolde, na.rm=TRUE)) %>% 
           ungroup(),
         aes(x=date, y= mean_mprpaid))+
    facet_wrap(~hometype)+
    geom_point(alpha=0.1)+
    geom_vline(xintercept=as.Date("2022-04-15"), linetype="dashed", alpha= 0.5)+
    theme_minimal()+
    scale_x_date()+
    # scale_x_date(date_breaks="1 year",
    #              date_labels = "%Y",
    #              breaks = yearbreaks,
    #              date_minor_breaks = "1 month"
    #)+
    ggtitle(x)+
    theme(axis.text.x = element_text(angle=45))+
    labs(caption = "Date = dépôt dossier\n Moyenne par jour de la prime MPR (payée au solde).",
         subtitle = "Prime MPR")
}) %>% 
  setNames(str_c("graph_mpr_",plotnames))

graph_mprpaid_pompes <- assignmentplot[[1]]
graph_mprpaid_pompes


graph_mprpaid_chaudiere <- assignmentplot[[2]]
graph_mprpaid_chaudiere

graph_mprpaid_chauffagesolaire <- assignmentplot[[3]]
graph_mprpaid_chauffagesolaire


graph_mprpaid_chaudieregranules <- assignmentplot[[4]]
graph_mprpaid_chaudieregranules


graph_mprpaid_pompesgeotherm <- assignmentplot[[5]]
graph_mprpaid_pompesgeotherm

graph_mprpaid_chaudierebuche <- assignmentplot[[6]]
graph_mprpaid_chaudierebuche


ggplot(mpr_rdd_export %>% 
         filter(subtypename == "Pompe à chaleur air / eau" )%>% 
         mutate(date = as.Date(dtdepotdossier))%>%
         filter(date < "2022-07-15" & date >"2022-01-01") %>% 
         group_by(date,hometype) %>% 
         summarise(mean_mtceesolde = mean(mtceesolde, na.rm = TRUE)) %>% 
         ungroup(),
       aes(x=date, y= mean_mtceesolde))+
  facet_wrap(~hometype)+
  geom_point(alpha=0.1)+
  geom_vline(xintercept=as.Date("2022-04-15"), linetype="dashed", alpha= 0.5)+
  theme_minimal()+
  scale_x_date()+
  # scale_x_date(date_breaks="1 year",
  #              date_labels = "%Y",
  #              breaks = yearbreaks,
  #              date_minor_breaks = "1 month"
  #)+
  ggtitle("Pompe à chaleur air / eau")+
  theme(axis.text.x = element_text(angle=45))+
  labs(caption = "Date = dépôt dossier.\n Moyenne par jour de la prime CEE (payée au solde).",
       subtitle = "Prime CEE")

primeceeplot <- lapply(plotnames, function(x){
  ggplot(mpr_rdd_export %>% 
           filter(subtypename == x )%>% 
           mutate(date = as.Date(dtdepotdossier))%>%
           filter(date < "2022-07-15" & date >"2022-01-01") %>% 
           group_by(date,hometype) %>% 
           summarise(mean_mtceesolde = mean(mtceesolde, na.rm=TRUE)) %>% 
           ungroup(),
         aes(x=date, y= mean_mtceesolde))+
    facet_wrap(~hometype)+
    geom_point(alpha=0.1)+
    geom_vline(xintercept=as.Date("2022-04-15"), linetype="dashed", alpha= 0.5)+
    theme_minimal()+
    scale_x_date()+
    # scale_x_date(date_breaks="1 year",
    #              date_labels = "%Y",
    #              breaks = yearbreaks,
    #              date_minor_breaks = "1 month"
    #)+
    ggtitle(x)+
    theme(axis.text.x = element_text(angle=45))+
    labs(caption = "Date = dépôt dossier.\n Moyenne par jour de la prime CEE (payée au solde).",
         subtitle = "Prime CEE")
}) %>% 
  setNames(str_c("graph_cee_",plotnames))

primeceeplot[[1]]
primeceeplot[[2]]
primeceeplot[[3]]
primeceeplot[[4]]
primeceeplot[[5]]
primeceeplot[[6]]

prixgesteplot <- lapply(plotnames, function(x){
  ggplot(mpr_rdd_export %>% 
           filter(subtypename == x )%>% 
           mutate(date = as.Date(dtdepotdossier))%>%
           filter(date < "2022-07-15" & date >"2022-01-01") %>% 
           group_by(date,hometype) %>% 
           summarise(mean_mtgeste = mean(mtttcplanfinsolde, na.rm= TRUE)) %>% 
           ungroup(),
         aes(x=date, y= mean_mtgeste))+
    facet_wrap(~hometype)+
    geom_point(alpha=0.1)+
    geom_vline(xintercept=as.Date("2022-04-15"), linetype="dashed", alpha= 0.5)+
    theme_minimal()+
    scale_x_date()+
    ggtitle(x)+
    theme(axis.text.x = element_text(angle=45))+
    labs(caption = "Date = dépôt dossier.\n Moyenne par jour des montants TTC des travaux (au solde).",
         subtitle = "Montant TTC geste")
}) %>% 
  setNames(str_c("graph_mtgeste_",plotnames))


prixgesteplot[[1]]
prixgesteplot[[2]]
prixgesteplot[[3]]
prixgesteplot[[4]]
prixgesteplot[[5]]
prixgesteplot[[6]]


geste_plot <- list()

for(i in 1:6){
  geste_plot[[i]] <- list(assignmentplot[[i]],
                          primeceeplot[[i]],
                          prixgesteplot[[i]]
  ) %>% setNames(c("MPR","CEE","Prix"))
  
  
}
geste_plot <- geste_plot %>% 
  setNames(plotnames)


geste_plot2 <- list()
for(i in 1:6) {
  geste_plot2[[i]]<- geste_plot[[i]][[1]]/geste_plot[[i]][[2]]/geste_plot[[i]][[3]]
}
geste_plot2 <- geste_plot2 %>% 
  setNames(plotnames)
geste_plot2[[1]]


# RDD ---------------------------------------------------------------------

library(nprobust)

### Density estimate ####

est_rdd <- mpr_rdd_filter %>% 
  .[subtypename == "Pompe à chaleur air / eau",] %>% 
  mutate(date = as.Date(dtdepotdossier))%>%
  filter(date < "2022-07-30" & date >"2022-01-01") %>% 
  mutate(t= as.numeric(date - as.Date("2022-04-15")))
est_rdd[,t]

pre_density <- kdrobust(mpr_rdd_filter$mtceesolde, subset = (t>=0))


##### Lprobust estimation #####
est_rdd_pre <- est_rdd[t<0]
est_rdd_post <- est_rdd[t>=0]

est_rdd_pre_tmo <- est_rdd[hometype=="Très modestes" & t<0]
est_rdd_post_tmo <- est_rdd[hometype=="Très modestes" & t>=0]

result_est_linear_pretmo <- lprobust(est_rdd_pre$mtceesolde, 
                       est_rdd_pre$mtwpsolde)

result_est_linear_posttmo <-  lprobust(est_rdd_post$mtceesolde, 
                                est_rdd_post$mtwpsolde)


result_est_quad_pretmo <- lprobust(est_rdd_pre$mtceesolde, 
                                     est_rdd_pre$mtwpsolde,
                                   p=2)

nprobust.plot(result_est_quad_pretmo)
##### export #####

graph_rdd_export <-list(assignmentplot, primeceeplot, prixgesteplot, geste_plot, geste_plot2) %>% 
  setNames(c("assignmentplot", "primeceeplot", "prixgesteplot", "geste_plot", "geste_plot2"))
save(graph_rdd_export,
     file = "projet_mpr/graphs_discontinuites.rda")

tables_list <- list(data_n_table,data_n_table_hometype)
save(tables_list, file = "tables_list.rda")
write.csv(mpr_rdd_export,
          "mpr_rdd_export.csv")

