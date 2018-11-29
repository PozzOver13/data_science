########################################################
# PROGRAM:  DATASET - costruzione campione
# DATE:     2018-07-17
# NOTE: 
########################################################

rm(list = ls())
for (i in 1:10) gc()

#### SETUP ####
require(dplyr)
require(tidyr)
require(readr)
require(ggplot2)
require(stringi)
require(stringr)
require(gridExtra)
library(lpSolve)

source("Documents/GitHub/data_science/AFSM/prog/02R_GA_lancio.R")

normalizza <- function(x) {
  min_x = min(x, na.rm = T)
  max_x = max(x, na.rm = T)
  
  (x - min_x) / (max_x - min_x)
}

path_datasets <- "~/Google Drive/Tempo_Libero/" 
path_functs <- "~/Documents/GitHub/data_science/AFSM/prog/functions/"

source(paste0(path_functs, "seleziona_squadra_ottimizzata.R"))

#### DATA IMPORT ####
df_in <- read.csv(file = paste0(path_datasets, "AFSM/Stats/00R_DT_campione_2015_2018.csv"), 
                  sep = "|", stringsAsFactors = F)

df <- df_in %>%
  filter(year == "2016_2017", !is.na(qt_i)) %>%
  mutate(qt_i = as.numeric(str_replace_all(qt_i, ",", ".")))

df_test <- df_in %>%
  filter(year == "2016_2017", !is.na(qt_i)) %>%
  mutate(qt_i = as.numeric(str_replace_all(qt_i, ",", ".")),
         score_pg = normalizza(pg),
         score_mf = normalizza(mf),
         score_gf_p = normalizza(gf_p),
         score_gs_sq = 1 - normalizza(gs_s),
         score_gf_sq = normalizza(gf_s),
         random_st = rnorm(length(pg)),
         score_rig_p = normalizza(rp),
         score_random = normalizza(random_st),
         score_intg = 0.4*score_pg + 0.3*score_mf + 0.2*score_gs_sq + 0.1*score_random,
         
         score_por = 0.5*score_pg + 0.3*score_gs_sq + 0.2*score_rig_p,
         score_dif = 0.5*score_pg + 0.3*score_gs_sq + 0.2*score_mf,
         score_cc  = 0.5*score_pg + 0.1*score_gf_sq + 0.2*score_mf + 0.2*score_gf_p,
         score_att  = 0.5*score_pg + 0.05*score_gf_sq + 0.15*score_mf + 0.3*score_gf_p
         
         ) %>%
  arrange(desc(r))

output <- amv_ga_lancia_algoritmo(df_test,
                                  app_w_score_por_tot = 0.2, 
                                  app_w_score_def_tot = 0.2, 
                                  app_w_score_cc_tot = 0.2, 
                                  app_w_score_att_tot = 0.2,
                                  app_w_costo_tot_nrm = 0.15,
                                  app_w_val_sq_ideale = 0.2,
                                  app_formazione_ideale = c(4,3,3))


df_finale <- output[[1]]
sum(df_finale$qt_i)
