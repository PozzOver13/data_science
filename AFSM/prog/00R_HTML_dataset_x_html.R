########################################################
# PROGRAM:  DATASET - train 1
# DATE:     2018-08-06
# NOTE:     preparazione dataset
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
require(corrplot)
require(xgboost)

normalizza <- function(x) {
  min_x = min(x, na.rm = T)
  max_x = max(x, na.rm = T)
  
  (x - min_x) / (max_x - min_x)
}

path_datasets <- "~/Google Drive/Tempo_Libero/" 
path_functs <- "~/Documents/GitHub/data_science/AFSM/prog/functions/"

source(paste0(path_functs, "metriche_accuracy.R"))

#### DATA IMPORT ####
df_in <- read.csv(paste0(path_datasets, "AFSM/Stats/quotazioni_2018_2019.csv"),
                  stringsAsFactors = F, sep = ";")

load(paste0(path_datasets, "AFSM/Stats/01R_DT_lista_info_x_anno.rdata"))

df_2015 <- dataset_storici[[1]]
df_2016 <- dataset_storici[[2]]
df_2017 <- dataset_storici[[3]]

names(df_in) <- c("role", "playername", "team", "qt_i")

# costruisco dataset di addestramento
df_in_n <- df_in %>%
  left_join(df_2017 %>%
              select(playername, gf, gs, squadra_classe, am_tot_0:mean_mark_tot),
            by = "playername") %>%
  mutate(new_entry = ifelse(is.na(mean_mark_tot), 1, 0))

#### APPLICAZIONE MODELLI ####


#### SAVE ####
save(df_in_n, file = paste0(path_datasets, "AFSM/Stats/00R_df_html.rdata"))


