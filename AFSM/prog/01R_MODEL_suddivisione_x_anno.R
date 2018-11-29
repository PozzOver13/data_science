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

normalizza <- function(x) {
  min_x = min(x, na.rm = T)
  max_x = max(x, na.rm = T)
  
  (x - min_x) / (max_x - min_x)
}

path_datasets <- "~/Google Drive/Tempo_Libero/" 
path_functs <- "~/Documents/GitHub/data_science/AFSM/prog/functions/"

#### DATA IMPORT ####
df_in <- read.csv(file = paste0(path_datasets, "AFSM/Stats/00R_DT_campione_2015_2018.csv"), 
                  sep = "|", stringsAsFactors = F)

# creo dataset suddivisi per anno
un_y <- unique(df_in$year)

for (i in 1:3) {
  
  df_temp <- df_in %>%
    filter(year == un_y[i])
  
  assign(paste0("df_", un_y[i]), df_temp)
  
}

dataset_storici <- list(df_2015, df_2016, df_2017)

#### SAVE ####
save(dataset_storici, file = paste0(path_datasets, "AFSM/Stats/01R_DT_lista_info_x_anno.rdata"))
