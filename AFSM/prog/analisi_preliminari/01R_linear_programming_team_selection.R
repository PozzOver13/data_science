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
require(corrplot)
require(gridExtra)
library(lpSolve)

path_datasets <- "~/Google Drive/Tempo_Libero/" 
path_functs <- "~/Documents/GitHub/data_science/AFSM/prog/functions/"

source(paste0(path_functs, "seleziona_squadra_ottimizzata.R"))

#### DATA IMPORT ####
df_in <- read.csv(file = paste0(path_datasets, "AFSM/Stats/00R_DT_campione_2015_2018.csv"), 
                  sep = "|", stringsAsFactors = F)

df <- df_in %>%
  filter(anno == "2016_2017", !is.na(qt_i)) %>%
  mutate(qt_i = as.numeric(str_replace_all(qt_i, ",", ".")))

#Create the constraints
num_gk = 1
num_def = 6
num_mid = 6
num_fwd = 3
max_cost = 180


# Create vectors to constrain by position
df$Goalkeeper = ifelse(df$r == "P", 1, 0)
df$Defender = ifelse(df$r == "D", 1, 0)
df$Midfielder = ifelse(df$r == "C", 1, 0)
df$Forward = ifelse(df$r == "A", 1, 0)
# Create vector to constrain by max number of players allowed per team
team_constraint = unlist(lapply(unique(df$squadra), function(x, df){
  ifelse(df$squadra==x, 1, 0)
}, df=df))
# next we need the constraint directions
const_dir <- c("=", "=", "=", "=", rep("<=", 21))

# The vector to optimize against
objective = df$pg

# Put the complete matrix together
const_mat = matrix(c(df$Goalkeeper, df$Defender, df$Midfielder, df$Forward,
                     df$qt_i, team_constraint),
                   nrow=(5 + length(unique(df$squadra))),
                   byrow=TRUE)

const_rhs = c(num_gk, num_def, num_mid, num_fwd, max_cost, rep(2, 20))

dim(const_mat)
length(objective)


# And solve the linear system
x = lp ("max", objective, const_mat, const_dir, const_rhs, all.bin=TRUE, all.int=TRUE)


# team resulting
res_team <- df[which(x$solution==1),] %>%
  arrange(desc(Goalkeeper), desc(Defender), 
          desc(Midfielder), desc(Forward), 
          desc(qt_i))

sum(res_team$qt_i)

normalizza <- function(x) {
  min_x = min(x, na.rm = T)
  max_x = max(x, na.rm = T)
  
  (x - min_x) / (max_x - min_x)
}

df_test <- df_in %>%
  filter(anno == "2016_2017", !is.na(qt_i)) %>%
  mutate(qt_i = as.numeric(str_replace_all(qt_i, ",", ".")),
         score_pg = normalizza(pg),
         score_mf = normalizza(mf),
         score_gf_p = normalizza(gf_p),
         score_pt_sq = normalizza(gs_s),
         random_st = rnorm(length(pg)),
         score_random = normalizza(random_st),
         score_intg = 0.4*score_pg + 0.3*score_mf + 0.2*score_pt_sq + 0.1*score_random)

team_out <- seleziona_squadra_opt(df_test, 
                                  num_gk = 1, 
                                  num_def = 8, 
                                  num_mid = 6, 
                                  num_fwd = 4, 
                                  max_cost = 245, 
                                  opt_score = "score_intg")

#### SELEZIONA TITOLARI ####




team_out_11 <- seleziona_squadra_opt(team_out, 
                                  num_gk = 1, 
                                  num_def = 4, 
                                  num_mid = 3, 
                                  num_fwd = 3, 
                                  max_cost = 2450, 
                                  opt_score = "score_intg")
