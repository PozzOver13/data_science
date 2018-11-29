########################################################
# PROGRAM:  MODEL - train 1
# DATE:     2018-08-06
# NOTE:     Portieri
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
load(paste0(path_datasets, "AFSM/Stats/01R_DT_lista_info_x_anno.rdata"))

df_2015 <- dataset_storici[[1]]
df_2016 <- dataset_storici[[2]]
df_2017 <- dataset_storici[[3]]

# costruisco dataset di addestramento
df_2016_tr <- df_2016 %>%
  select(playername, role, team, 
         year, quote, squadra_classe, 
         status, pg_tot, mean_mark_tot, gs, gf) %>%
  rename(pg_tot_target = pg_tot,
         mean_mark_tot_target = mean_mark_tot) %>%
  left_join(df_2015 %>%
              select(playername, am_tot_0:mean_mark_tot),
            by = "playername") %>%
  mutate(new_entry = ifelse(is.na(mean_mark_tot), 1, 0))

df_2017_ts <- df_2017 %>%
  select(playername, role, team, 
         year, quote, squadra_classe, 
         status, pg_tot, mean_mark_tot, gs, gf) %>%
  rename(pg_tot_target = pg_tot,
         mean_mark_tot_target = mean_mark_tot) %>%
  left_join(df_2016 %>%
              select(playername, am_tot_0:mean_mark_tot),
            by = "playername") %>%
  mutate(new_entry = ifelse(is.na(mean_mark_tot), 1, 0))

# controllo numero di casi senza info anno precedente
round(table(df_2016_tr$new_entry) / dim(df_2016_tr)[1], 3)
round(table(df_2016_tr$status) / dim(df_2016_tr)[1], 3)

df_2016_tr_cl <- df_2016_tr %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

df_2017_tr_cl <- df_2017_ts %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

# ipotesi segmentazione
# P, D, C, A
# new_entry vs old

## PORTIERI
df_2016_P = df_2016_tr_cl %>%
  filter(role == "P")

df_2017_P = df_2017_tr_cl %>%
  filter(role == "P")

names(df_2016_P)

df_2016_P_nrm <- df_2016_P %>%
  select(starts_with("clsh_"),
         squadra_classe,
         starts_with("gs"),
         starts_with("mean_mark_"),
         pg_tot,
         starts_with("rp_"))

df_2017_P_nrm <- df_2017_P %>%
  select(starts_with("clsh_"),
         squadra_classe,
         starts_with("gs"),
         starts_with("mean_mark_"),
         pg_tot,
         starts_with("rp_"))

# normalizzazione
mean <- apply(df_2016_P_nrm, 2, mean)
std <- apply(df_2016_P_nrm, 2, sd)
train_data_1 <- scale(df_2016_P_nrm, center = mean, scale = std) %>% as.data.frame()
test_data_1 <- scale(df_2017_P_nrm, center = mean, scale = std) %>% as.data.frame()


train_p <- df_2016_P %>% 
  select(status, new_entry) %>%
  bind_cols(train_data_1)

test_p <- df_2017_P %>% 
  select(status, new_entry) %>%
  bind_cols(test_data_1)


# correlazione
corr_p <- cor(df_2016_P_nrm)

corrplot(corr_p, method = "square", type = "upper")

y <- train_p$status
# XGBoost Classifier Training
xgb <- xgboost(data = data.matrix(train_p[,-1]),
               label = y,
               eta = 0.02,
               max_depth = 3, 
               nround=10000, 
               subsample = 0.5, 
               colsample_bytree = 0.5, 
               seed = 13, 
               eval_metric = "logloss", 
               objective = "binary:logistic",
               nthread = 3)

xgb.importance(train_p[,-1] %>% names(),
               xgb) %>%
  ggplot(aes(reorder(Feature, -Gain), Gain)) +
  geom_bar(stat = "identity", 
           alpha = 0.5, 
           fill = "lightseagreen") +
  coord_flip() +
  labs(title = "XGB - importance",
       x = "") +
  theme_bw()

tr_y_pred_prob <- predict(xgb, data.matrix(train_p[,-1]))

data.frame(nm = df_2016_P$playername,
           sq = df_2016_P$squadra_classe,
           pr = tr_y_pred_prob, stringsAsFactors = F) %>%
  ggplot(aes(reorder(nm, -pr), pr, fill = sq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

tr_y_pred <- as.numeric(tr_y_pred_prob > 0.08)

real_1 <- train_p$status
table(real_1, tr_y_pred)

prec_zero(real_1, tr_y_pred)
prec_one(real_1, tr_y_pred)
recl_zero(real_1, tr_y_pred)
recl_one(real_1, tr_y_pred)
accrcy(real_1, tr_y_pred)

df_2016_P$playername[as.numeric(rownames(train_p[train_p$status == 1,]))]
df_2016_P$playername[tr_y_pred == 1]

#### TEST ####
pred_test <- predict(xgb, data.matrix(test_p[,-1]))

pred_2 <- ifelse(pred_test > 0.08, 1, 0)
real_2 <- test_p$status
table(real_2, pred_2)

# test performance
prec_zero(real_2, pred_2)
prec_one(real_2, pred_2)
recl_zero(real_2, pred_2)
recl_one(real_2, pred_2)
accrcy(real_2, pred_2)


df_2017_P$playername[as.numeric(rownames(test_p[test_p$status == 1,]))]
df_2017_P$playername[pred_2 == 1]

#### SAVE ####
xgb_p <- xgb
cutoff_p <- 0.08
mean_var_list_p <- list(mean, std)

output_p <- list(xgb_p, cutoff_p, mean_var_list_p)
save(output_p, file = "Google Drive/Tempo_Libero/AFSM/Output_R/02R_output_portieri.rdata")



















