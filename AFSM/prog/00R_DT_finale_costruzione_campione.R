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

path_datasets <- "~/Google Drive/Tempo_Libero/" 
path_functs <- "~/Documents/GitHub/data_science/AFSM/prog/functions/"

#### DATA IMPORT ####
## stats
csv_disp <- list.files(paste0(path_datasets, "/AFSM/VotiCSV"), "*.csv", full.names = T)
classifica_list <- list.files(paste0(path_datasets, "/AFSM/Stats"), "*.csv", full.names = T)[2]

df_in <- list()
for (i in 1:length(csv_disp)) {
  
  t_csv <- read.csv(csv_disp[[i]], sep = "|", 
                    stringsAsFactors = F, header = T, quote = "")
  
  df_in <- rbind(df_in, t_csv)
}

#### DATA CLEANING ####
df_in_c <- df_in %>% 
  # filter(!is.na(Year)) %>%
  mutate(Mark = as.numeric(str_replace_all(Mark, "\\*", "")),
         Clean_sheet = ifelse(Role == "P" & GS == 0, 1, 0),
         DBL = ifelse(GF > 1, 1, 0)) %>%
  group_by(Team, Role, PlayerName, Home, Year) %>%
  summarise(Mean_Mark = mean(Mark),
            Mark_tot = sum(Mark),
            PG_tot = n(),
            GF_tot = sum(GF),
            GS_tot = sum(GS),
            RP_tot = sum(RP),
            RS_tot = sum(RS),
            RF_tot = sum(RF),
            AM_tot = sum(AM),
            ES_tot = sum(ES),
            AS_tot = sum(AS),
            CLSH_tot = sum(Clean_sheet),
            DBL_tot = sum(DBL)) %>%
  ungroup() %>%
  group_by(Team, Role, PlayerName, Year) %>%
  mutate(Ncheck = n()) %>%
  filter(Ncheck > 1) %>%
  ungroup()

df_in_c_f <- df_in_c %>%
  select(-Ncheck) %>%
  gather(key = "vars", value = "value", -Home, -PlayerName, -Role, -Team, -Year) %>%
  mutate(new_var = paste(vars, Home, sep = "_")) %>%
  select(new_var, value, PlayerName, Role, Team, Year)

df_in_c_f_a <- df_in_c_f %>%
  spread(key = new_var, value = value) %>%
  mutate(PG_tot = PG_tot_0 + PG_tot_1,
         GF_tot = GF_tot_0 + GF_tot_1,
         GS_tot = GS_tot_0 + GS_tot_1,
         RP_tot = RP_tot_0 + RP_tot_1,
         RS_tot = RS_tot_0 + RS_tot_1,
         RF_tot = RF_tot_0 + RF_tot_1,
         AM_tot = AM_tot_0 + AM_tot_1,
         ES_tot = ES_tot_0 + ES_tot_1,
         AS_tot = AS_tot_0 + AS_tot_1,
         CLSH_tot = CLSH_tot_0 + CLSH_tot_1,
         DBL_tot = DBL_tot_0 + DBL_tot_1,
         Mark_tot = Mark_tot_0 + Mark_tot_1,
         Mean_Mark_tot = (Mean_Mark_0 + Mean_Mark_1)/2) %>%
  group_by(Role) %>%
  mutate(benchmark_v = quantile(Mean_Mark_tot, probs = 0.8),
         benchmark_pg = quantile(PG_tot, probs = 0.8),
         benchmark_gf = quantile(GF_tot, probs = 0.8),
         benchmark_gs = quantile(GS_tot, probs = 0.75)) %>%
  ungroup() %>%
  mutate(status = ifelse(Role %in% c("D", "C"),
                         ifelse(PG_tot > benchmark_pg & 
                                  Mean_Mark_tot > benchmark_v, 1, 0),
                         ifelse(Role == "A", 
                                ifelse(PG_tot > benchmark_pg & 
                                         GF_tot > benchmark_gf, 1, 0),
                                ifelse(PG_tot > benchmark_pg & 
                                         GS_tot < benchmark_gs & GS_tot > 10, 1, 0))),
         molt_goal = ifelse(Role == "A", 3.0,
                            ifelse(Role == "C", 3.5,
                                   ifelse(Role == "D", 4.0, 10))),
         voti_tot = Mark_tot + GF_tot*molt_goal + RF_tot*3.0 + RP_tot + AS_tot -
           (GS_tot + AM_tot*0.5 + ES_tot)) 

table(df_in_c_f_a$status) / dim(df_in_c_f_a)[1]

## NOTE: 
# 1 - non sono stati filtrati i giocatori anche se chiaro che non andrebbero considerati quelli che avevano un valore iniziale molto basso. (Non li avremmo mai considerati)
# 2 - le medie per ruolo potrebbero essere affinate ma per iniziare possono funzionare al fine di segnalare cosa considerare e cosa evitare


## classifica
classifica <- read.csv(classifica_list, sep = ";", 
                       stringsAsFactors = F, header = T, na = "") 
class_aggr <- classifica %>% 
  mutate(DR = as.numeric(DR)) %>%
  group_by(Squadra) %>%
  summarise(pos_media = mean(Pos),
            pt_media = mean(Pt),
            gf_media = mean(GF),
            gs_media = mean(GS),
            quote_media = mean(Quote)) %>%
  ungroup() %>%
  mutate(squadra_classe = cut(pt_media, 
                              breaks = quantile(pt_media, 
                                                probs = seq(0,1,0.1)), 
                              labels = F, include.lowest = T))

classifica <- classifica %>%
  left_join(class_aggr %>%
              select(Squadra, squadra_classe),
            by = "Squadra") %>%
  rename(Year = Anno,
         Team = Squadra) %>%
  mutate(Year = as.numeric(str_sub(Year, 1, 4)))

names(classifica) <- str_to_lower(names(classifica))
names(df_in_c_f_a) <- str_to_lower(names(df_in_c_f_a))



# lower case per le squadre di club
df_in_c_f_a$team <- str_trim(str_to_lower(df_in_c_f_a$team))
classifica$team <- str_trim(str_to_lower(classifica$team))

unique(df_in_c_f_a$year)
unique(classifica$year)

# dataset di output
df_out <- df_in_c_f_a %>%
  left_join(classifica,
            by = c("team", "year")) %>%
  filter(!is.na(pos))

#### SAVE ####
write.table(df_out,  
            file = paste0(path_datasets, "AFSM/Stats/00R_DT_campione_2015_2018.csv"), 
            sep = "|", row.names = F)

# table(df_out$year[df_out$role == "P"], df_out$status[df_out$role == "P"])
