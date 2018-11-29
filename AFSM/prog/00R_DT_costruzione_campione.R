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
csv_disp <- list.files(paste0(path_datasets, "/AFSM/Stats"), "*.csv", full.names = T)
csv_disp_s <- csv_disp[4:6]
csv_disp_c <- csv_disp[2]
csv_disp_q <- csv_disp[3]

df_in <- list()
for (i in 1:length(csv_disp_s)) {
  
  t_csv_t <- read.table(csv_disp_s[i], as.is = TRUE)
  t_csv <- read.csv(text = t_csv_t[[1]], sep = "|", 
                    stringsAsFactors = F, header = T, quote = "")
  
  t_csv$anno <- str_sub(csv_disp_s[i], -13, -5)
  
  df_in <- rbind(df_in, t_csv)
}

names(df_in)[12:13] <- c("Rig_f", "Rig_s")
names(df_in) <- str_to_lower(names(df_in))

## classifica
classifica <- read.csv(csv_disp_c, sep = ";", 
                       stringsAsFactors = F, header = T) 
class_aggr <- classifica %>% 
  mutate(DR = as.numeric(DR)) %>%
  group_by(Squadra) %>%
  summarise(pos_media = mean(Pos),
            pt_media = mean(Pt),
            gf_media = mean(GF),
            gs_media = mean(GS),
            quote_media = mean(Quote)) %>%
  ungroup() %>%
  mutate(squadra_classe = cut(pt_media, breaks = quantile(pt_media, probs = seq(0,1,0.1)), labels = F, include.lowest = T))

classifica <- classifica %>%
  left_join(class_aggr %>%
              select(Squadra, squadra_classe),
            by = "Squadra")

names(classifica) <- str_to_lower(names(classifica))

## quotazioni
quotaz <- read.csv(csv_disp_q, sep = ";", 
                       stringsAsFactors = F, header = T) %>%
  select(Nome, Qt_I) %>%
  mutate(anno = "2016_2017")
names(quotaz) <- str_to_lower(names(quotaz))


#### DATA PREPARATION ####
# lower case per le squadre di club
df_in$squadra <- str_trim(str_to_lower(df_in$squadra))
classifica$squadra <- str_trim(str_to_lower(classifica$squadra))

unique(df_in$anno)
unique(classifica$anno)

# dataset di output
df_out <- df_in %>%
  left_join(classifica,
            by = c("squadra", "anno")) %>%
  rename(gf_p = gf.x,
         gs_p = gs.x,
         gf_s = gf.y,
         gs_s = gs.y) %>%
  filter(!is.na(pos)) %>%
  left_join(quotaz,
            by = c("nome", "anno"))
df_out %>%
  summarise_all(funs(sum(is.na(.))/length(.))) %>%
  gather("var", "pct_miss") %>%
  arrange(desc(pct_miss)) %>%
  ggplot(aes(reorder(var, -pct_miss), pct_miss)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw()


#### SAVE ####
write.table(df_out,  
            file = paste0(path_datasets, "AFSM/Stats/00R_DT_campione_2015_2018.csv"), 
                          sep = "|", row.names = F)
