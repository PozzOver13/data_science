
#### SET UP #### 
require(tidyverse)

#### DATA IMPORT ####
setwd("~/Documents/GitHub/data_science/ncaa_2018")
results = read_csv("datainput/DataFiles/RegularSeasonCompactResults.csv")
seeds = read_csv("datainput/DataFiles/NCAATourneySeeds.csv")
teams = read_csv("datainput/DataFiles/Teams.csv")
coaches = read_csv("datainput/DataFiles/TeamCoaches.csv")

time_horizon = 2014:2017

# selecting just the season in which we are interested in
results_ok = results %>%
  filter(Season %in% time_horizon)

seeds_ok = seeds %>%
  filter(Season %in% time_horizon)

coaches_ok = coaches %>%
  filter(Season %in% time_horizon)

#### EDA ####
aggr_info_w = results_ok %>%
  group_by(WTeamID, Season) %>%
  summarise(W = n()) %>%
  rename(TeamID = WTeamID)

aggr_info_l = results_ok %>%
  group_by(LTeamID, Season) %>%
  summarise(L = n()) %>%
  rename(TeamID = LTeamID)

aggr_info_w = results_ok %>%
  group_by(WTeamID, Season) %>%
  summarise(ppg_scored_w = median(WScore),
            ppg_allowed%>%
  rename(TeamID = WTeamID)

aggr_info_l = results_ok %>%
  group_by(LTeamID, Season) %>%
  summarise(L = n()) %>%
  rename(TeamID = LTeamID)

aggr_info = aggr_info_w %>%
  left_join(aggr_info_l, 
            by = c("TeamID", "Season"))

