#load packages
library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
#import data
pbp_all <- readRDS("nflscrapR play by play ALL.rds")
#clean data
pbp_all_rp <- pbp_all %>%
  filter(!is_na(epa), !is_na(posteam), play_type=="no_play" | play_type=="pass" | play_type=="run") %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0),
    passer_player_name = ifelse(play_type == "no_play" & pass == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                passer_player_name),
    receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"), 
                                  str_extract(desc, "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                                  receiver_player_name),
    rusher_player_name = ifelse(play_type == "no_play" & rush == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|		      (up the middle)|(right guard)|(right tackle)|(right end)))"),
                                rusher_player_name),
    name = ifelse(!is_na(passer_player_name), passer_player_name, rusher_player_name),
    yards_gained=ifelse(play_type=="no_play",NA,yards_gained),
    play=1
  ) %>%
  filter(pass==1 | rush==1)
#start working
nineteen_pbp<-pbp_all_rp %>% filter(season==2019)
nineteen_rushing<-nineteen_pbp %>%
  filter(rush == 1, down<=4,play_type!="no_play") %>%
  group_by(rusher_player_name) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()) %>%
  arrange(desc(mean_epa)) %>%
  filter(plays>1)
#group by team
nineteen_team_rushing<-nineteen_pbp %>%
  filter(rush == 1, down<=4,play_type!="no_play") %>%
  group_by(posteam) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()) %>%
  arrange(desc(mean_epa))
nineteen_team_rushing
#create columns for qb run v other run
nineteen_passers<-unique(nineteen_pbp$passer_player_name)
nineteen_qbs<-nineteen_passers[-c(1,35,41,49,51,52,54,56,57,60,61,64,66,67,70,72,73,75,76,83,84,85,86,87,88,90,91,92,93,96,99,100,101,102,104,105,107,108,109,110,112)]
nineteen_run_pbp<-nineteen_pbp %>%
  filter(rush == 1, down<=4,play_type!="no_play")
'%notin%' <- negate('%in%')
qb_run<-nineteen_run_pbp %>%
  mutate(
    qb_run = ifelse(rusher_player_name %in% nineteen_qbs,1,0),
    other_run = ifelse(!(rusher_player_name %in% nineteen_qbs),1,0)
    )
#create plot data
qb_run_chart_data <- qb_run %>%
  group_by(posteam) %>%
  summarise(
    n_qb_run = sum(qb_run),
    n_other_run = sum(other_run),
    epa_per_qb_run = sum(epa*qb_run)/n_qb_run,
    epa_per_other_run = sum(epa*other_run)/n_other_run)
nfl_logos_df <- read_csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
qb_run_chart <- qb_run_chart_data %>% left_join(nfl_logos_df, by = c("posteam" = "team_code"))
#plot
qb_run_chart %>%
  ggplot(aes(x = epa_per_qb_run, y = epa_per_other_run)) +
  geom_image(aes(image = url), size = log(qb_run_chart$n_qb_run)/50) +
  labs(x = "QB Rush EPA/play",
       y = "Standard Rush EPA/play",
       caption = "Data from nflscrapR, @surdek10",
       title = "EPA/play on QB Runs vs Standard Runs",
       subtitle = "2019") +
  theme_bw() +
  geom_smooth(method=lm,se=FALSE) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

ggsave('qb_run_epa.png', dpi=1000)
#add league regression line