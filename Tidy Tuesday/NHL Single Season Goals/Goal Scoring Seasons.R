
#load libraries

library(tidyverse)
library(ggimage)

#import data
#season averages from hockey-reference.com/leagues/stats.html

season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

season_averages<-readr::read_csv('/Users/msurd/Documents/R/Tidy Tuesday/NHL Mar 3 2020/average_goals.csv')
  
#filter the top goal scorers in NHL history
#combine data frames and create metrics to be plotted
#filter for 50 goal seasons

top_ten_seasons<-season_goals%>%
  filter(league=="NHL",rank<=10)%>%
  select(player,rank,season,goals,season_games,headshot)%>%
  left_join(season_averages,by="season")%>%
  mutate(adjusted_goals=goals*(mean(season_average)/season_average),
         goals_per_game=goals/season_games)%>%
  filter(goals>50)
  
#find slope of regression line in order to plot dashed tiers

slope<-coef(lm(adjusted_goals ~ goals_per_game, data = top_ten_seasons))

#scatterplot
#title and axis labels
#insert headshot, name, and season for the best individual seasons
#insert dashed tiers and regression line

plot<-ggplot(top_ten_seasons,
       aes(x=adjusted_goals,y=goals_per_game)) +
       labs(title="NHL Single Season Goal Scoring Tiers",
            subtitle="Top 10 Goal Scorers Of All Time, 50 Goal Seasons",
            x="Total Goals Adjusted By Season",
            y="Goals Per Game") +
       geom_point() +
       geom_image(aes(image=headshot),size=0.03,
                  data=subset(top_ten_seasons,
                              goals_per_game>0.79|adjusted_goals>61.7|
                                goals_per_game>0.76&adjusted_goals>56)) +
       geom_text(aes(label=player),size=2.5,
                 data=subset(top_ten_seasons,
                             goals_per_game>0.79|adjusted_goals>61.7|
                               goals_per_game>0.76&adjusted_goals>56),
                 nudge_y=-0.0175) +
       geom_text(aes(label=season),size=2.5,
            data=subset(top_ten_seasons,
                        goals_per_game>0.79|adjusted_goals>61.7|
                          goals_per_game>0.76&adjusted_goals>56),
            nudge_y=-0.0275) +
       theme_classic() +
       geom_abline(slope = -1/slope[2], intercept = 1.6, lty=3) +
       geom_abline(slope = -1/slope[2], intercept = 1.8, lty=3) +
       geom_abline(slope = -1/slope[2], intercept = 2.0, lty=3) +
       geom_abline(slope = -1/slope[2], intercept = 2.2, lty=3) +
       geom_abline(slope = -1/slope[2], intercept = 2.4, lty=3) +
       geom_abline(slope = -1/slope[2], intercept = 2.6, lty=3) +
       geom_abline(slope = -1/slope[2], intercept = 2.8, lty=3) +
       geom_smooth(method="lm",se=FALSE)

plot
