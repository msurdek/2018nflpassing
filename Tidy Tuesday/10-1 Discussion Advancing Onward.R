#Read In Data
lifetime_earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/lifetime_earn.csv')
student_debt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')
retirement <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/retirement.csv')
home_owner <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/home_owner.csv')
race_wealth <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/race_wealth.csv')
income_time <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_time.csv')
income_limits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_limits.csv')
income_aggregate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_aggregate.csv')
income_distribution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')
income_mean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_mean.csv')

#Load packages
library(tidyverse)

#Data
colnames(income_time)[2] <- "Percentile"

#Viz
plot <- ggplot(income_time, aes(x = year, y = income_family)) +
  geom_line(aes(color = Percentile)) +
  geom_point(aes(color = Percentile)) +
  geom_text(aes(label = ifelse(year == "1963",round(income_family),"")), hjust=0.5, vjust = -0.7, size = 3) +
  geom_text(aes(label = ifelse(year == "2016", round(income_family),"")), hjust=0.5, vjust = -0.7, size = 3) +
  expand_limits(y=c(0,200000)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  ylab("Average Family Income") +
  xlab("Year") +
  ggtitle("Income Inequality Over Time")

plot
