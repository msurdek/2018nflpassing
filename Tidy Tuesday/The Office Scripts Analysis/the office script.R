library(schrute)
library(tidytext)
library(tidyverse)
library(RColorBrewer)
library(formattable)

mydata <- schrute::theoffice

token.mydata <- mydata %>%
  tidytext::unnest_tokens(word, text)

count.token.mydata <- token.mydata %>%
  group_by(index) %>%
  mutate(words = table(index)) %>%
  distinct(index, .keep_all = TRUE)

words.per.line <- count.token.mydata %>%
  group_by(character) %>%
  summarise(mean = mean(words),
            count = n()) %>%
  filter(count > 600)

table.data <- words.per.line[,1:2]
table.data <- table.data[order(-table.data$mean),]
table.data$mean <- round(table.data$mean, digits = 1)
table.data$Rank <- order(table.data$mean, decreasing = TRUE)
table.data <- data.frame(table.data$Rank, table.data$character, table.data$mean)
names(table.data)[1] <- "Rank"
names(table.data)[2] <- "Character"
names(table.data)[3] <- "Words Per Line"


chart.characters <- c("Phyllis","Oscar","Pam",
                      "Jim","Dwight","Michael")

chart.data <- count.token.mydata %>%
  filter(character %in% chart.characters)

chart.data$character <- factor(chart.data$character,
                               levels = chart.characters)

chart.data %>%
  ggplot(aes(x = character, y = words, color = character)) +
  geom_jitter(width = 0.35, alpha = 0.6) +
  stat_summary(fun.y = "mean", color = "black", geom = "point") +
  theme_void() +
  coord_flip() +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Which Characters Have Longer Lines In The Office?",
       caption = "Data From {schrute} | #TidyTuesday | @surdek10")
  

brewer.pal(n = 6, name = 'Set2')

formattable(table.data, list(
  Character = formatter(
    "span", style = x ~ ifelse(x == "Michael", style(color = "#FFD92F", font.weight = "bold"),
                        ifelse(x == "Dwight", style(color = "#A6D854", font.weight = "bold"),
                        ifelse(x == "Jim", style(color = "#E78AC3", font.weight = "bold"),
                        ifelse(x == "Pam", style(color = "#8DA0CB", font.weight = "bold"),
                        ifelse(x == "Oscar", style(color = "#FC8D62", font.weight = "bold"),
                        ifelse(x == "Phyllis", style(color = "#66C2A5", font.weight = "bold"), NA)))))))))
