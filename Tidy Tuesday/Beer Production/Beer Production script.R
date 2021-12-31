library(tidyverse)
library(reshape2)

brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

materials <- brewing_materials %>%
  select(year, type, month_current) %>%
  group_by(type, year) %>%
  summarise(total = sum(month_current)) %>%
  mutate(types = ifelse(type %in% c("Barley and barley products", "Corn and corn products", "Rice and rice products", "Wheat and wheat products"), "Barley, Corn, Rice, Wheat", type)) %>%
  filter(!(types %in% c("Total Grain products", "Total Non-Grain products", "Total Used"))) %>%
  mutate(types = factor(types, levels = c("Other", "Hops (used as extracts)", "Hops (dry)", "Sugar and syrups", "Barley, Corn, Rice, Wheat", "Malt and malt products")))

materials %>%
  filter(year <= 2015) %>%
  ggplot() +
  geom_bar(aes(x = year, y = total, fill = types), stat = 'identity',  position = "fill") +
  scale_x_continuous(breaks = c(2009:2018), limits = c(2009,2018)) +
  theme_bw() +
  theme(
    legend.position = c(0.85,0.5),
    legend.text = element_text(size = 8.5),
    panel.grid = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(x = "", y = "Percent Of Ingredients")

production <- beer_taxed %>%
  filter(type == "Production", month == 12)

production %>%
  ggplot() +
  geom_line(aes(x=year, y=(ytd_current/100000000))) +
  scale_x_continuous(breaks = c(2008:2019)) +
  scale_y_continuous(limits = c(1,2))



craft_one <- brewer_size %>%
  filter(brewer_size != "Total") %>%
  mutate(style = ifelse(brewer_size == "6,000,001 Barrels and Over", "Mass", "Craft")) %>%
  group_by(year, style) %>%
  summarise(barrels = sum(total_barrels))

craft_two <- craft_one %>%
  group_by(year) %>%
  summarise(total = sum(barrels))

craft <- craft_one %>%
  left_join(craft_two, by = "year") %>%
  filter(year != 2019)

craft %>%
  ggplot() +
  geom_line(aes(x=year, y=(barrels/1000000), color = style)) +
  geom_text(aes(x = 2017, y = 67, label = "Craft Beer")) +
  geom_text(aes(x = 2017, y = 150, label = "Mass Produced Beer")) +
  geom_text(aes(x = 2017, y = 200, label = "Total")) +
  scale_x_continuous(breaks = c(2009:2018), limits = c(2009,2018)) +
  scale_y_continuous(limits = c(0,250)) +
  geom_line(aes(x = year, y = (total/1000000))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") +
  labs(title = "United States Beer Production Since 2009",
       y = "Barrels (Millions)")
  

states <- beer_states %>%
  filter(year == 2019) %>%
  group_by(state) %>%
  summarise(barrels = sum(barrels)) %>%
  filter(state != "total")

head(states[order(-states$barrels),], 10)

growth <- beer_states %>%
  filter(year == 2016 | year == 2019) %>%
  group_by(state, year) %>%
  summarise(barrels = sum(barrels)) %>%
  filter(state != "total") %>%
  dcast(state ~ year) %>%
  mutate(delta = growth$`2019` / growth$`2016`)

head(growth[order(-growth$delta),],10)
