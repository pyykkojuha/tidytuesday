# TidyTueday, Week 51, 2022 ----
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-12-20

rm(list=ls())

# libraries ----
library(ggplot2)
library(dplyr)
library(data.table)

# data ----
#weather_forecasts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/weather_forecasts.csv')
cities <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/cities.csv')
#outlook_meanings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/outlook_meanings.csv')

# color/font ----
# https://coolors.co/
FONT   <- "Epilogue"
COLOR1 <- "#9f86c0"
COLOR2 <- "#231942"

# PLOT -----
TIDY <- ggplot(subset(cities, !is.na(wind)), aes(x=elevation, y=distance_to_coast, size=avg_annual_precip^2, color=distance_to_coast*elevation), group=koppen) +
  geom_path(size=.2) +
  geom_line(size=.2) +
  geom_point() +
  scale_color_viridis_c(option = "magma") + 
  labs(caption="P Y Y X X O | Source: USA National Weather Service | #TidyTuesday 2022/51") +
  #THEME:
  theme_void() +
  theme(legend.position = "none", 
        plot.background  = element_rect(colour = COLOR1, fill = COLOR1),
        panel.background = element_rect(colour = COLOR1, fill = COLOR1),
        plot.caption = element_text(hjust = 1, colour = COLOR2, size = 6, family = FONT),
        plot.caption.position = "plot", 
        plot.margin = unit(c(2, 2, 2, 2),"mm"))

ggsave(TIDY, file="figure/TIDY_2022_51.png", width = 16*(2/3), height = 9*(2/3), units = "in") 

#end
