# TidyTueday, Week 41, 2023
# PYYXXO
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-10-10/readme.md

library(ggplot2)
library(tidyverse)
library(MetBrewer)
library(mapproj)
library(ggtext)

# data ---- 
haunted_places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-10/haunted_places.csv')
america_map <- map_data("world", region='USA')

# colors ----
colors <- met.brewer(name="Hiroshige", n=10, type="discrete")

# plot ----
TIDY <- ggplot(america_map, aes(x=long, y=lat, group=group)) +
  # USA MAP
  geom_polygon(fill = colors[1], col = "grey20") + 
  # DENSITY MAP
  stat_density2d(data = haunted_places, aes(y = latitude, x = longitude, group = NULL, alpha = ..level..), geom = "polygon") +
  scale_alpha_continuous(limits = c(0, .001)) +
  # LIMIT TO CONTINENTAL USA
  scale_x_continuous(limits = c(-126, -64)) +
  scale_y_continuous(limits = c(24, 50)) +
  coord_map() +
  # LABS & THEME
  labs(title    = "Smog  of  Ghosts",
       subtitle = "Density of Haunted Places in the United States",
       caption  = "Graph: **PYYXXO** | Source: **Tim Renner** | #TidyTuesday 2023/41") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(    color = colors[3], fill = colors[3]),
        plot.title      = element_text(    color = "black",   size = 50, hjust = .28,  vjust = -4.5, family="Climate Crisis 2019"),
        plot.subtitle   = element_text(    color = "grey20",  size = 12, hjust = .108, vjust = -10,  family="Old Standard TT"),
        plot.caption    = element_markdown(color = "grey40",  size = 8,  hjust = .85,  vjust = 1,    family="Old Standard TT"), 
        plot.margin     = unit(c(-5, -10, 5, -5),"mm"))

ggsave(TIDY, file="figure/TIDY_2023_41.png", width = 15*(2/3), height = 10*(2/3), units = "in")
