# TidyTueday, Week 19, 2021 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-04/readme.md

rm(list=ls())

library(ggplot2)
library(extrafont)
library(sf)
library(sp)
library(spData)           # world
library(ggmap)            # get_map

# data ----
water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')

# edit ----
mali <- subset(water, country_name=="Mali" & water_source=="Borehole")
world$selected <- "X"
world$selected[world$name_long=="Mali"] <-"M"

# plot ----
FONT <- "Montserrat Medium"
GREEN <- "#009639"
YELLOW <- "#FFD100"
RED <- "#EF3340"

TIDY <- ggplot(data = world, aes(fill = selected)) +
  geom_sf(lwd = 0) +  
  scale_fill_manual(values=c(RED, YELLOW)) +
  annotate(geom="text", y=12.65, x=-8,     hjust=.5, vjust=.5, angle=0, alpha=1, family=FONT, size=4, color=GREEN, label="+") +
  annotate(geom="text", y=12.65, x=-7.9,     hjust=0, vjust=.5, angle=0, alpha=1, family=FONT, size=5, color=GREEN, label="BAMAKO") +
  geom_point(data = mali, aes(x = lon_deg, y = lat_deg), size = 1, alpha=1, fill = "black") +
  coord_sf(xlim = c(-12.6-5, 4.7+5), ylim = c(9.7, 25.2), expand = FALSE) +
  annotate(geom="text", y=20,   x=-17.5, hjust=0, vjust=.5, angle=0, alpha=1, family=FONT, size=14.2, color=GREEN, label="BOREHOLES IN MALI") +
  annotate(geom="text", y=19,   x=-17.5, hjust=0, vjust=.5, angle=0, alpha=1, family=FONT, size=11.2, color=GREEN, label="water point data exchange") +
  annotate(geom="text", y=10.5, x=9,     hjust=1, vjust=.5, angle=0, alpha=1, family=FONT, size=5,    color=GREEN, label="source: wpdx | graphic: pyyxxo | #tidytuesday 2021/19") +
  theme_void() +
  theme(legend.position="none",
        plot.background = element_rect(fill = YELLOW, colour = NA), 
        plot.margin  = unit(c(.1, .1, .1, .1),"cm"))

ggsave(TIDY, file="figure/TIDY_2021_19.png", width = 16, height = 9, units = "in", bg=YELLOW)
