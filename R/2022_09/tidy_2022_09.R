# TidyTueday, Week 09, 2022 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-01/readme.md

rm(list=ls())

# libraries ----
library(ggplot2)
library(ggridges)

# data ----
stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')

# color ----
COLOR1 <- "#000000"
COLOR2 <- "#BDB2FF"
FONT1  <- "Oswald"
FONT2  <- "Press Start 2P"

# edit ----
ggplot(stations, aes(y=LATITUDE, x=LONGITUDE)) + geom_point()
stati <- subset(stations, LATITUDE > 23 & LATITUDE < 50 & LONGITUDE < -50 & LONGITUDE > -130 & FUEL_TYPE_CODE == "ELEC")
ggplot(stati, aes(y=LATITUDE, x=LONGITUDE)) + geom_point()
stati$LAT <- round(stati$LATITUDE, 0)

# plot ----

TIDY <- ggplot(stati, aes(x = LONGITUDE, y = as.factor(LAT))) +
  geom_density_ridges(fill=NA, 
                      rel_min_height = 0.02, 
                      bandwidth = 1.25,
                      size = 0.9,
                      col = COLOR1) +
  coord_cartesian(clip = "off") +
  labs(title = "ELECTRONIC STATIONS IN USA",
       subtitle = "DENSITY BY LATITUDE (EXCLUDING ALASKA & HAWAII)",
       caption = "@PYYXXO | Source: US DOT | #TidyTuesday 2022/09") +
  theme_void() +
  theme(plot.title    = element_text(size = 34, hjust=0.80, color=COLOR1, family=FONT1, margin=unit(c(0, 0, 0, 0),"mm")),
        plot.subtitle = element_text(size =  8, hjust=0.88, color=COLOR1, family=FONT1, margin=unit(c(0, 0, 0, 0),"mm")),
        plot.caption  = element_text(size =  6, hjust=0.15, color=COLOR1, family=FONT2, margin=unit(c(0, 0, 0, 0),"mm")), 
        plot.background  = element_rect(fill = COLOR2, colour = NA), 
        plot.margin = unit(c(5, 15, 5, 15),"mm"))

ggsave(TIDY, file="figure/TIDY_2022_09.png", width = 16*(2/3), height = 9*(2/3), units = "in") 
