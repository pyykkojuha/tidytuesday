# TidyTueday, Week 45, 2022 ----
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-11-08

rm(list=ls())

# libraries ----
library(ggplot2)
library(tidyverse)  # separate

# data ----
state_stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/state_stations.csv')

state_stations     <- separate(state_stations, frequency, into = c("FREQ", "TYPE"), sep=" ", remove=F)
state_stations$FREQ <- as.numeric(state_stations$FREQ) 
state_stations$TYPE[is.na(state_stations$TYPE) & state_stations$FREQ>200] <- "AM"
state_stations$TYPE[is.na(state_stations$TYPE) & state_stations$FREQ<200] <- "FM"
state_stations$TYPE[state_stations$FREQ==95.7] <- "FM"

# color/font ----
# https://coolors.co/310a31-847996-88b7b5-a7cab1-f4ecd6
BG   <- "#310A31"
FILL <- "#F4ECD6"
TEXT  <- "#88B7B5"
FONT  <- "Roboto Mono"

# data extra ----
lines <- data.frame(
  x = 88:108,
  xend = 88:108,
  y     = rep(0.019,21),
  yend  = rep(0.024,21),
  y2    = rep(0.030,21),
  yend2 = rep(0.035,21))

# plot ----
TIDY <- ggplot(subset(state_stations, TYPE=="FM")) +
  geom_density(aes(x=FREQ), bw = 0.1, col=FILL, fill=FILL) +
  #TEXT
  annotate(geom="text", y=0.027, x=88:108, 
           label=88:108,
           hjust=.5, vjust=.5, size=5, 
           family=FONT, color=TEXT) +
  annotate(geom="text", y=0.035, x=107.5, 
           label="FM",
           hjust=.5, vjust=1, size=7, 
           family=FONT, color=TEXT) +
  annotate(geom="text", y=0.015, x=98, 
           label="U.S. RADIO STATION FREQUENCIES",
           hjust=.5, vjust=1, size=12, 
           family=FONT, color=TEXT) +
  annotate(geom="text", y=0.003, x=98, 
           label="Graphic: P Y Y X X O   Data: Wikipedia / Frank Hull   #TidyTuesday 2022/45",
           hjust=.5, vjust=0, size=4.85, 
           family=FONT, color=TEXT) +
  # LINES
  geom_segment(data=lines, aes(x = x,   y = y, xend = xend,   yend = yend), col=TEXT, size=1) +
  geom_segment(data=lines, aes(x = x,   y = y2, xend = xend,   yend = yend2), col=TEXT, size=1) +
  #THEME
  theme_void() +
  theme(plot.background = element_rect(fill = BG, colour = BG), 
        plot.margin     = unit(c(4, 4, 4, 4),"mm"))

ggsave(TIDY, file="figure/TIDY_2022_45.png", width = 15*(2/3), height = 10*(2/3), units = "in") 
