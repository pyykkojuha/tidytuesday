# TidyTueday, Week 32, 2022 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-08-09/readme.md

rm(list=ls())

# libraries ----
library(ggplot2)
library(ggridges)

# data ----
wheels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')

# edit data ----
wheels$height_m <- wheels$height * 0.3048
wheels$year <- substr(wheels$opened,1,4)
wheels$year10 <- floor(as.numeric(wheels$year)/10)*10
wheels$year10[wheels$year10<1990] <- 1980 
wheels$year10[is.na(wheels$year10)] <- 2020
wheels$decade <- NA
wheels$decade[wheels$year10==1980] <- "1980s"
wheels$decade[wheels$year10==1990] <- "1990s"
wheels$decade[wheels$year10==2000] <- "2000s"
wheels$decade[wheels$year10==2010] <- "2010s"

# color/font ----
# Colors by COOLORS: https://coolors.co/palette/16697a-489fb5-82c0cc-ede7e3-ffa62b
ORANGE <- "#ffa62b"
BLUE   <- "#489fb5"
WHITE  <- "#ede7e3"
FONT   <- "Spectral"

# plot ----
TIDY <- ggplot(subset(wheels, year10 != 2020 & !is.na(height_m))) +
  geom_density_ridges(aes(x=height_m, y=decade), 
                      rel_min_height = 0.01, size=1.25,
                      fill=BLUE, color=ORANGE, bandwidth=10) +
  scale_x_continuous(limits=c(30,250), breaks=seq(50,200,50), position = "top",
                     labels=c("| 50 m", "| 100 m", "| 150 m", "| 200 m")) +
  coord_cartesian(clip = "off") +
  labs(subtitle="Ferris wheel heights by decade",
       title="Graphic: P Y Y X X O | Source: ferriswheels package by Emil Hvitfeldt | #TidyTuesday 2022/32") +
  #THEME
  theme_void() +  # comment off to fix axis position
  theme(plot.background = element_rect(fill = BLUE, colour = BLUE), 
        legend.position = "none",
        axis.text.x = element_text(size = 14, angle=0,  vjust=1.1, hjust=.047, colour=WHITE, family=FONT),
        axis.text.y = element_text(size = 32, angle=0,  vjust=-.5,  hjust=2, colour=WHITE, family=FONT),
        axis.title = element_blank(),
        plot.subtitle = element_text(size = 40, hjust=.56,   color = ORANGE,  margin = unit(c(0, 0, 2, 0), "mm"), family=FONT),
        plot.title    = element_text(size = 10, hjust=.79, color = WHITE,  margin = unit(c(5, 0, 3, 0), "mm"), family=FONT),
        plot.margin     = unit(c(1, 0, 0, 15),"mm"))

ggsave(TIDY, file="figure/TIDY_2022_32.png", width = 16*(2/3), height = 9*(2/3), units = "in") 
