# TidyTueday, Week 33, 2022 ----
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-08-16

rm(list=ls())

# libraries ----
library(ggplot2)
library(tidytuesdayR)

# data ----

tuesdata     <- tidytuesdayR::tt_load('2022-08-16')
characters   <- tuesdata$characters
psych_stats  <- tuesdata$psych_stats
myers_briggs <- tuesdata$myers_briggs

# font/color ----

FONT   <- "Oxygen"
COLOR1 <- "ghostwhite"
COLOR2 <- "black"

# plot ----

TIDY <- ggplot(myers_briggs, aes(x=avg_match_perc, group=myers_briggs)) +
  geom_density(alpha=.5, color=COLOR1, size=.5, bw=1.666666) +
  scale_x_continuous(limits=c(0,100)) +
  annotate(geom="text", y=0.001, x=0+0.0001, label="0%",
           hjust=0, vjust=0, size=2, family=FONT, color=COLOR1) +
  annotate(geom="text", y=0.001, x=100-0.0001, label="100%",
           hjust=1, vjust=0, size=2, family=FONT, color=COLOR1) +
  annotate(geom="text", y=0.11, x=0, 
           label="Myers Briggs Type distributions \nGraphic: P Y Y X X O \nSource: opensychometrics.org \n#TidyTuesday 2022/33",
           hjust=0, vjust=1, size=2, family=FONT, color=COLOR1) +
  theme_void() +
  theme(plot.background = element_rect(fill = COLOR2, colour = COLOR2),
        plot.margin     = unit(c(10, 30, 20, 35),"mm"))

ggsave(TIDY, file="figure/TIDY_2022_33.png", width = 16*(2/3), height = 9*(2/3), units = "in") 
