# TidyTueday, Week 03, 2022 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-18/readme.md

rm(list=ls())

# libraries ----
library(ggplot2)
library(ggthemes)
library(extrafont)

canva_palettes[90]
COLORS <- c("#882426" ,"#cdbea7" ,"#323030", "#c29545")

# data ----
chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

# edit ----
chocolate$cocoa <- lapply(chocolate$cocoa_percent, function(x) {gsub("%", "", x)})
chocolate$cocoa <- as.numeric(chocolate$cocoa)

# plot ----
TIDY <- ggplot(chocolate) +
  geom_text(aes(x=60, y=.024), label="60%", size=16.0, family="Fascinate", hjust=.5, vjust=0, color=COLORS[2]) +
  geom_text(aes(x=70, y=.349), label="70%", size=24.0, family="Fascinate", hjust=.5, vjust=0, color=COLORS[2]) +
  geom_text(aes(x=80, y=.038), label="80%", size=19.0, family="Fascinate", hjust=.5, vjust=0, color=COLORS[2]) +
  scale_y_continuous(limits=c(0,.39)) +
  geom_density(aes(x=cocoa+.1), bw=.5, size=3, color=COLORS[2]) +
  geom_density(aes(x=cocoa), bw=.5, size=3, color=COLORS[1]) +
  # TITLES
  labs(title="COCOA percent amounts of plain dark chocolate bar rated",
       x="",
       y="",
       caption="@PYYXXO | data source: Flavors of CACAO | #TidyTuesday 2022/03") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = COLORS[3], colour = NA),
    plot.title    = element_text(size = 36, hjust=.5, colour = COLORS[4], margin = unit(c(0, 0, 5, 0), "mm"), family="Fascinate"),
    plot.caption  = element_text(size = 20, hjust=.5, colour = COLORS[4], margin = unit(c(5, 0, 0, 0), "mm"), family="Fascinate"), 
    plot.margin = unit(c(0.5, .1, .5, .1),"cm"))

ggsave(TIDY, file="figure/TIDY_2022_03.png", width = 16, height = 9, units = "in", bg=COLORS[3]) 
