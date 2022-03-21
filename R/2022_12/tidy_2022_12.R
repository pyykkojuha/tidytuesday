# TidyTueday, Week 12, 2022 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-22/readme.md

rm(list=ls())

# libraries ----
library(ggplot2)

# data ----
babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

# color ----
# https://coolors.co/10002b-240046-3c096c-5a189a-7b2cbf-9d4edd-c77dff-e0aaff
COLOR1   <- "#10002B"
COLOR2   <- "#240046"
COLOR3   <- "#3C096C"
COLOR4   <- "#5A189A"
COLOR5   <- "#7B2CBF"
COLOR6   <- "#9D4EDD"
COLOR7   <- "#C77DFF"
COLORSKY <- "#E0AAFF"
COLORBG  <- "#000000"

# font ----
FONT1 <- "Andada Pro"
FONT2 <- "Press Start 2P"

# plot ----
TIDY <- ggplot() +
  geom_rect(aes(xmin=1880, xmax=2017, ymin=0, ymax=0.011), fill=COLORSKY, col=NA) +
  geom_area(aes(x=year, y=prop+.0044), col=NA, fill=COLOR7, data=subset(babynames, sex=="M" & name=="Bill")) +
  geom_area(aes(x=year, y=prop+.0029), col=NA, fill=COLOR6, data=subset(babynames, sex=="F" & name=="Stella")) +
  geom_area(aes(x=year, y=prop+.0024), col=NA, fill=COLOR5, data=subset(babynames, sex=="F" & name=="Dora")) +
  geom_area(aes(x=year, y=prop+.0025), col=NA, fill=COLOR4, data=subset(babynames, sex=="F" & name=="Claire")) +
  geom_area(aes(x=year, y=prop*2.51),  col=NA, fill=COLOR3, data=subset(babynames, sex=="M" & name=="Derrick")) +
  geom_area(aes(x=year, y=prop*0.31),  col=NA, fill=COLOR2, data=subset(babynames, sex=="M" & name=="Carl")) +
  geom_area(aes(x=year, y=prop*1.41),  col=NA, fill=COLOR1, data=subset(babynames, sex=="F" & name=="Kate")) +
  labs(x="BABY NAME MOUNTAINS",
       caption = "Graphic: PYYXXO      Source: babynames R package from H. Wickham      #TidyTuesday 2022/12") +
  theme_void() +
  theme(plot.caption    = element_text(size = 5.6, hjust=0.5, color=COLOR7, family=FONT2, margin=unit(c(0, 0, 0, 0),"mm")),
        axis.title.x    = element_text(size = 40,  hjust=0.5, color="ghostwhite", family=FONT1, margin=unit(c(0, 0, 4, 0),"mm")), 
        plot.background = element_rect(fill = COLORBG, colour = NA), 
        plot.margin     = unit(c(3, 7, 7, 7),"mm"))

ggsave(TIDY, file="figure/TIDY_2022_12.png", width = 15*(2/3), height = 10*(2/3), units = "in") 
