# TidyTueday, Week 10, 2021 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-03-02/readme.md

library(ggplot2)
library(plyr)
library(ggtext)  
library(gridExtra)  
library(extrafont)

# data ----

youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')

cola <- subset(youtube, brand=="Coca-Cola" | brand=="Pepsi")

cola2 <- ddply(cola, .(brand), summarise, 
               funny = sum(funny), 
               show_product_quickly = sum(show_product_quickly), 
               patriotic = sum(patriotic), 
               celebrity = sum(celebrity), 
               danger = sum(danger), 
               animals = sum(animals), 
               use_sex = sum(use_sex))

FONT <- "Nokia Cellphone FC Small"

topics <- c("Was it trying to be funny?_________________",
            "Did it show the product right away?_________",
            "Was it patriotic?_________________________",
            "Did it feature a celebrity?_________________",
            "Did it involve danger?____________________",
            "Did it include animals?____________________",
            "Did it use sex to sell its product?____________")

TIDY <- ggplot(cola2) +
  annotate(geom="text", y=9.1, x=0, label="SUPER BOWL ADS 2001-2020", alpha=.9, hjust=0, vjust=.5, 
           family=FONT, size=21.1, color="black") +
  annotate(geom="text", y=8, x=21, label="Coca-Cola", alpha=1, hjust=1, vjust=.0, 
           family=FONT, size=17, color="#F40009") +
  annotate(geom="text", y=1:7, x=21, label=t(cola2[1,2:8]), alpha=1, hjust=1, vjust=.0, 
           family=FONT, size=24, color="#F40009") +
  annotate(geom="text", y=8, x=25, label="Pepsi", alpha=1, hjust=1, vjust=.0, 
           family=FONT, size=17, color="#004B93") +
  annotate(geom="text", y=1:7, x=25, label=t(cola2[2,2:8]), alpha=1, hjust=1, vjust=.0, 
           family=FONT, size=24, color="#004B93") +
  annotate(geom="text", y=1:7, x=0, label=topics, alpha=.5, hjust=0, vjust=.0, 
           family=FONT, size=12, color="black") +
  scale_x_continuous(limits=c(0,25)) +
  labs(caption="Source: superbowl-ads.com / fivethirtyeight | Graphic: PYYXXO | Code: pyyxxo.fi/tt | #TidyTuesday 2021/10") +
  theme_void() +
  theme(plot.background = element_rect(fill = "floralwhite", colour = NA), 
        plot.caption  = element_text(size = 14, hjust=.5, color="deeppink", family="DIN Condensed Bold", margin = unit(c(10, 0, 0, 0), "mm")), 
        plot.margin = unit(c(1, 1, 1, 1),"cm") ) 

ggsave(TIDY, file="figure/TIDY_2021_10.png", width = 16, height = 9, units = "in", bg = "floralwhite")
