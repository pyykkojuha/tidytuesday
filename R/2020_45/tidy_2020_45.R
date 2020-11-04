# TidyTueday, Week 45, 2020
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-11-03/readme.md

library(ggplot2)
library(extrafont)
library(plyr)

# data
ikea <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')

# plot 16:9
TIDYt <- ggplot(ikea, aes(x=width, y=height)) + 
  geom_point(aes(size=depth^2), alpha=.4) +
  labs(caption = "I K E A   F U R N I T U R E   D I M E N S I O N S    .:.    s o u r c e :  kaggle.com/ahmedkallam/ikea-sa-furniture-web-scraping  |  c o d e :  pyyxxo.fi/tt  |  #TidyTuesday 2020/45") +
  scale_y_continuous(limits=c(0, 310)) +
  theme_void() +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none",
        text = element_text(family="Alata", color="floralwhite"),
        plot.background = element_rect(fill = "darksalmon", colour = NA),
        plot.margin=unit(c(0,1,1,0),"cm") )
        
ggsave(TIDYt, file="figure/TIDY_2020_45.png", width = (2/3)*16, height = 6, units = "in") 

# plot 1:1
TIDYi <- ggplot(ikea, aes(x=width, y=height)) + 
  geom_point(aes(size=depth^2), alpha=.4) +
  labs(caption = "I K E A   F U R N I T U R E   D I M E N S I O N S
       c o d e :  pyyxxo.fi/tt  |  #TidyTuesday 2020/45
       s o u r c e :  kaggle.com/ahmedkallam/ikea-sa-furniture-web-scraping") +
  scale_y_continuous(limits=c(0, 310)) +
  theme_void() +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none",
        text = element_text(family="Alata", color="floralwhite"),
        plot.caption = element_text(size=8),
        plot.background = element_rect(fill = "darksalmon", colour = NA),
        plot.margin=unit(c(0,.5,.5,0),"cm") )

ggsave(TIDYi, file="figure/TIDY_2020_45i.png", width = 6, height = 6, units = "in") 
