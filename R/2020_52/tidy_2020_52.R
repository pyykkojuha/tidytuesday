# TidyTueday, Week 52, 2020
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-12-22/readme.md
# SO 2 @runemandersen 4 plot inspiration

library(ggplot2)
library(plyr)
library(gridExtra)  
library(tidyverse)

# data
bigmac <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')

# plot
tidy <-  bigmac %>%
  ggplot(aes(x = date, y = dollar_price, class=name)) +
  geom_line(data=filter(bigmac, name!="Switzerland" | name!="South Africa"), color="red", alpha=.5, size=.8) +
  geom_line(data=filter(bigmac, name=="Switzerland"), color="yellow", alpha=.8, size=2) +
  geom_line(data=filter(bigmac, name=="South Africa"), color="yellow", alpha=.8, size=2) +
  theme_minimal() +
  annotate("text", x=as.Date("2000-01-01"), y=9.5, label="The Economist’s Big Mac index", 
           size=12, color="red", family="Lato", hjust=0, vjust=.5) +
  annotate("text", x=as.Date("2018-01-01"), y=7.5, label="SUISSE", 
           size=10, color="yellow", family="Lato", hjust=1, vjust=.5) +
  annotate("text", x=as.Date("2010-01-01"), y=1, label="SOUTH AFRICA", 
           size=10, color="yellow", family="Lato", hjust=1, vjust=.5) +
  annotate("text", x=as.Date("2019-12-01"), y=0.1, label="@pyyxxo", 
           size=4, color="red", family="Press Start 2P", hjust=1, vjust=0, alpha=.75) +
  ylab("USD") + xlab("Date") +
  scale_y_continuous(breaks=seq(0, 10, 1), limits=c(0,10), position = "right") +
  theme(plot.background = element_rect(fill = "gray10", colour = NA),
        panel.grid.major.y = element_line(colour = "gray40", size = 0.1), 
        panel.grid.major.x = element_line(colour = "gray40", size = 0.1),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x =   element_text(size = 14, colour = "yellow"),
        axis.text.y =   element_text(size = 16, colour = "yellow"),
        axis.title.y =   element_text(size = 14, colour = "yellow", angle=90, vjust=.5),
        text=element_text(family="Lato"),
        plot.margin=unit(c(.5, .5, .5, .5),"cm"))

ggsave(tidy, file="figure/TIDY_2020_52.png", width = 16, height = 9, units = "in", bg = "gray10")
