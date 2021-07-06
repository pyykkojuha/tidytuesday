# TidyTueday, Week 28, 2021 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-07-06/readme.md

rm(list=ls())

library(ggplot2)
library(extrafont)
library(dplyr)
library(plyr)

# data ----
holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')

table(holidays$year)

years <- ddply(holidays, .(year_of_event), summarise, y = length(year))
years <- subset(years, !is.na(year_of_event))

# plot ---
TIDY <- ggplot(data = subset(holidays, !is.na(year))) +
  geom_bar(aes(x = year), fill="indianred", color="black", width=12, alpha=.8) +
  annotate(geom="text", y=14, x=1300, 
           label="INDEPENDENCE YEARS", 
           alpha=.9, hjust=0, vjust=1, 
           family="Monoton", size=24, color="black") +
  annotate(geom="text", y=14.2, x=1300, 
           label="Source: Wikipedia | Graphic: PYYXXO | Code: pyyxxo.fi/tt | #TidyTuesday 2021/28", 
           alpha=.5, hjust=0, vjust=0, 
           family="Source Code Pro", size=4, color="black") +
  annotate(geom="text", 
           y=c(2,2,10,9,18,16), x=c(1291,1523,1821,1918,1960,1991), 
           label=c("1291","1523","1821","1918","1960","1991"), 
           alpha=1, hjust=c(.1,.9,.9,.9,.1,.1), vjust=.8,
           family="Monoton", size=15, color="black") +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque", colour = NA), 
        plot.margin = unit(c(.1, .3, .0 , .0),"cm") ) 

ggsave(TIDY, file="figure/TIDY_2021_28.png", width = 16, height = 9, units = "in", bg="bisque")
