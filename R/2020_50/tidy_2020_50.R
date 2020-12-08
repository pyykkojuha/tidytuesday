# TidyTueday, Week 50, 2020
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-12-08/readme.md

library(ggplot2)
library(plyr)
library(gridExtra)  
library(rnaturalearth)   
library(maps)
library(sf)
library(ggspatial)
library(rnaturalearthdata)

# DATA
women <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')

# import map data
world <- ne_countries(scale = "medium", returnclass = "sf")

#
# #
# # #
# SELECT COUNTRIES

# select countries
countries <- unique(women$country)
world$selected <-0

# to list missing countries to be included
missingfromlist <- data.frame(matrix(vector(), 0, 1, dimnames=list(c(), c("countrymissing"))), stringsAsFactors=F)
iii <- 1  

for(i in 1:length(countries)){
  before <- sum(world$selected)
  world$selected[world$sovereignt==countries[i]] <-1
  after <- sum(world$selected)
  if(before==after){missingfromlist[iii,1] <- countries[i]}
  iii=iii+1
}

# check
unique(countries)
unique(world$sovereignt[world$selected==1])

# add missed countries
na.omit(missingfromlist)
world$selected[world$sovereignt=="Iraq"] <-1
world$selected[world$sovereignt=="United Kingdom"] <-1
world$selected[world$sovereignt=="United Arab Emirates"] <-1
world$selected[world$sovereignt=="United States of America"] <-1
world$selected[world$sovereignt=="Democratic Republic of the Congo"] <-1
world$selected[world$sovereignt=="United Republic of Tanzania"] <-1

#
# #
# # #
# PLOT

map <-  ggplot(data = world) + aes(fill = as.factor(selected)) +
  geom_sf(lwd = 0) +  
  scale_fill_manual(values=c("gray11", "darkorchid1")) +
  coord_sf(crs = "+proj=laea +lat_0=40 +lon_0=0 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs") +
  theme_void() +  
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black", colour = NA),
        axis.text = element_text(colour = "black"),
        legend.position="none") 

t <- data.frame(cbind(c(1:10), c(1:10)))

title <- ggplot(t, aes(y=X1 , x=X2))  + 
  scale_x_continuous(limits=c(1,10)) + 
  scale_y_continuous(limits=c(1,10)) +
  annotate("text", x=1, y=9,   hjust=0, vjust=.5,size = 50, color="darkorchid1", family="Rubik Mono One", label="BBC") +
  annotate("text", x=1, y=7,   hjust=0, vjust=.5,size = 51, color="darkorchid1", family="Rubik Mono One", label="100") +
  annotate("text", x=1, y=5.1, hjust=0, vjust=.5,size = 30, color="darkorchid1", family="Rubik Mono One", label="Women") +
  annotate("text", x=1, y=3.5, hjust=0, vjust=.5,size = 38, color="darkorchid1", family="Rubik Mono One", label="2020") +
  annotate("text", x=1, y=2,   hjust=0, vjust=.5,size =  8, color="coral",       family="Rubik Mono One", label="What if south was north?") +
  annotate("text", x=1, y=1,   hjust=0, vjust=.5,size =  4, color="darkorchid1", family="Press Start 2P", label="@pyyxxo #TidyTuesday 2020/50") +
  theme_void() +
  theme(plot.background = element_rect(fill = "black", colour = NA), 
        plot.margin = unit(c(.1, .1, .1, .1),"cm") ) 

# flip the world
tidy <- grid.arrange(title, editGrob(ggplotGrob(map), vp=viewport(angle=-180)), widths=c(7/16, 9/16), ncol=2)

ggsave(tidy, file="figure/TIDY_2020_50.png", width = 16, height = 9, units = "in", bg = "black")
