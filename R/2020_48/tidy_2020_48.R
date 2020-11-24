# TidyTueday, Week 48, 2020
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-11-24/readme.md
# https://www.wta.org/go-outside/hikes?b_start:int=1

library(ggplot2)
library(extrafont)
library(plyr)
library(tidyverse)  # separate

# data
hike_data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))

# edits
hike <- separate(hike_data, length, into = c("miles", "type"), sep=" miles", remove=F)
hike <- separate(hike, location, into = c("region", "region2"), sep=" -- ", remove=F)
rm(hike_data)
hike$miles     <- as.numeric(hike$miles)
hike$gain      <- as.numeric(hike$gain)       #ft
hike$highpoint <- as.numeric(hike$highpoint)
hike$rating    <- as.numeric(hike$rating)

hike$length_km    <- hike$miles     * 1.609344
hike$gain_km      <- hike$gain      * 0.0003048
hike$highpoint_km <- hike$highpoint * 0.0003048

# order
hike$rank_all <- rank(hike$highpoint_km)

hpoint <- ddply(hike, .(region), summarise, 
           length = mean(length_km), 
           total = sum(length_km), 
           gain = mean(gain_km), 
           highpoint = mean(highpoint_km), 
           highest = max(highpoint_km))

hpoint$rank_region <- rank(hpoint$highpoint)
highest <- merge(hike, hpoint, by=c("region"), all=T)
highest <- highest[with(highest, order(rank_region, rank_all)), ]  # sort

highest$position <- 0
pos <- 0
for(i in 1:dim(highest)[1]){
  highest$position[i] <- pos + 1
  pos <- pos + 1
  if(i>1){
    if(highest$region[i-1] != highest$region[i]){
      highest$position[i] <- pos + 1
      pos <- pos + 1
    }
  }
}

regionchange <- ddply(highest, .(region), summarise, 
                      change = max(position) + 1,)
regionchange$change <- as.numeric(regionchange$change)
regionchange$y <- 0

# plot
TIDY <-  ggplot(highest, aes(x=position, y=highpoint_km, fill=region), width=.5) + 
  geom_point() +
  geom_segment(aes(x=position, xend=position, y=0, yend=highpoint_km)) +
  geom_text(data=regionchange, aes(x=change, y=y, label=region),
            size=6, alpha=.8,
            angle=90, hjust=0, vjust=0,
            family="Rubik Mono One", colour="plum2") +
  labs(title = "Washington State Hikes", 
       subtitle = "Highest points of 1958 trails by region",
       caption = "DATA: wta.org | #tidyTuesday 2020/48 @pyyxxo") +
  ylab("Highest point (km)") +  xlab("Trails") +
  scale_y_continuous(limits=c(0,4), breaks=seq(0,4,1)) +
  scale_x_continuous(breaks=regionchange$change) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(colour = "saddlebrown"),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none", 
        text=element_text(family="Rubik Mono One", size=12),
        plot.title    = element_text(colour = "black"),
        plot.caption  = element_text(colour = "plum2"),
        plot.subtitle = element_text(colour = "saddlebrown", margin = unit(c(0, 0, 2, 0), "mm")),
        axis.text     = element_text(colour = "saddlebrown"),
        axis.title    = element_text(colour = "saddlebrown"),
        axis.title.x  = element_text(margin = unit(c(0, 0, 0, 0), "mm")),
        axis.title.y  = element_text(margin = unit(c(0, 0, 2, 0), "mm")),
        plot.background = element_rect(fill = "khaki1", colour = NA),
        plot.margin=unit(c(.5, .5, .1, .5),"cm")) 

ggsave(TIDY, file="figure/TIDY_2020_48.png", width = (2/3)*16, height = (2/3)*9, units = "in") 
