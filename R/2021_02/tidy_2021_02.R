# TidyTueday, Week 2, 2021
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-05/readme.md

library(ggplot2)
library(plyr)
library(gridExtra)  
library(tidyverse)

# data
transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')
transit_cost <- subset(transit_cost, !is.na(country))

# plot
tidy <-  ggplot(transit_cost) + 
  geom_boxplot(aes(x=as.factor(year), y=cost_km_millions), fill="lightcoral") +
  geom_text(data=subset(transit_cost, cost_km_millions>1000), 
            aes(x=as.factor(year), y=cost_km_millions, label = city), na.rm = TRUE, hjust = -0.1, size = 6, family="Bebas Neue") +
  labs(caption = "data: transitcosts.com | code: pyyxxo.fi/tt | #TidyTuesday 2021/02") +
  ylab("") + xlab("Transit Costs of Lines by Midpoint year of construction") +
  scale_y_continuous(limits = c(0, 4000), 
                     breaks = c(0, 1000, 2000, 3000, 4000),
                     label = c("0 $/km", "1,000,000,000 USD/km", "2,000,000,000 USD/km", "3,000,000,000 USD/km", "4,000,000,000 USD/km")) +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(colour = "black", size = 0.1), 
        panel.grid.major.x = element_line(colour = "black", size = 0.1),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(family="Bebas Neue"),
        plot.caption =  element_text(size = 18, colour = "black", hjust=1), 
        axis.text.x  =  element_text(size = 24, colour = "black", angle=90, vjust=.5),
        axis.text.y  =  element_text(size = 24, colour = "black"),
        axis.title.x =  element_text(size = 32, colour = "black", margin = unit(c(8, 8, 8, 8), "mm")),
        axis.title.y =  element_text(size = 32, colour = "black"),
        plot.background = element_rect(fill = "khaki2", colour = NA),
        plot.margin=unit(c(1,1,1,.2),"cm") )

ggsave(tidy, file="figure/TIDY_2021_02.png", width = 16, height = 9, units = "in", bg = "khaki2")
