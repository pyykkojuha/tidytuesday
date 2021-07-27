# TidyTueday, Week 31, 2021 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-07-27/readme.md

rm(list=ls())

library(ggplot2)
library(ggridges) 

# data ----
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

# subset ----
basketball <- subset(olympics, sport=="Basketball")

# plot ----
TIDY <- ggplot(data = basketball, aes(x = height, y = as.factor(year), fill=sex)) +
  geom_density_ridges(color="black",  size=.9, scale=3, alpha=.7,
                      quantile_lines=TRUE, quantile_fun=function(x,...)mean(x)) +
  coord_flip() +
  scale_x_continuous(breaks=seq(160, 220, 10), labels =c("160 cm", "170 cm", "180 cm", "190 cm", "200 cm", "210 cm", "220 cm")) +
  labs(title="OLYMPIC BASKETBALL",
       subtitle = "HEIGHT DISTRIBUTION OF PLAYERS WITH MEAN LINES BY SEX",
       y="",
       x="",
       caption = "CODE: pyyxxo | SOURCE: Kaggle | #tidytuesday 2021/31") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "black", colour = NA),
        text=element_text(family="Montserrat"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#EEE1C6", size = 0.2),
        panel.grid.minor  = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.ticks.x  = element_blank(),
        plot.title    = element_text(size = 52, colour = "#EEE1C6", hjust=.0), 
        plot.subtitle = element_text(size = 35.5, colour = "#EEE1C6", hjust=.0, margin = unit(c(0, 0, 5 , 0), "mm")), 
        plot.caption  = element_text(size = 12, colour = "#EEE1C6", hjust=1, family="Press Start 2P"), 
        axis.text.x   = element_text(size = 24, colour = "#EEE1C6", angle=270, vjust=0,  hjust=0, family="Press Start 2P"),
        axis.text.y   = element_text(size = 18, colour = "#EEE1C6", angle=0,   vjust=.5, hjust=0, family="Press Start 2P"),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.margin = unit(c(.2, .2, .2, .2),"cm") ) 

ggsave(TIDY, file="figure/TIDY_2021_31.png", width = 16, height = 9, units = "in", bg="black")
