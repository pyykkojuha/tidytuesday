# TidyTueday, Week 42, 2020
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-10-13/readme.md

library(ggplot2)
library(extrafont)

datasaurus <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-13/datasaurus.csv')

TIDY <- ggplot(datasaurus, aes(x=x, y=y)) + 
  geom_point(                                                         size = 7, shape = 24, fill = "cornflowerblue", color = "coral", alpha=.6) +
  geom_point(data=subset(datasaurus, dataset=="dino"), aes(y=y, x=x), size = 7, shape = 25, fill = "coral", color = "cornflowerblue", alpha=.8) +
  labs(title = "Dino inside Datasaurus") +
  scale_x_continuous(limits=c(0,100)) +
  scale_y_continuous(limits=c(0,100)) +
  theme_void() +
  theme(plot.title    = element_text(colour = "darkblue"),
        plot.background = element_rect(fill = "aquamarine", colour = NA),
        text=element_text(family="Covered By Your Grace", size=16),
        plot.margin=unit(c(1, 1, 1, 1),"cm")) 
 
ggsave(TIDY, file="TIDY_2020_42.png", width = 6, height = 6, units = "in") 
