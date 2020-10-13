# TidyTueday, Week 41, 2020
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-10-06/readme.md

library(ggplot2)
library(extrafont)

tournament <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')

TIDY <- ggplot(subset(tournament, !is.na(seed)), aes(x=as.factor(seed), y=reg_percent), width=.5) + 
  geom_boxplot(alpha=0.7, fill="firebrick3", color="darkslateblue") + 
  labs(title = "NCAA Women's Basketball Tournament", 
       subtitle = "Regular seasons 1982-2018") +
  xlab("Seed") + 
  ylab("Win percent") +
  scale_y_continuous(limits=c(0,100)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none", 
        plot.title    = element_text(colour = "darkslateblue"),
        plot.subtitle = element_text(colour = "darkslateblue", margin = unit(c(0, 0, 7, 0), "mm")),
        axis.text     = element_text(colour = "darkslateblue"),
        axis.title    = element_text(colour = "darkslateblue"),
        axis.title.x  = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
        axis.title.y  = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
        plot.background = element_rect(fill = "darkseagreen", colour = NA),
        text=element_text(family="Bebas Neue", size=18),
        plot.margin=unit(c(1, .5, 1, .5),"cm")) 

ggsave(TIDY, file="TIDY_2020_41.png", width = (1/2)*16, height = (1/2)*9, units = "in") 

#end
