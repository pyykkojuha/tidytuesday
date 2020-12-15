# TidyTueday, Week 51, 2020
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-12-15/readme.md

# https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html

library(ggplot2)
library(plyr)
library(gridExtra)
library(ggwordcloud)

# data
ninja_warrior <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-15/ninja_warrior.csv')

obstacles <- ddply(ninja_warrior, .(obstacle_name), summarize, N = length(obstacle_name))

obstacles11 <- subset(obstacles, N>=11)

set.seed(6) # for word mapping

tidy <-  ggplot(obstacles11, aes(label = obstacle_name, size = N)) +
  geom_text_wordcloud_area(area_corr_power = 1, family="Covered By Your Grace") +
  scale_size_area(max_size = 11) +
  theme_minimal() +
  labs(title="American Ninja Warrior",
       subtitle="Most Popular Obstacles",
       caption="@pyyxxo #TidyTuesday 2020/51\nData: Data.World / sasukepedia") +
  theme(plot.title    = element_text(colour = "darkblue", size=20),
        plot.subtitle = element_text(colour = "darkblue", size=18),
        plot.caption  = element_text(colour = "darkblue",  size=12),
        plot.background = element_rect(fill = "aquamarine", colour = NA),
        text=element_text(family="Covered By Your Grace"),
        plot.margin=unit(c(.5, .5, .5, .5),"cm"))

ggsave(tidy, file="figure/TIDY_2020_51.png", width = 16*(2/4), height = 9*(2/4), units = "in", bg = "aquamarine")
