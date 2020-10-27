# TidyTueday, Week 44, 2020
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-10-27/readme.md

library(ggplot2)
library(extrafont)
library(plyr)

# data
turbines <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')

# edits
turbines$year <- as.numeric(substr(turbines$commissioning_date, 1, 4))

# summarize
year <- ddply(turbines, .(year), summarize, N = length(objectid))
year$cumulative <- cumsum(year$N)

# plot
TIDY <- ggplot(year, aes(x=year, y=cumulative)) + 
  geom_line(size=2) +
  geom_col(data=year, aes(x=year, y=N), colour = "indianred", fill = "indianred", size=.1) +
  labs(title = "CANADIAN WIND TURBINES",
       subtitle = "By Comissioning Year",
       caption = "#TidyTuesday 2020/44") +
  xlab("") + ylab("") +
  scale_y_continuous(breaks=seq(0, 7000, 1000)) +
  scale_x_continuous(breaks=seq(1993, 2019, 1)) +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(colour = "indianred", size = 0.1), 
        panel.grid.major.x = element_line(colour = "indianred", size = 0.1),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(family="Major Mono Display"),
        plot.title =    element_text(size = 36, colour = "black", hjust=.5), 
        plot.subtitle = element_text(size = 30, colour = "black", hjust=.5, margin = unit(c(0, 0, 7, 0), "mm")), 
        axis.text.x =   element_text(size = 14, colour = "indianred", angle=90, vjust=.5),
        axis.text.y =   element_text(size = 16, colour = "indianred"),
        plot.background = element_rect(fill = "ivory", colour = NA),
        plot.margin=unit(c(1,1,1,1),"cm") )

ggsave(TIDY, file="figure/TIDY_2020_44.png", width = (2/3)*16, height = 6, units = "in") 
