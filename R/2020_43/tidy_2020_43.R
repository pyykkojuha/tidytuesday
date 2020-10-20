# TidyTueday, Week 43, 2020
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-10-20/readme.md

library(ggplot2)
library(extrafont)
library(plyr)

# data
beer_awards <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')
bigcities <- subset(beer_awards, city=="Chicago" | city=="Houston" | city=="New York" | city=="Los Angeles")

# edits
bigcities$medal <- factor(three$medal,c("Bronze", "Silver", "Gold"))
cities <- ddply(bigcities, .(city, medal), summarize, N = length(state) )
cities

# plot
TIDY <- ggplot(cities, aes(x=city, y=N, fill=medal)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("darkgoldenrod4", "slategray1", "gold2"), name = "Medals:") + 
  scale_y_continuous(breaks=c(25, 25+20, 25+20+29)) +
  labs(title = "Great American\nBeer Festival",
       subtitle = "1987-2020  x  Big Cities") +
  xlab("") + ylab("") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_line(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.87, 1.275), 
        text = element_text(family="Monoton"),
        plot.title =    element_text(size = 30, colour = "lightcoral"), 
        plot.subtitle = element_text(size = 20, colour = "cyan", margin = unit(c(0, 0, 7, 0), "mm")), 
        legend.title =  element_text(size = 16, colour = "lightcoral"), 
        legend.text =   element_text(size = 16, colour = "cyan"), 
        axis.text.x =   element_text(size = 20, colour = "lightcoral"),
        axis.text.y =   element_text(size = 16, colour = "cyan"),
        plot.background = element_rect(fill = "black", colour = NA),
        plot.margin=unit(c(1,1,1,1),"cm") )

ggsave(TIDY, file="figure/TIDY_2020_43.png", width = 12, height = 6, units = "in") 
