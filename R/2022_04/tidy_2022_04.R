# TidyTueday, Week 04, 2022 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-25/readme.md

rm(list=ls())

# libraries ----
library(ggplot2)
library(MetBrewer)
library(extrafont)

# color/font ----
names(MetPalettes)
met.brewer("Renoir")
COLORS <- met.brewer("Renoir")
FONT <- "Sora"
FONT2 <- "Sora Thin"

# data ----
ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')
games <- merge(details, ratings, by="id")

# plot ----
TIDY <- ggplot(games) + 
  # TEXT
  geom_text(aes(x=4,  y=140000), label="- MEAN AVERAGE +", size=8, family=FONT, hjust=.0, vjust=.5, color=COLORS[6]) +
  geom_text(aes(x=11, y=140000), label="- BAYES AVERAGE +", size=8, family=FONT, hjust=.0, vjust=.5, color=COLORS[6]) +
  geom_text(aes(x=10.1, y=11000), label="+\no\nw\nn\ne\nd\n-", size=6, family=FONT, hjust=.5, vjust=.0, color=COLORS[5], angle=0) +
  # DATA POINTS
  geom_point(aes(x=average,         y=owned, size=wanting, fill=wishing), shape=22, alpha=.85) +
  geom_point(aes(x=bayes_average+7, y=owned, size=wanting, fill=wishing), shape=22, alpha=.85) +
  # LEGEND
  geom_rect(aes(xmin=0.7, xmax=4.1, ymin=85000, ymax=115000), fill=NA, color=COLORS[5]) +
  geom_point(aes(x=1, y=100000+5000, size=1500, fill=10000), shape=22, alpha=.85) +
  geom_point(aes(x=1, y=100000-5000, size=500, fill=1000), shape=22, alpha=.85) +
  geom_text(aes(x=1.2, y=100000), label="bigger = more wanted\nmore red = more wished", 
            size=4, family=FONT2, hjust=.0, vjust=.5, color=COLORS[2]) +
  # CUSTOM
  scale_fill_continuous(low=COLORS[4], high=COLORS[8]) +
  scale_x_continuous(limits=c(.69, 16)) +
  # TITLES
  labs(title="BOARD GAMES - Ratings on two scales",
       subtitle="Owning, wanting, & wishing the best rated board games",
       x="",
       y="",
       caption="@PYYXXO | data source: Kaggle / Board Games Geek | #TidyTuesday 2022/04") +
  # THEME
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = COLORS[3], colour = NA),
        plot.title    = element_text(size = 28, hjust=.1, colour = COLORS[4], margin = unit(c(0, 0, 0, 0), "mm"), family=FONT),
        plot.subtitle = element_text(size = 22, hjust=.15, colour = COLORS[5], margin = unit(c(0, 0, 5, 0), "mm"), family=FONT),
        plot.caption  = element_text(size =  8, hjust=.92, colour = COLORS[6], margin = unit(c(5, 0, 0, 0), "mm"), family=FONT2), 
        plot.margin = unit(c(5, 5, 5, 0),"mm"))

ggsave(TIDY, file="figure/TIDY_2022_04.png", width = 16*(2/3), height = 9*(2/3), units = "in", bg=COLORS[3]) 
