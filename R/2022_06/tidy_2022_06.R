# TidyTueday, Week 06, 2022 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-02-08/readme.md

rm(list=ls())

# libraries ----
library(ggplot2)
library(plyr)

# color ----
COLOR1 <- "#005f73"  # low
COLOR2 <- "#bb3e03"  # high
COLOR3 <- "#e9d8a6"  # bg
FONT1  <- "Anton"
FONT2  <- "Press Start 2P"

# data ----
airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

# subset ----
airmen2 <- ddply(airmen, .(state), summarize, N = length(state))
table(airmen2$N)
airmen2 <- subset(airmen2, !is.na(state) & N >= 20)
airmen2$rank <- rank(airmen2$N, ties.method = "random")

# plot ----
TIDY <- ggplot(airmen2, aes(x=rank, y=N, fill=N)) +
  geom_col() +
  scale_y_continuous(limits=c(0,120)) +
  geom_text(aes(x=rank,  y=N+14, label=state), size=10, family=FONT1, hjust=.5, vjust=0, color=COLOR1) +
  geom_text(aes(x=rank,  y=N+12, label=N),     size=10, family=FONT1, hjust=.5, vjust=1, color=COLOR2) +
  scale_fill_gradient(low = COLOR1, high = COLOR2) +
  labs(x = "S  T  A  T  E  S     W  I  T  H     M  O  S  T     A  I  R  M  E  N",
       y = "",
       caption = "@PYYXXO + data: Tuskegee Airmen Challenge / CAF + #TidyTuesday 2022/06") +
  theme_void() +
  theme(legend.position = "none",
        axis.title       = element_text(size = 24, hjust=.5, colour = COLOR1, margin = unit(c(0, 0, 0, 0), "mm"), family=FONT1), 
        plot.caption     = element_text(size = 8,  hjust=.5, colour = COLOR2, margin = unit(c(5, 0, 0, 0), "mm"), family=FONT2), 
        plot.background  = element_rect(fill = COLOR3, colour = NA), 
        plot.margin = unit(c(10, 5, 5, 5),"mm"))

ggsave(TIDY, file="figure/TIDY_2022_06.png", width = 16*(2/3), height = 9*(2/3), units = "in") 
