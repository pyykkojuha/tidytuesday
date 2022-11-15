# TidyTueday, Week 46, 2022 ----
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-11-15

# THIS CODE IS BASED ON THE FOLLOWING:
# ORIGINAL SOURCE CODE BY nrennie: https://github.com/nrennie/tidytuesday/blob/main/2022/2022-11-15/20221115.R
# ORGINAL INSPIRATION BY nrennie FROM: https://blog.datawrapper.de/why-web-pages-can-have-a-size-problem/

rm(list=ls())

# libraries ----
library(ggplot2)
library(tidyverse)
library(lubridate)

# data ----
bytes_total <- readr::read_csv('https://raw.githubusercontent.com/zakvarty/tidytuesday/page-metrics-data-fix/data/2022/2022-11-15/bytes_total.csv')

data1 <- bytes_total %>% 
  select(date, client, p90) %>% 
  mutate(date = ymd(date))

data1 <- subset(data1, date >= ymd("2012-01-01"))

# color/font ----
# https://coolors.co/
FONT   <- "Open Sans"
BLACK1 <- "#212529"
BLACK2 <- "#343a40"
WHITE1 <- "#f8f9fa"
WHITE2 <- "#e9ecef"
LINES1 <- "#ffca3a"
LINES2 <- "#ff595e"

# plot ----
TIDY <- ggplot(data = data1, mapping = aes(x = date, y = p90, colour = client)) +
  # BACKGROUND COLOR FOR YEARS:
  geom_rect(aes(xmin = ymd("2012-01-01"), 
                xmax = ymd("2012-12-31"),
                ymin = 0,
                ymax = 9300),
                fill = BLACK2,
                col  = BLACK2) +
  geom_rect(aes(xmin = ymd("2014-01-01"),
                xmax = ymd("2014-12-31"),
                ymin = 0,
                ymax = 9300),
                fill = BLACK2,
                col  = BLACK2) +
  geom_rect(aes(xmin = ymd("2016-01-01"),
                xmax = ymd("2016-12-31"),
                ymin = 0,
                ymax = 9300),
                fill = BLACK2,
                col  = BLACK2) +
  geom_rect(aes(xmin = ymd("2018-01-01"),
                xmax = ymd("2018-12-31"),
                ymin = 0,
                ymax = 9300),
                fill = BLACK2,
                col  = BLACK2) +
  geom_rect(aes(xmin = ymd("2020-01-01"),
                xmax = ymd("2020-12-31"),
                ymin = 0,
                ymax = 9300),
                fill = BLACK2,
               col  = BLACK2) +
  geom_rect(aes(xmin = ymd("2022-01-01"),
                xmax = ymd("2022-12-31"),
                ymin = 0,
                ymax = 9300),
                fill = BLACK2,
                col  = BLACK2) +
  #HORIZONTAL LINES:
  geom_hline(yintercept = 0, col = WHITE2, size=1) +
  geom_hline(yintercept = seq(1000, 9000, 1000), col = WHITE2, size=.25) +
  #DATA LINES:
  geom_line(size=1.1) +
  annotate("text", x = ymd("2013-07-01"), y = 3800, label = "Desktop", colour = LINES1, family = FONT, fontface = "bold") +
  annotate("text", x = ymd("2015-07-01"), y = 2200, label = "Mobile",  colour = LINES2, family = FONT, fontface = "bold") +
  scale_colour_manual(values = c(LINES1, LINES2)) +
  #AXIS LABELS
  scale_x_date(breaks = seq(ymd("2012-07-01"), ymd("2022-07-01"), by = "1 year"),
               labels = 2012:2022,
               limits = ymd(c("2011-12-31", "2022-12-31")),
               expand = expansion(add = c(0, 0))) +
  scale_y_continuous(breaks = seq(0, 9000, 1000),
                     labels = c(format(seq(0, 8000, 1000), big.mark=","), "9,000 KB"),
                     minor_breaks = c(0),
                     limits = c(0, 9300), 
                     expand = c(0, 0)) +
  #TITLES:
  labs(title   = "Web page weights",
       subtitle = "90th percentile", 
       caption = "Graphic: PYYXXO (Code remixed from Nicola Rennie) | Source: httpArchive.org | #TidyTuesday 2022/46") +
  #THEME:
  theme(legend.position = "none", 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background  = element_rect(colour = BLACK1, fill = BLACK1),
        panel.background = element_rect(colour = BLACK1, fill = BLACK1),
        plot.title = element_text(family = FONT, size = 12, colour = WHITE1, face = "bold"),
        plot.subtitle = element_text(family = FONT, size = 11, colour = WHITE1),
        plot.caption = element_text(hjust = 1, colour = WHITE2, size = 8, family = FONT, margin = margin(t = 20)),
        axis.text = element_text(family = FONT, size = 10, colour = WHITE2),
        plot.title.position = "plot",
        plot.caption.position = "plot", 
        plot.margin = unit(c(2, 2, 2, 2),"mm"))

ggsave(TIDY, file="figure/TIDY_2022_46.png", width = 16*(2/3), height = 9*(2/3), units = "in") 
