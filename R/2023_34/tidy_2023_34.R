# TidyTueday, Week 34, 2022
# PYYXXO
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-08-22/readme.md

library(ggplot2)
library(ggtext)
library(tidyverse)
library(plyr)
library(scales) 

# data ---- 
refugees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-22/population.csv')

# data edits ---- 
# reduce returned
refugees$refugees_net <- refugees$refugees - refugees$returned_refugees
# sum numbers by out/in and merge
COO <- ddply(refugees, .(coo_name), summarise, OUT = sum(refugees_net))
COA <- ddply(refugees, .(coa_name), summarise, IN = sum(refugees_net))
CO <- merge(COO, COA, by.x="coo_name", by.y="coa_name", all=TRUE)
# NAs to zero
CO$OUT[is.na(CO$OUT)] <- 0
CO$IN[is.na(CO$IN)] <- 0
# calculate net refugees
CO$TOTAL <- CO$IN - CO$OUT
# subset 5 M+
CO100 <- subset(CO, abs(TOTAL)>=5000000)
# rank for geom_bar()
CO100$RANK <- rank(CO100$TOTAL, ties.method="random")
# rename full
table(CO100$coo_name)
CO100$coo_name[CO100$coo_name == "Central African Rep."] <- "Central African Republic"
CO100$coo_name[CO100$coo_name == "Syrian Arab Rep."] <- "Syrian Arab Republic"
CO100$coo_name[CO100$coo_name == "Iran (Islamic Rep. of)"] <- "Islamic Republic of Iran"

# fonts ----
FONT1 <- "Amiko"

# plot ----
TIDY <- ggplot(CO100, aes(y=TOTAL, x=RANK, fill=TOTAL)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept = 0, col="white", linewidth = .25) +
  geom_text(data=subset(CO100, TOTAL<0), aes(x=RANK, y= 1000000, label=coo_name), angle=0, hjust=0, vjust=.5, family=FONT1, col="white") +
  geom_text(data=subset(CO100, TOTAL>0), aes(x=RANK, y=-1000000, label=coo_name), angle=0, hjust=1, vjust=.5, family=FONT1, col="white") +
  scale_y_continuous(breaks = seq(-60000000,-10000000,10000000),
                     limits =   c(-60000000,30000000),
                                         labels = unit_format(unit = "M", scale = 1e-6),
                     sec.axis = sec_axis(~ ., breaks=seq(10000000,50000000,10000000),
                                         labels = unit_format(unit = "M", scale = 1e-6, prefix = "+"))) +
  scale_x_continuous(expand = c(0.01, 0.01)) + 
  scale_fill_gradientn(colours=rainbow(4))  +
  coord_flip() +
  labs(title = "THESE NUMBERS MUST BE OFF BECAUSE THEY EXCEED POPULATIONS\nBUT THIS IS WHAT THE DATA GAVE ME: Countries with 5 M+ difference\nbetween refugees in and out (2010–2022)",
       subtitle = NULL, x = NULL, y = NULL,
       caption = "Graphic: P Y Y X X O | #TidyTuesday 2023/34\nSource: PopulationStatistics {refugees} R package") +
  theme_minimal(base_family = FONT1) +
  theme(legend.position="none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(colour="white"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour="white", size=12),
        axis.ticks.x = element_line(linewidth = .25, colour = "white"),
        plot.background  = element_rect(colour = "gray10", fill = "gray10"),
        plot.title    = element_text(size = 20),
        plot.subtitle = element_text(size = 17),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1, colour = "gray70"),
        plot.margin = unit(c(5,5,5,5), 'mm'))

ggsave(TIDY, file="figure/TIDY_2023_34.png", width = 16*(2/3), height = 9*(2/3), units = "in")
