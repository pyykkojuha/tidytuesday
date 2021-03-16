# TidyTueday, Week 12, 2021 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-03-16/readme.md

library(ggplot2)
library(ggtext)  
library(gridExtra)  
library(extrafont)

games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

nba2k <- subset(games, substr(gamename,1,3)=="NBA")

nba2k$month2 <- NA
nba2k$month2[nba2k$month=="January"] <- 1
nba2k$month2[nba2k$month=="February"] <- 2
nba2k$month2[nba2k$month=="March"] <- 3
nba2k$month2[nba2k$month=="April"] <- 4
nba2k$month2[nba2k$month=="May"] <- 5
nba2k$month2[nba2k$month=="June"] <- 6
nba2k$month2[nba2k$month=="July"] <- 7
nba2k$month2[nba2k$month=="August"] <- 8
nba2k$month2[nba2k$month=="September"] <- 9
nba2k$month2[nba2k$month=="October"] <- 10
nba2k$month2[nba2k$month=="November"] <- 11
nba2k$month2[nba2k$month=="December"] <- 12

nba2k$time <- nba2k$year + (nba2k$month2-1)/12

FONT <- "Staatliches"
MO <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
COLORS <- c("dodgerblue2", "forestgreen", "blue3", "brown3", "darkorchid1")

TIDY <- ggplot(nba2k) + 
  geom_line(aes(x=time, y=avg, color=gamename), size=2, alpha=.9) +
  geom_point(aes(x=time, y=avg, color=gamename), size=4, alpha=.9) +
  scale_x_continuous(breaks = c(2016:2022),
                     minor_breaks = seq(2016,2022,1/12), 
                     limits = c(min(nba2k$time),max(nba2k$time))) +
  scale_y_continuous(breaks = c(0,10000,20000), 
                     limits = c(-1100,max(nba2k$avg)),
                     labels = c("ZERO", "10K", "20K")) +
  annotate(geom="text", y=2000, x=2017+2/12,  label="2K17", alpha=.7, hjust=.5, vjust=.5, 
           family=FONT, size=32, color=COLORS[1]) +
  annotate(geom="text", y=5000, x=2018+2/12,  label="2K18", alpha=.7, hjust=.5, vjust=.5, 
           family=FONT, size=32, color=COLORS[2]) +
  annotate(geom="text", y=8000, x=2019+2/12,  label="2K19", alpha=.7, hjust=.5, vjust=.5, 
           family=FONT, size=32, color=COLORS[3]) +
  annotate(geom="text", y=14000, x=2020+5/12, label="2K20", alpha=.7, hjust=.5, vjust=.5, 
           family=FONT, size=32, color=COLORS[4]) +
  annotate(geom="text", y=4000, x=2021-2/12,  label="2K21", alpha=.7, hjust=.5, vjust=.5, 
           family=FONT, size=32, color=COLORS[5]) +
  annotate(geom="text", y=-1000, x=seq(2017,2020+11/12,1/12),      label=rep(MO,4), alpha=1, hjust=.5, vjust=1, 
           family=FONT, size=8, color="black") +
  annotate(geom="text", y=-1000, x=c(2021,2021+1/12),              label=MO[1:2],  alpha=1, hjust=.5, vjust=1, 
           family=FONT, size=8, color="black") +
  annotate(geom="text", y=-1000, x=seq(2016+8/12,2016+11/12,1/12), label=MO[9:12], alpha=1, hjust=.5, vjust=1, 
           family=FONT, size=8, color="black") +
    labs(title="NBA 2K in Steam",
         y="Average number of players at the same time",
         x="",
         caption="Source: Kaggle | Graphic: PYYXXO | Code: pyyxxo.fi/tt | #TidyTuesday 2021/12") +
  scale_color_manual(values=COLORS) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(colour = "black", size = 0.15),
        panel.grid.minor.x = element_line(colour = "black", size = 0.02),
        plot.background = element_rect(fill = "gold2", colour = NA), 
        plot.title   = element_text(size = 64, hjust=.0, color="black", family=FONT),
        axis.text    = element_text(size = 32, hjust=.5, color="black", family=FONT),
        axis.title   = element_text(size = 24, hjust=.5, color="black", family=FONT),
        plot.caption = element_text(size = 12, hjust=1, color="deeppink", family=FONT), 
        plot.margin  = unit(c(1, 1, 1, 1),"cm"),
        legend.position = "none")

ggsave(TIDY, file="figure/TIDY_2021_12.png", width = 16, height = 9, units = "in", bg = "gold2")
