# TidyTueday, Week 08, 2022 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-02-22/readme.md

rm(list=ls())

# libraries ----
library(ggplot2)
library(plyr)
library(ggtext)  

# color ----
COLOR1 <- "#FFADAD"
COLOR2 <- "#BDB2FF"
COLORbg <- "#FDFFB6"
COLORtx <- "#9BF6FF"
COLORti <- "#370617"
FONT1  <- "Oswald"
FONT2  <- "Press Start 2P"

# data ----
freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

# test ----
frdm <- ddply(freedom, .(country), summarize, 
              N = length(country),
              CL_mean = mean(CL),
              PR_mean = mean(PR),
              CL_sd   = sd(CL),
              PR_sd   = sd(PR),
              CL_min  = min(CL),
              PR_min  = min(PR),
              CL_max  = max(CL),
              PR_max  = max(PR))

frdm$CL_change = frdm$CL_max-frdm$CL_min
frdm$PR_change = frdm$PR_max-frdm$PR_min
frdm$main_diff = frdm$CL_mean-frdm$PR_mean

fin <- subset(freedom, country == "Finland")
chn <- subset(freedom, country == "China")
nig <- subset(freedom, country == "Nigeria")
mwi <- subset(freedom, country == "Malawi")
mal <- subset(freedom, country == "Mali")
sie <- subset(freedom, country == "Sierra Leone")

# plot ----
TIDY <- ggplot(subset(freedom, country == "Sierra Leone")) +
  geom_line(aes( x=year, y=-CL-.1), col=COLOR1, size=2, alpha=.8) +
  geom_line(aes( x=year, y=-PR+.1), col=COLOR2, size=2, alpha=.8) +
  geom_point(aes(x=year, y=-CL-.1), col=COLOR1, size=5) +
  geom_point(aes(x=year, y=-PR+.1), col=COLOR2, size=5) +
  scale_y_continuous(limits=c(-7.1,-0.9), breaks= -7:-1, labels = 7:1) +
  labs(title="Freedom in the World: SIERRA LEONE",
       subtitle="<span style='color:#FFADAD;'>Civil Liberties</span> + <span style='color:#BDB2FF;'>Political rights</span>",
       caption="@PYYXXO | Source: Freedom House / UN / Arthur Cheib | #TidyTuesday 2022/08",
       x="",
       y="worst . . . . . . . . . . . . . . . . . . . . . . best") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size=.5, color=COLORtx),
        plot.title    = element_text(    size = 34, hjust=1, color=COLORti, family=FONT1),
        plot.subtitle = element_markdown(size = 41, hjust=1, color=COLORti, family=FONT1),
        plot.caption  = element_text(    size =  6, hjust=1, color=COLORtx, family=FONT2), 
        axis.title    = element_text(    size = 20, hjust=.5, color=COLORtx, family=FONT1),
        axis.text     = element_text(    size = 18, hjust=.5, color=COLORtx, family=FONT1),
        plot.background  = element_rect(fill = COLORbg, colour = NA), 
        plot.margin = unit(c(3, 3, 3, 3),"mm"))

ggsave(TIDY, file="figure/TIDY_2022_08.png", width = 16*(2/3), height = 9*(2/3), units = "in") 
