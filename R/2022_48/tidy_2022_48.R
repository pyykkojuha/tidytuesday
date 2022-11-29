# TidyTueday, Week 48, 2022 ----
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-11-29

rm(list=ls())

# libraries ----
library(ggplot2)
library(dplyr)
library(data.table)

# data ----
wcmatches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv')
worldcups <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv')

wc <- wcmatches %>% 
  group_by(year, winning_team) %>% 
  summarise(n = n())

wc <- subset(wc, !is.na(winning_team))

wc <- wc %>% 
  group_by(winning_team) %>% 
  mutate(csum = cumsum(n))

# color/font ----
# https://coolors.co/
FONT   <- "Epilogue"

BLACK1 <- "#231942"
BLACK2 <- "#9f86c0"
WHITE1 <- "#fefae0"

OTHERS <- "#ffc8dd"

BRAZIL <- "#03071e"
FRANCE <- "#023e8a"
ARGENT <- "#6a040f"
ITALIA <- "#0077b6"
GERMAN <- "#03045e"
WEGERM <- "#dc2f02"
ESPANA <- "#d00000"

# plot ----
TIDY <- ggplot(wc, aes(y=csum, x=year, group=winning_team)) + 
  # ALL COUNTRIES
  geom_step(col=OTHERS, alpha=.8) +
  # SPECIFIC COUNTRIES 1
  geom_step(data=subset(wc, winning_team=="Brazil"), aes(y=csum, x=year), col=BRAZIL, alpha=.8) +
  geom_point(data=subset(wc, winning_team=="Brazil" & csum==max(wc$csum[wc$winning_team=="Brazil"])), aes(y=csum, x=year), col=BRAZIL) +
  annotate(geom="text", 
           y=max(wc$csum[wc$winning_team=="Brazil"]), 
           x=max(wc$year[wc$winning_team=="Brazil"]+1), label="Brazil",
           hjust=0, vjust=.5, size=4, family=FONT, color=BRAZIL) +
  # SPECIFIC COUNTRIES 2
  geom_step(data=subset(wc, winning_team=="France"), aes(y=csum, x=year), col=FRANCE, alpha=.8) +
  geom_point(data=subset(wc, winning_team=="France" & csum==max(wc$csum[wc$winning_team=="France"])), aes(y=csum, x=year), col=FRANCE) +
  annotate(geom="text", 
           y=max(wc$csum[wc$winning_team=="France"]), 
           x=max(wc$year[wc$winning_team=="France"]+1), label="France",
           hjust=0, vjust=.5, size=4, family=FONT, color=FRANCE) +
  # SPECIFIC COUNTRIES 3
  geom_step(data=subset(wc, winning_team=="Argentina"), aes(y=csum, x=year), col=ARGENT, alpha=.8) +
  geom_point(data=subset(wc, winning_team=="Argentina" & csum==max(wc$csum[wc$winning_team=="Argentina"])), aes(y=csum, x=year), col=ARGENT) +
  annotate(geom="text", 
           y=max(wc$csum[wc$winning_team=="Argentina"]), 
           x=max(wc$year[wc$winning_team=="Argentina"]+1), label="Argentina",
           hjust=0, vjust=.5, size=4, family=FONT, color=ARGENT) +
  # SPECIFIC COUNTRIES 4 (MINOR)
  geom_step(data=subset(wc, winning_team=="Italy"), aes(y=csum, x=year), col=ITALIA, alpha=.8) +
  geom_point(data=subset(wc, winning_team=="Italy" & csum==max(wc$csum[wc$winning_team=="Italy"])), aes(y=csum, x=year), col=ITALIA) +
  annotate(geom="text", 
           y=max(wc$csum[wc$winning_team=="Italy"]+3), 
           x=max(wc$year[wc$winning_team=="Italy"]), label="Italy",
           hjust=.5, vjust=1, size=3, family=FONT, color=ITALIA) +
  # SPECIFIC COUNTRIES 5 (MINOR)
  geom_step(data=subset(wc, winning_team=="Germany"), aes(y=csum, x=year), col=GERMAN, alpha=.8) +
  geom_point(data=subset(wc, winning_team=="Germany" & csum==max(wc$csum[wc$winning_team=="Germany"])), aes(y=csum, x=year-.2), col=GERMAN) +
  annotate(geom="text", 
           y=max(wc$csum[wc$winning_team=="Germany"]), 
           x=max(wc$year[wc$winning_team=="Germany"]+1), label="Germany",
           hjust=0, vjust=0, size=3, family=FONT, color=GERMAN) +
  # SPECIFIC COUNTRIES 6 (MINOR)
  geom_step(data=subset(wc, winning_team=="Spain"), aes(y=csum, x=year), col=ESPANA, alpha=.8) +
  geom_point(data=subset(wc, winning_team=="Spain" & csum==max(wc$csum[wc$winning_team=="Spain"])), aes(y=csum, x=year+.2), col=ESPANA) +
  annotate(geom="text", 
           y=max(wc$csum[wc$winning_team=="Spain"]), 
           x=max(wc$year[wc$winning_team=="Spain"]+1), label="Spain",
           hjust=0, vjust=1, size=3, family=FONT, color=ESPANA) +
  # SPECIFIC COUNTRIES 7 (MINOR)
  geom_step(data=subset(wc, winning_team=="West Germany"), aes(y=csum, x=year), col=WEGERM, alpha=.8) +
  geom_point(data=subset(wc, winning_team=="West Germany" & csum==max(wc$csum[wc$winning_team=="West Germany"])), aes(y=csum, x=year), col=WEGERM) +
  annotate(geom="text", 
           y=max(wc$csum[wc$winning_team=="West Germany"]), 
           x=max(wc$year[wc$winning_team=="West Germany"]+.5), label="West Germany",
           hjust=0, vjust=.5, size=3, family=FONT, color=WEGERM) +
  # SCALE
  scale_x_continuous(breaks=unique(wc$year), limits=c(1930,2028),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, 10),
                     limits = c(0, 80), 
                     expand = c(0, 0)) +
  labs(title="Cumulative match wins in World Cup",
       caption="Graphic: P Y Y X X O | Source: Kaggle FIFA World Cup | #TidyTuesday 2022/48") +
  #THEME:
  theme_minimal() +
  theme(legend.position = "none", 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background  = element_rect(colour = WHITE1, fill = WHITE1),
        panel.background = element_rect(colour = WHITE1, fill = WHITE1),
        plot.title = element_text(family = FONT, size = 24, colour = BLACK1, face = "bold"),
        plot.caption = element_text(hjust = 0, colour = BLACK2, size = 8, family = FONT, margin = margin(t = 20)),
        axis.text.y = element_text(family = FONT, size = 10, colour = BLACK1, hjust=1, vjust=.5),
        axis.text.x = element_text(family = FONT, size = 12, colour = BLACK1, angle=90, hjust=1, vjust=.5),
        plot.title.position = "plot",
        plot.caption.position = "plot", 
        plot.margin = unit(c(2, 2, 2, 2),"mm"))

ggsave(TIDY, file="figure/TIDY_2022_48.png", width = 16*(2/3), height = 9*(2/3), units = "in") 
