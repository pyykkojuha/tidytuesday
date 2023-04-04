# TidyTueday, Week 14, 2023 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-04-04/readme.md

# libraries ----
library(ggplot2)

# data ----
jalkapallo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-04/soccer21-22.csv')

# variable 	class 	description
# Date 	character 	The date when the match was played
# HomeTeam 	character 	The home team
# AwayTeam 	character 	The away team
# FTHG 	double 	Full time home goals
# FTAG 	double 	Full time away goals
# FTR 	character 	Full time result
# HTHG 	double 	Halftime home goals
# HTAG 	double 	Halftime away goals
# HTR 	character 	Halftime results
# Referee 	character 	Referee of the match
# HS 	double 	Number of shots taken by the home team
# AS 	double 	Number of shots taken by the away team
# HST 	double 	Number of shots on target by the home team
# AST 	double 	Number of shots on target by the away team
# HF 	double 	Number of fouls by the home team
# AF 	double 	Number of fouls by the away team
# HC 	double 	Number of corners taken by the home team
# AC 	double 	Number of corners taken by the away team
# HY 	double 	Number of yellow cards received by the home team
# AY 	double 	Number of yellow cards received by the away team
# HR 	double 	Number of red cards received by the home team
# AR 	double 	Number of red cards received by the away team

# edit ----
## 0-0
jalkapallo$nilnil <- 0
jalkapallo$nilnil[jalkapallo$FTHG==0 & jalkapallo$FTAG==0] <- 1

## full game stats (home+away)
jalkapallo$goals   <- jalkapallo$FTHG  + jalkapallo$FTAG
jalkapallo$shots   <- jalkapallo$HS  + jalkapallo$AS
jalkapallo$target  <- jalkapallo$HST + jalkapallo$AST
jalkapallo$fouls   <- jalkapallo$HF  + jalkapallo$AF
jalkapallo$corners <- jalkapallo$HC  + jalkapallo$AC
jalkapallo$yellow  <- jalkapallo$HY  + jalkapallo$AY
jalkapallo$red     <- jalkapallo$HR  + jalkapallo$AR

## summary by 0-0
nilnil <- ddply(jalkapallo, "nilnil", summarise,
                n       = length(nilnil),
                shots   = round(mean(shots),1),
                target  = round(mean(target),1),
                fouls   = round(mean(fouls),1),
                corners = round(mean(corners),1),
                yellow  = round(mean(yellow),1),
                red     = round(mean(red),1))

## data for geom_tile
shots <- ddply(jalkapallo, c("target", "shots"), summarise,
                n       = length(nilnil),
                goals   = round(mean(goals),1))

# fonts ----
FONT1 <- "Atkinson Hyperlegible Regular"
FONT2 <- "Atkinson Hyperlegible Bold"

# facts for plot (0-0 vs other) ----
nilnil
summary(lm(jalkapallo$shots   ~ jalkapallo$nilnil))
summary(lm(jalkapallo$target  ~ jalkapallo$nilnil))
summary(lm(jalkapallo$fouls   ~ jalkapallo$nilnil))
summary(lm(jalkapallo$corners ~ jalkapallo$nilnil))
summary(lm(jalkapallo$yellow  ~ jalkapallo$nilnil))
summary(lm(jalkapallo$red     ~ jalkapallo$nilnil))
round(22/380,2)

# plot ----
TIDY <- ggplot(shots, aes(x=target, y=shots, fill=goals)) +
  geom_tile() +
  scale_fill_binned(type = "viridis",breaks=c(0,1,2,3,5,9),
                    limits=c(0,9)) +
  scale_y_continuous(limits=c(10,42)) +
  scale_x_continuous(limits=c(0,20)) +
  coord_fixed() +
  labs(title="Does 0-0 make a match boring?",
       subtitle="22 of 380 matches (6%) ended 0-0 in Premier League season 2021-22.
On average, goalless matches only have less shots on target (4.9 v 9.1)
but no difference on shots (24.2 v 25.7), fouls (20.0 v 20.2), corners (11.1 v 10.4), 
yellow cards (3.6 v 3.4), or red cards (0.1 v 0.1), compared to matches with 1+ goals.
Plot below shows goal averages by each pair of shots and shots on target.",
       y="Shots",
       x="Shots on target",
       fill="Goals on\naverage\nin a match\n",
       caption="                                                  Graphic: PYYXXO | #TidyTuesday 2023/14
Source: Premier League Match Data 2021-2022 via Evan Gower on Kaggle") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        legend.position = c(1.2, .77),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title    = element_text(hjust = 0, colour = "black", size = 22, family = FONT2),
        plot.subtitle = element_text(hjust = 0, colour = "black", size = 10, family = FONT1),
        legend.title  = element_text(hjust = 0,  colour = "black", size = 11,  family = FONT2),
        legend.text   = element_text(hjust = 0, vjust=.5, colour = "black", size = 11,  family = FONT2),
        plot.caption  = element_text(hjust = 3.1, colour = "black", size = 7,  family = FONT1),
        axis.title.y  = element_text(hjust=.04,  size = 14, family=FONT2, color="black"),
        axis.title.x  = element_text(hjust=.05, size = 14, family=FONT2, color="black"),
        axis.text     = element_text(hjust=.5, vjust=.5, size = 14, family=FONT2, color="black"),
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        plot.margin     = unit(c(3, 30, 3, 0),"mm"))

ggsave(TIDY, file="figure/TIDY_2023_14.png", width = 9*(2/3), height = 11*(2/3), units = "in")
