# TidyTueday, Week 33, 2021 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-08-10/readme.md

# CREDITS:
# shoutout to @BjnNowak for labeling: https://bjnnowak.netlify.app/2021/08/10/r-labelling-area-plots/
# shoutout to @leeolney3 for inspiration and code to modify distributions: https://twitter.com/leeolney3/status/1424947959356182529

# CODE:
rm(list=ls())

library(tidytuesdayR)
library(ggplot2)
library(plyr)
library(ggstream)
library(ggsci)

# data ----
tuesdata <- tidytuesdayR::tt_load(2021, week = 33)
inv <- tuesdata$investment
invest1 <- ddply(inv, .(year, meta_cat), summarise, grossinv = sum(gross_inv))
invest2 <- subset(invest1, meta_cat=="Public safety" |  meta_cat=="Education" |  meta_cat=="Digital" |  meta_cat=="Health" |  meta_cat=="Social")

# Reorder factors (@BjnNowak)
invest2$meta_cat<-factor(invest2$meta_cat,
                         c(
                           "Social",
                           "Digital",
                           "Health",
                           "Education",
                           "Public safety"
                         ))

# get positions (@BjnNowak)
final<-invest2%>%
  filter(year=="2017")%>%              # Keep only 2017 value
  arrange(desc(meta_cat))%>%                # Inverse factor order (first is at the bottom of plot)
  mutate(                              # Create new column ypos and
    ypos=cumsum(grossinv)       # fill with cumulative sum of invest for 2017
  )                                     

# Create color palette [https://applecolors.com/palette/1526-beautiful-pastel-sunrise-logo]
pal<-c("#DFA1A1", "#FDBFBF", "#FFD380", "#80E7E5", "#2E3B65")

# Specify color palette with a new column inside main (@BjnNowak)
invest2<-invest2%>%
  mutate(
    col_lab=case_when(
      meta_cat=="Social"~"#DFA1A1",
      meta_cat=="Digital"~"#FDBFBF",
      meta_cat=="Health"~"#FFD380",
      meta_cat=="Education"~"#80E7E5",
      meta_cat=="Public safety"~"#2E3B65"
    ))

# years
min(invest2$year)
max(invest2$year)

# plot ----
TIDY <- ggplot(data=invest2, aes(x=year, y=grossinv, fill=meta_cat)) +
  geom_stream(bw=0.6) + # (@leeolney3)
  scale_x_continuous(limits=c(1947,2030)) +
  geom_text(data=final, aes(y=ypos-c(450000, 460000, 500000, 525000, 710000), label=meta_cat, color=meta_cat), x=2018, hjust=0, family="Lato", size=12) + #  (@BjnNowak)
  scale_fill_manual(breaks=invest2$meta_cat,values=invest2$col_lab) + # (@BjnNowak)
  scale_color_manual(breaks=invest2$meta_cat,values=invest2$col_lab) + #  (@BjnNowak)
  labs(title="Selected categories of BEA Infrastructure Investment 1947-2017",
       caption="@pyyxxo | data source: Bureau of Economic Analysis | #TidyTuesday 2021/33") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "mintcream", colour = NA),
        text = element_text(family="Lato", colour="gray30"),
        plot.title    = element_text(size = 32, hjust=.5), 
        plot.caption  = element_text(size = 8, hjust=.5, margin = unit(c(10, 0, 0, 0), "mm"), family="Press Start 2P"), 
        plot.margin = unit(c(1, .5, 1, .5),"cm") )

ggsave(TIDY, file="figure/TIDY_2021_33.png", width = 16, height = 9, units = "in", bg="mintcream") 

