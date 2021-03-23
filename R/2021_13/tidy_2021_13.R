# TidyTueday, Week 13, 2021 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-03-23/readme.md

rm(list=ls())

library(ggplot2)
library(plyr)
library(ggtext)  
library(gridExtra)  
library(extrafont)

# data ----

unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')

UN <- as.data.frame.matrix(table(unvotes$country_code, unvotes$vote))

UN$a0 <- UN$no/(UN$abstain+UN$no+UN$yes)
UN$a1 <- UN$abstain/(UN$abstain+UN$no+UN$yes)
UN$a2 <- UN$yes/(UN$abstain+UN$no+UN$yes)

UN$rank0 <- rank(UN$a0)
UN$rank1 <- rank(UN$a1)
UN$rank2 <- rank(UN$a2)

UN$co = rownames(UN)

UN2 <- reshape(UN, varying=c("a0", "a1", "a2"), timevar="a", idvar="co", v.name=c("prop"), direction="long")

TIDY <- ggplot(UN2, aes(x=rank2, y=prop, fill=as.factor(a))) + 
  geom_bar(stat="identity", width=1) + 
  labs(title="United Nations Votes by Country",
       subtitle="Proportion of <span style='color:coral3;'>no</span>/abstain/<span style='color:darkolivegreen;'>yes</span> votes by country in General Assembly",
       caption="Source: Harvard's Dataverse | Graphic: PYYXXO | Code: pyyxxo.fi/tt | #TidyTuesday 2021/13") +
  scale_fill_manual(values=c("coral3", "#5b92e5", "darkolivegreen")) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#5b92e5", colour = NA), 
        plot.title   = element_text(size = 66, hjust=.5, color="white", family="Aleo Light"),
        plot.subtitle  = element_markdown(size=32, hjust=.5, color="white", family="Aleo Bold"),
        plot.caption = element_text(size = 12, hjust=.5, color="aquamarine1", family="Aleo Regular"), 
        plot.margin  = unit(c(1, 1, 1, 1),"cm"),
        legend.position = "none")

ggsave(TIDY, file="figure/TIDY_2021_13.png", width = 16, height = 9, units = "in", bg = "#5b92e5")
