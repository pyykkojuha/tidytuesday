# TidyTueday, Week 5, 2021 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-26/readme.md

rm(list=ls())

library(ggplot2)
library(plyr)
library(ggtext)  
library(gridExtra)  

# data ----

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')
cola <- subset(plastics, parent_company=="Pepsico" | parent_company=="PepsiCo" | parent_company == "The Coca-Cola Company")
cola$parent_company[cola$parent_company=="Pepsico"] <- "PepsiCo"
cola <- na.omit(cola[,c("country", "parent_company", "year", "num_events", "volunteers", "grand_total")])
table(cola$parent_company)

cola2 <- ddply(cola, .(parent_company), summarise, 
               events = sum(num_events), 
               volunteers = sum(volunteers), 
               total = sum(grand_total))

# plot ----

PIE  <- ggplot(cola2, aes(x="", y=total, fill=parent_company)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#004B93", "#F40009")) +
  theme_void() +  
  theme(plot.background = element_rect(fill = "black", colour = NA),
        legend.position="none", 
        plot.margin = unit(c(.1, .1, .1, .1),"cm") ) 

TITLE <- ggplot(cola2, aes(y=total , x=events)) + 
  labs(title="WASTE PICKER BRAND AUDITS:<br>PLASTIC COLLECTED<br>2019-2020<br>(ALL DATA)<br>",
       subtitle="<span style='color:#F40009;'>The Coca-Cola<br>Company</span><br>-vs-<br><span style='color:#004B93;'>PepsiCo</span>") +
  theme_void() +
  theme(plot.background = element_rect(fill = "black", colour = NA), 
        text = element_text(family="Source Code Pro", color="white"),
        plot.title  = element_markdown(size=32, hjust=.5),
        plot.subtitle  = element_markdown(size=56, hjust=.5),
        plot.margin = unit(c(3.5, .1, .1, 1),"cm") ) 

CAPTION <- ggplot(cola2, aes(y=total , x=events)) +
  labs(caption="Source: Break Free From Plastic | Graphic: PYYXXO | Code: pyyxxo.fi/tt | #TidyTuesday 2021/5") +
  theme_void() +
  theme(plot.background = element_rect(fill = "black", colour = NA), 
        text = element_text(family="Source Code Pro", color="white"),
        plot.caption  = element_text(size = 14, hjust=.5), 
        plot.margin = unit(c(.1, .1, 1.1, .1),"cm") ) 

LAYOUT <- rbind(c(1,1,1,1,2,2,2,2),
                 c(1,1,1,1,2,2,2,2),
                 c(1,1,1,1,2,2,2,2),
                 c(1,1,1,1,2,2,2,2),
                 c(1,1,1,1,2,2,2,2),
                 c(1,1,1,1,2,2,2,2),
                 c(1,1,1,1,2,2,2,2),
                 c(1,1,1,1,2,2,2,2),
                 c(3,3,3,3,3,3,3,3))

TIDY <- grid.arrange(TITLE, PIE, CAPTION, layout_matrix = LAYOUT)

ggsave(TIDY, file="figure/TIDY_2021_05.png", width = 16, height = 9, units = "in", bg = "black")
