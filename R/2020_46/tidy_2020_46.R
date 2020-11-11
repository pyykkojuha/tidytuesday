# TidyTueday, Week 46, 2020
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-11-10/readme.md
# https://ourworldindata.org/technology-adoption

library(ggplot2)
library(extrafont)
library(plyr)
library(gridExtra)  # gridarrange

# DATA
mobile <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')
landline <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')

summary(mobile)
summary(landline)

df01 <-   mobile[, c("code", "year", "total_pop", "mobile_subs", "continent")]
df02 <- landline[, c("code", "year", "landline_subs")]
df00 <- merge(df01, df02, by=c("code", "year"))

data <- subset(df00, continent=="Europe" & landline_subs>0 & mobile_subs>0 & year>=1996 & year<=2013)

summary(data)

# FIGURE # # # 
# LEGEND
# extract legend: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legendX <- ggplot(data) + 
  geom_density(aes(x=landline_subs, fill=as.factor(year)), alpha=.1) +
  scale_x_continuous(limits = c(0, 250)) +
  labs(fill = "OurWorldInData.org\n#TidyTuesday 2020/46\n@pyyxxo\n\n\nEUROPEAN\nCOUNTRIES:") +
  guides(fill = guide_legend(override.aes = list(alpha = .9))) +
  theme_minimal() + 
  theme(legend.position = "right",
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 8, family="Open Sans", color="floralwhite"),
        plot.background = element_rect(fill = "black", colour = NA),
        plot.margin=unit(c(1,1,1,1),"cm") )

mylegend<-g_legend(legendX)

# UPPER PLOT
L <- ggplot(data) + 
  geom_density(aes(x=landline_subs, fill=as.factor(year)), alpha=.2) +
  ylab("Telephone") +
  scale_x_continuous(limits = c(0, 210), 
                     breaks = c(0, 50, 100, 150, 200),
                     label = c("0", "50", "[Subscriptions per 100 people]", "150", "200")) +
  xlab("") + labs(fill = "") + theme_minimal() + 
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = 14, family="Open Sans", color="floralwhite"),
        plot.background = element_rect(fill = "black", colour = NA),
        plot.margin=unit(c(1,1,-.4,1),"cm") )

# LOWER PLOT
M <- ggplot(data) + 
  geom_density(aes(x=mobile_subs, fill=as.factor(year)), alpha=.2)  +
  scale_x_continuous(limits = c(0, 210)) +
  scale_y_reverse() +
  ylab("Mobile") +
  xlab("") + labs(fill = "") + theme_minimal() + 
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 14, family="Open Sans", color="floralwhite"),
        plot.background = element_rect(fill = "black", colour = NA),
        plot.margin=unit(c(-.2,1,1,1),"cm") )

# COMBINE 
layout2 <- rbind(c(rep(1,6),3),
                 c(rep(2,6),3))

LM <- grid.arrange(L, M, mylegend, layout_matrix = layout2)

ggsave(LM, file="figure/TIDY_2020_46.png", width = (2/3)*16, height = (2/3)*9, units = "in", bg="black") 
