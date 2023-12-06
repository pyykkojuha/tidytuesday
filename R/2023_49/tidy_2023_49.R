# TidyTueday, Week 49, 2023
# PYYXXO
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-12-05/readme.md

# libraries ----
library(ggplot2)
library(ggtext)

# data ----
life_expectancy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-05/life_expectancy.csv')

# fonts ----
font1 <- "BioRhyme"
font2 <- "Alata"

# plot ----
TIDY <- ggplot(life_expectancy_x, aes(x=Year, y=LifeExpectancy, col=Entity)) +
  # LINES
  geom_line(linewidth=2, alpha=1) +
  # POINTS OF INTEREST
  geom_point(data=subset(life_expectancy_x, Year==1950 | Year == 1969 | Year == 1982 | Year == 1999 | Year == 2021), 
             aes(x=Year, y=LifeExpectancy, col=Entity), alpha=1, size=6) +
  # MW POINT ON TOP 2021
  geom_point(data=subset(life_expectancy_x, Entity == "Malawi" & Year == 2021), aes(x=Year, y=LifeExpectancy, col=Entity), alpha=1, size=6) +
  # NAMES
  geom_text(aes(y=59, x=1951, label="Zambia"), colour="#009E49", hjust=0, vjust=1, family=font2, size=16) +
  geom_text(aes(y=39, x=1976, label="Malawi"), colour="#C8102E", hjust=0, vjust=1, family=font2, size=16) +
  # SCALE & COLOR
  scale_color_manual(values=c("#C8102E", "#009E49")) +
  scale_y_continuous(limits = c(0,80), breaks = seq(0,80,10), sec.axis = dup_axis()) +
  scale_x_continuous(breaks=c(1950, 1969, 1982, 1999, 2021)) +
  # LABS
  labs(title = "**LIFE EXPECTANCY AT BIRTH**",
       y = NULL, 
       x = NULL, 
       caption="Graph: **PYYXXO** | Source: **Our World in Data Life Expectancy Report** | #TidyTuesday **2023/49**") + 
  # SETTINGS
  theme_minimal(base_family = font2) +
  theme(axis.text.x     = element_text(size = 18, family = font2, angle = 0, hjust = .5, colour="white", margin = unit(c(0, 0, 5, 0),"mm")),
        axis.text.y     = element_text(size = 24, family = font2, angle = 0, hjust = 1,  colour="white") ,
        plot.title      = element_markdown(size = 40, hjust = 1,  colour="white", margin = unit(c(0, 0, 1, 0),"mm")) ,
        plot.caption    = element_markdown(colour = "gray70",  size = 9, hjust = 1, vjust = .5, family = font1), 
        plot.background = element_rect(color ="gray5", fill ="gray5"),
        legend.position       = "none", 
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour="gray20"),
        plot.margin      = unit(c(10, 10, 10, 10),"mm"))

ggsave(TIDY, file="figure/TIDY_2023_49.png", width = 16*(2/3), height = 16*(2/3), units = "in") 
