# TidyTueday, Week 44, 2021 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-10-26/readme.md

# libraries ----
library(ggplot2)
library(ghibli)

# data ----
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

# edit ----
COLORS <- ghibli_palettes$KikiMedium
FONT <- "Bebas Neue"

# plot ----
TIDY <- ggplot(data=race) +
  geom_segment(aes(x=date, xend=date, y=participants/(-2), yend=participants/2), size=1, alpha=.5, color=COLORS[4]) +
  # LIMITS
  scale_x_date(limits=c(as.Date("2016-01-01"),as.Date("2020-12-31"))) +
  # TITLES
  labs(title="",
       x="Ultra Trail Running Race Participants Timeline 2016-2020",
       y="",
       caption="@PYYXXO | data source: Benjamin Nowak / ITRA | #TidyTuesday 2021/44") +
  # THEME
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor  = element_blank(), 
        legend.position = "none",
        axis.title.x = element_text(size = 48, angle=0,  vjust=.5, hjust=.5, colour=COLORS[6], family=FONT),
        axis.title.y = element_blank(),
        axis.text   = element_blank(),
        plot.background = element_rect(fill = COLORS[5], colour = NA),
        plot.caption  = element_text(size = 14, hjust=.5, colour = COLORS[2], margin = unit(c(5, 0, 0, 0), "mm"), family=FONT), 
        plot.margin = unit(c(0, .5, 1, .5),"cm") )

ggsave(TIDY, file="figure/TIDY_2021_44.png", width = 16, height = 9, units = "in", bg=COLORS[5]) 
