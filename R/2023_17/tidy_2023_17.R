# TidyTueday, Week 17, 2023 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-04-25/readme.md

# libraries ----
library(ggplot2)

# data ----
london_marathon <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/london_marathon.csv')

# fonts ----
FONT1 <- "Rubik Mono One"
FONT2 <- "Atkinson Hyperlegible Bold"

# colors ----
# https://coolors.co/palette/220901-621708-941b0c-bc3908-f6aa1c
COL1 <- "#220901"
COL2 <- "#621708"
COL3 <- "#941B0C"
COL4 <- "#BC3908"
COL5 <- "#F6AA1C"

# plot ----
TIDY <- ggplot(subset(london_marathon, Year<2020)) +
  geom_area(aes(x=Year, y=Accepted)  , col = NA, fill = COL2) +
  geom_area(aes(x=Year, y=Starters)  , col = NA, fill = COL3) +
  geom_area(aes(x=Year, y=Finishers) , col = NA, fill = COL4) +
  geom_text(label="Accepted",  y=50000, x=1982, hjust=0, vjust=0, family=FONT1, size=8, col=COL2) +
  geom_text(label="Starters",  y=47500, x=1982, hjust=0, vjust=0, family=FONT1, size=8, col=COL3) +
  geom_text(label="Finishers", y=45000, x=1982, hjust=0, vjust=0, family=FONT1, size=8, col=COL4) +
  geom_text(label="London Marathon",                                y=10000, x=2018, hjust=1, vjust=0, family=FONT1, size=11, col=COL5) +
  geom_text(label="Runners 1981-2019",                              y=7500,  x=2018, hjust=1, vjust=0, family=FONT1, size=8,  col=COL5) +
  geom_text(label="Graphic: PYYXXO",                                y=5000,  x=2018, hjust=1, vjust=1, family=FONT2, size=3,  col=COL5) +
  geom_text(label="Data: Nicola Rennie's LondonMarathon R package", y=4000,  x=2018, hjust=1, vjust=1, family=FONT2, size=3,  col=COL5) +
  geom_text(label="#TidyTuesday 2023/17",                           y=3000,  x=2018, hjust=1, vjust=1, family=FONT2, size=3,  col=COL5) +
  theme_void() +
  theme(plot.background = element_rect(fill = COL1, colour = NA),
        plot.margin     = unit(c(-8, -8, -8, -8),"mm"))

ggsave(TIDY, file="figure/TIDY_2023_17.png", width = 9*(2/3), height = 9*(2/3), units = "in")
