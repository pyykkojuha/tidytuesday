# TidyTueday, Week 29, 2022 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-07-19/readme.md

# libraries ----
library(ggplot2)

# data ----
technology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')

# color/font ----
BLUE  <- "#002ea2"
WHITE <- "#FFFFFF"
FONT  <- "Roboto Mono"

# data edit ----
FIN <- subset(technology, iso3c == "FIN" & label=="People with internet access" & year>1990)
M1 <- 1996.990
M2 <- 2000.290
M3 <- 2001.790
M4 <- 2005.365
M5 <- 2019.390

# plot ----
TIDY <- ggplot(FIN) +
  geom_line(aes(x=year, y=value, group=label), color=WHITE, size=2.5) +
  #TEXT
  annotate(geom="text", y=5000000, x=1991, 
           label="People with internet\naccess in Finland",
           hjust=0, vjust=1, size=10, 
           family=FONT, color=WHITE) +
  annotate(geom="text", y=4000000, x=1991, 
           label="Source: NBER, CHAT dataset, doi: 10.3386/w15319\nGraphic: P Y Y X X O | #TidyTuesday 2022/29",
           hjust=0, vjust=1, size=3, 
           family=FONT, color=WHITE) +
  annotate(geom="text", y=100000, x=M1-.25,
           label="1M\n1997",
           hjust=1, vjust=0, size=6, 
           family=FONT, color=WHITE) +
  annotate(geom="text", y=100000, x=M2-.25, 
           label="2M\n2001",
           hjust=1, vjust=0, size=6, 
           family=FONT, color=WHITE) +
  annotate(geom="text", y=100000, x=M3+.25, 
           label="3M\n2002",
           hjust=0, vjust=0, size=6, 
           family=FONT, color=WHITE) +
  annotate(geom="text", y=100000, x=M4+.25, 
           label="4M\n2006",
           hjust=0, vjust=0, size=6, 
           family=FONT, color=WHITE) +
  annotate(geom="text", y=100000, x=M5-.25, 
           label="5M\n2020",
           hjust=1, vjust=0, size=6, 
           family=FONT, color=WHITE) +
  #LINES
  geom_segment(aes(x = M1,   y = 0, xend = M1,   yend = 1000000), col=WHITE, size=1) +
  geom_segment(aes(x = M2,   y = 0, xend = M2,   yend = 2000000), col=WHITE, size=1) +
  geom_segment(aes(x = M3,   y = 0, xend = M3,   yend = 3000000), col=WHITE, size=1) +
  geom_segment(aes(x = M4,   y = 0, xend = M4,   yend = 4000000), col=WHITE, size=1) +
  geom_segment(aes(x = M5,   y = 0, xend = M5,   yend = 5000000), col=WHITE, size=1) +
  geom_segment(aes(x = 1991,   y = 0, xend = 2020,   yend = 0), col=WHITE, size=0.5) +
  #THEME
  theme_void() +
  theme(plot.background = element_rect(fill = BLUE, colour = BLUE), 
        plot.margin     = unit(c(4, 4, 4, 4),"mm"))

ggsave(TIDY, file="figure/TIDY_2022_29.png", width = 15*(2/3), height = 10*(2/3), units = "in") 
