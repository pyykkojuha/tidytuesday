# TidyTueday, Week 3, 2021
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-12/readme.md

library(ggplot2)

artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')

TIDY <- ggplot(artwork) + 
  geom_point(aes(x=year, y=1000+width/3, size=height), alpha=.5, color="mediumpurple") +
  geom_bar(aes(x=year), alpha=.7, color="lightpink3", fill="lightpink3") +
  annotate("text", x=1545, y=3700, label="TATE ART MUSEUM", 
           size=36, color="black", family="Aleo", hjust=0, vjust=0, alpha=.75) +
  annotate("text", x=1545, y=3300, label="data: tategallery | visual: @pyyxxo | code: pyyxxo.fi/tt | #TidyTuesday 2021/3", 
           size=6, color="black", family="Aleo", hjust=0, vjust=0, alpha=.75) +
  annotate("text", x=1545, y=2000, label="DIMENSIONS OF ARTWORKS: X = year  Y = width SIZE = height", 
           size=12, color="black", family="Aleo", hjust=0, vjust=0, alpha=.75) +
  annotate("text", x=1545, y= 500, label="NUMBER OF ARTWORKS BY YEAR FROM 1545 TO 2012", 
           size=12, color="black", family="Aleo", hjust=0, vjust=0, alpha=.75) +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family="Aleo", color="mediumslateblue"),
        plot.caption =    element_text(size = 14, hjust=.5), 
        plot.title = element_text(size = 42, hjust=.5), 
        plot.background = element_rect(fill = "honeydew1", colour = NA),
        plot.margin=unit(c(0.05,0.05,0.05,0.05),"cm") )

ggsave(TIDY, file="figure/TIDY_2021_03.png", width = 16, height = 9, units = "in", bg = "honeydew1")
