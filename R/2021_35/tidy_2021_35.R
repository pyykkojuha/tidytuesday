# TidyTueday, Week 35, 2021 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-08-24/readme.md

# libraries ----
library(ggplot2)

# data ----
lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')

# edit ----
newborns <- subset(lemurs, age_at_wt_d == 0 & !is.na(weight_g) & sex != "ND")
table(newborns$birth_type)
length(newborns$weight_g)

# plot ----
FONT1 <- "Atkinson Hyperlegible Regular"
FONT2 <- "Atkinson Hyperlegible Bold"

TIDY <- ggplot(data = newborns) +
  annotate("rect", xmin = -25, xmax = 175, ymin = -0.005, ymax = 0.0275, fill="darkolivegreen2", alpha = 1) +
  stat_density(aes(x = weight_g), 
               fill="deepskyblue2",
               alpha=.8,
               bw=2.5) +
  scale_y_continuous(limits=c(-0.01, 0.02825)) +
  scale_x_continuous(limits=c(-30, 180)) +
  geom_segment(aes(x = 0,   y = -0.001, xend = 0,   yend = 0.001)) +
  geom_segment(aes(x = 50,  y = -0.001, xend = 50,  yend = 0.001)) +
  geom_segment(aes(x = 100, y = -0.001, xend = 100, yend = 0.001)) +
  geom_segment(aes(x = 150, y = -0.001, xend = 150, yend = 0.001)) +
  annotate(geom="text", x=75,  y=0.025,  hjust=.5, vjust=1, angle=0, alpha=1, family=FONT2, size=26, color="black",
           label="BABY LEMURS") +
  annotate(geom="text", x=75,  y=0.0215,  hjust=.5, vjust=1, angle=0, alpha=1, family=FONT1, size=12, color="gray30",
           label="captive-born birthweight (n = 415)") +
  annotate(geom="text", x=0,   y=-0.002, label="0 g",   hjust=.5, vjust=1, angle=0, alpha=1, family=FONT1, size=8.5, color="black") +
  annotate(geom="text", x=50,  y=-0.002, label="50 g",  hjust=.5, vjust=1, angle=0, alpha=1, family=FONT1, size=8.5, color="black") +
  annotate(geom="text", x=100, y=-0.002, label="100 g", hjust=.5, vjust=1, angle=0, alpha=1, family=FONT1, size=8.5, color="black") +
  annotate(geom="text", x=150, y=-0.002, label="150 g", hjust=.5, vjust=1, angle=0, alpha=1, family=FONT1, size=8.5, color="black") +
  labs(caption= "Source: Duke Lemur Center\n#TidyTuesday 2021/35\n@pyyxxo") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "ghostwhite", colour = NA), 
        plot.title   = element_blank(),
        plot.subtitle= element_blank(),
        plot.caption = element_text(size = 14, hjust=.5, color="gray20", family=FONT1),
        plot.margin  = unit(c(0, 0, 8, 0),"mm"))

ggsave(TIDY, file="figure/TIDY_2021_35.png", width = 9, height = 12, units = "in")
