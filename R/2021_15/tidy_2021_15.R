# TidyTueday, Week 15, 2021 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-06/readme.md

library(ggplot2)
library(harrypotter)

# data ----
forest_area <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest_area.csv')

# edit ----

earth <- 148940000    # km2 https://en.wikipedia.org/wiki/Earth
aEU27 <- 4324782   # https://en.wikipedia.org/wiki/2013_enlargement_of_the_European_Union#Impact
aUSA <-  9147593   # https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_area
aBRA <-  8460415   # https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_area

EU27 <- subset(forest_area, entity=="EU-27" & year=="2020")
EU27$land <- 100*aEU27/earth
BRA <- subset(forest_area, entity=="Brazil" & year=="2020")
BRA$land <- 100*aBRA/earth
USA <- subset(forest_area, code=="USA" & year=="2020")
USA$land <- 100*aUSA/earth

# forest_area	= Percent of global forest area
Forest2020 <- rbind(EU27, BRA, USA)
Forest2020$forest_times_land <- Forest2020$forest_area / Forest2020$land
Forest2020$rank <- rank(Forest2020$forest_times_land)
Forest2020$entity[Forest2020$entity=="United States"] <- "U.S.A."

# plot ----

FONT <- "Source Code Pro ExtraLight"

TIDY <- ggplot(Forest2020, aes(x=rank, y=forest_times_land)) +
  geom_hline(yintercept=c(1,2), color="black", alpha=1) +
  geom_segment(aes(x = rank, xend = rank, y=0, yend = forest_times_land, color = forest_times_land), size=110, alpha=1) +
  annotate(geom="text", y=0.5, x=Forest2020$rank, label=Forest2020$entity, hjust=.5, vjust=.5, angle=0, alpha=1,
           family="Permanent Marker", size=24, color="lightslategray") +
  geom_hline(yintercept=0, color="black", alpha=1) +
  scale_x_continuous(limits=c(.5, max(Forest2020$rank)+.5)) +
  scale_y_continuous(breaks = c(0,1,2), 
                     limits = c(0,2.5),
                     labels = c("0x", "1x", "2x")) +
  scale_colour_hp(house = "Always") +
  labs(title="GLOBAL FOREST AREA x GLOBAL LAND AREA in 2020",
       y="",
       x="",
       caption="Value = (Percent of global forest area by a country) / (Land area of a country / Land area of the world)\n\nSource: Our World in Data + Wikipedia | Graphic: PYYXXO | #TidyTuesday 2021/15") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title.position = "plot",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "lightgoldenrod3", colour = NA), 
        plot.title   = element_text(size = 36, hjust=.5, color="black", family=FONT),
        plot.caption = element_text(size = 14, hjust=.5, color="black", family=FONT), 
        axis.text.y    = element_text(size = 55, hjust=.5, vjust=0, color="black", family=FONT),
        axis.text.x    = element_blank(),
        axis.title   = element_text(size = 24, hjust=.5, color="black", family=FONT),
        plot.margin  = unit(c(1, 3, 1, 3),"cm"))
  
ggsave(TIDY, file="figure/TIDY_2021_15.png", width = 16, height = 9, units = "in")
