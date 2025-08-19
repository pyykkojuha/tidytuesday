# TidyTuesday, Week 33, 2025
# PYYXXO
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-08-19/readme.md

rm(list=ls())

# libraries ----
library(ggplot2)
library(rnaturalearth)
library(ggtext)
library(sf)
# install.packages(
#   "rnaturalearthhires",
#   repos = "https://ropensci.r-universe.dev",
#   type = "source"
# )
library(rnaturalearthhires)

# data ----
scottish_munros <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-19/scottish_munros.csv')

## edits ----
names(scottish_munros)
names(scottish_munros)[names(scottish_munros) == 1891] <- "y1891"
names(scottish_munros)[names(scottish_munros) == 1921] <- "y1921"
names(scottish_munros)[names(scottish_munros) == 1933] <- "y1933"
names(scottish_munros)[names(scottish_munros) == 1953] <- "y1953"
names(scottish_munros)[names(scottish_munros) == 1969] <- "y1969"
names(scottish_munros)[names(scottish_munros) == 1974] <- "y1974"
names(scottish_munros)[names(scottish_munros) == 1981] <- "y1981"
names(scottish_munros)[names(scottish_munros) == 1984] <- "y1984"
names(scottish_munros)[names(scottish_munros) == 1990] <- "y1990"
names(scottish_munros)[names(scottish_munros) == 1997] <- "y1997"
names(scottish_munros)[names(scottish_munros) == 2021] <- "y2021"

scottish_munros$change_1891_2021 <- NA
scottish_munros$change_1891_2021[scottish_munros$y1891 == "Munro" & scottish_munros$y2021 == "Munro"] <- "Munro 1891 & 2021"
scottish_munros$change_1891_2021[scottish_munros$y1891 == "Munro" & scottish_munros$y2021 != "Munro"] <- "Munro 1891"
scottish_munros$change_1891_2021[scottish_munros$y1891 != "Munro" & scottish_munros$y2021 == "Munro"] <- "Munro 2021"

table(scottish_munros$change_1891_2021, useNA="always")

# check coordinates ----
min(scottish_munros$xcoord, na.rm=TRUE)
max(scottish_munros$xcoord, na.rm=TRUE)
min(scottish_munros$ycoord, na.rm=TRUE)
max(scottish_munros$ycoord, na.rm=TRUE)

# get map ----
# Get UK administrative regions
uk_regions <- ne_states(country = "United Kingdom", returnclass = "sf")

# Reproject to British National Grid (OSGB36, EPSG:27700)
uk_OSGB36 <- st_transform(uk_regions, 27700)

# Plot ----
TIDY <- ggplot(uk_OSGB36) +
  # Plot Scotland
  geom_sf(fill = "#386641", color = "#386641") +
  coord_sf(crs = st_crs(27700)) +
  # Labs
  labs(title = "Change in Scotland Munro Classification", 
       subtitle = "<span style='color:#9e2a2b;'>**Munro in 1891**</span> / <span style='color:#e09f3e;'>**Munro in 1891 & 2021**</span> / <span style='color:#7b2cbf;'>**Munro in 2021**</span>",
       y=NULL, x=NULL,
       caption ="**PYYXXO** | #TidyTuesday 2025/33 | **Data:** Database of British and Irish Hills v18.2") +
  # Points on top of each other
  geom_point(data=subset(scottish_munros, change_1891_2021 == "Munro 1891 & 2021"), aes(x=xcoord, y=ycoord) , col="black", fill="#e09f3e", size=8, alpha=1, shape=24) +
  geom_point(data=subset(scottish_munros, change_1891_2021 == "Munro 1891"),        aes(x=xcoord, y=ycoord) , col="black", fill="#9e2a2b", size=8, alpha=1, shape=24) +
  geom_point(data=subset(scottish_munros, change_1891_2021 == "Munro 2021"),        aes(x=xcoord, y=ycoord) , col="black", fill="#7b2cbf", size=8, alpha=1, shape=24) +
  # Set limits to projection
  scale_x_continuous(limits=c(130000,365000)) +
  scale_y_continuous(limits=c(702000,970000)) +
  # Theme
  theme_bw(base_family ="Tiempos Headline", base_size=18) +
  theme(panel.grid.major = element_line(color = gray(.4), linetype = "dashed", linewidth = 0.5), 
        panel.background = element_rect(fill = "#669bbc"),
        axis.text = element_text(colour = "black"),
        text=element_text(family="Tiempos Headline", size=18),
        plot.title    = element_markdown(family = "Tiempos Headline", color = "black", size=31), 
        plot.subtitle = element_markdown(family = "Tiempos Headline", color = "black", size=22), #, size = rel(1.45)),
        plot.caption  = element_markdown(family = "Tiempos Headline", color = "black", size=12, hjust=.5, margin=unit(c(8, 0,0,0),"mm")),
        legend.position="none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.background =  element_rect(fill = "#faedcd", color = "#faedcd"),
        plot.margin = unit(c(3, 3, 0, 3),"mm"))

ggsave(TIDY, file="figure/TIDY_2025_33.png", width = 12*(2/3), height = 15.5*(2/3), units = "in", bg="#faedcd") 
