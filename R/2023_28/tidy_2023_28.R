# TidyTueday, Week 28, 2023
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-07-11/readme.md

library(ggplot2)
library(ggforce) # geom_circle
library(ggtext)

# DATA
tuesdata <- tidytuesdayR::tt_load(2023, week = 28)
zonann_temps <- tuesdata$zonann_temps

# COLORS THANKS TO COOLORS.CO
COLORS_ZON <- c("#797d62", "#9b9b7a", "#d9ae94", "#E5C59E", "#f1dca7", "#ffcb69", "#d08c60", "#B58463")

# EDIT
zonann_temps_scaled = zonann_temps[,c(1,8:15)]
zonann_temps_scaled$Year2 <- -90 + 180*(zonann_temps_scaled$Year-1880)/(2022-1880)
zonann_temps_scaled[,2] <- zonann_temps_scaled[,2] * 25
zonann_temps_scaled[,3] <- zonann_temps_scaled[,3] * 25
zonann_temps_scaled[,4] <- zonann_temps_scaled[,4] * 25
zonann_temps_scaled[,5] <- zonann_temps_scaled[,5] * 25
zonann_temps_scaled[,6] <- zonann_temps_scaled[,6] * 25
zonann_temps_scaled[,7] <- zonann_temps_scaled[,7] * 25
zonann_temps_scaled[,8] <- zonann_temps_scaled[,8] * 25
zonann_temps_scaled[,9] <- zonann_temps_scaled[,9] * 25

# PLOT
LABEL <- "DESCRIPTION: Global surface temperatures anomalies by latitude (64N-90N, 44N-64N, 24N-44N, EQU-24N, 24S-EQU, 44S-24S, 64S-44S, 90S-64S, colored from top to bottom, respectively) from 1880 to 2022, i.e. deviations from the corresponding \n1951-1980 means which are visualized as vertical white line. | SOURCE: GISTEMP Team, 2023: GISS Surface Temperature Analysis (GISTEMP), version 4. NASA Goddard Institute for Space Studies. | #TidyTuesday 2023/28 | GRAPHIC: P Y Y X X O"

TIDY2 <- ggplot(zonann_temps_scaled) +
  # BACKGROUND SEGEMENTS (LATITUDES)
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin =   64, ymax =  Inf), fill = COLORS_ZON[8]) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin =   44, ymax =   64), fill = COLORS_ZON[7]) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin =   24, ymax =   44), fill = COLORS_ZON[6]) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin =    0, ymax =   24), fill = COLORS_ZON[5]) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin =  -24, ymax =    0), fill = COLORS_ZON[4]) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin =  -44, ymax =  -24), fill = COLORS_ZON[3]) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin =  -64, ymax =  -44), fill = COLORS_ZON[2]) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax =  -64), fill = COLORS_ZON[1]) +
  # GLOBE
  geom_circle(aes(x0=0, y0=0, r=90), col=NA, fill="#242220", alpha=.5, size=1) +
  # MEAN LINE
  geom_segment(aes(x=-90, xend=90, y=0, yend=0), col="seashell1", size=.1) +
  # LATIRUDE CURVES
  geom_line(aes(x=Year2, y=`64N-90N`), col=COLORS_ZON[8]) +
  geom_line(aes(x=Year2, y=`44N-64N`), col=COLORS_ZON[7]) +
  geom_line(aes(x=Year2, y=`24N-44N`), col=COLORS_ZON[6]) +
  geom_line(aes(x=Year2, y=`EQU-24N`), col=COLORS_ZON[5]) +
  geom_line(aes(x=Year2, y=`24S-EQU`), col=COLORS_ZON[4]) +
  geom_line(aes(x=Year2, y=`44S-24S`), col=COLORS_ZON[3]) +
  geom_line(aes(x=Year2, y=`64S-44S`), col=COLORS_ZON[2]) +
  geom_line(aes(x=Year2, y=`90S-64S`), col=COLORS_ZON[1]) +
  # TEXT
  geom_text(aes(y=-100, x=-100*16/9, label = LABEL), family="Sora", col="#242220", size=2, hjust=0, vjust=0) +
  # SETTINGS
  scale_y_continuous(limits=c(-100,100)) +
  scale_x_continuous(limits=c(-100*16/9,100*16/9)) +
  coord_fixed() +
  theme_void() +
  theme(plot.margin= unit(c(-9/2, -16/2, -9/2, -16/2),"mm"))

ggsave(TIDY, file="figure/TIDY_2023_28.png", width = 16*(2/3), height = 9*(2/3), units = "in") 
