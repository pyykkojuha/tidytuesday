# TidyTueday, Week 46, 2021 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-11-09/readme.md

# https://github.com/afrimapr
# https://github.com/afrimapr/afrilearndata  [BASE CODE COPIED FROM HERE]
# https://github.com/afrimapr/afrihealthsites
# https://afrimapr.github.io/afrilearndata/
# https://afrimapr.github.io/afrimapr.website/
# https://afrimapr-book.netlify.app/
# https://github.com/afrimapr/afrilearnr

rm(list=ls())

# libraries ----
library(tmap)
library(mapview)

# data ----
#remotes::install_github("afrimapr/afrilearndata")
library(afrilearndata)

# afripop2020: modelled population density 2020 per square km from WorldPop aggregated to mean per 20km squares

# plot ----
TIDY <- tm_shape(afripop2020) +
  tm_raster(palette = viridis::inferno(5), breaks=c(0,2,20,200,2000,50000))  +
  tm_layout(frame = FALSE,
            fontfamily = "Abel",
            legend.outside = TRUE, 
            legend.outside.position = "right",
            legend.title.color = "gray70",
            legend.title.size = 1.25, 
            legend.text.color = "gray70",
            legend.text.size = 1, 
            main.title = "AFRICA POPULATION DENSITY",
            main.title.size = 3,
            main.title.color = "gray90", 
            title = "@PYYXXO\ndata: afrimapr/afrilearndata/afripop2020\n#TidyTuesday 2021/46",
            title.size = 125,
            title.position = c("left", "bottom"),
            title.color = "gray70",
            bg.color="gray10")

tmap_save(TIDY, filename="figure/TIDY_2021_46.png", width = 16, height = 9, units = "in")
