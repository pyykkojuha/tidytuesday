# TidyTueday, Week 4, 2021
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-19/readme.md

rm(list=ls())

library(ggplot2)
library(Hmisc) #capitalize

# data
gender <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/gender.csv')
crops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/crops.csv')
households <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/households.csv')

#
# #
# # #

# CLEAN NAD MAP
# https://web.jackv.xyz/2021/01/18/tidy-tuesday-01-19-2021-kenyas-census/

  remotes::install_github("Shelmith-Kariuki/rKenyaCensus")
  k_shp <- rKenyaCensus::KenyaCounties_SHP
  library(tidyverse)
  library(sf)
  library(magrittr)

  clean_county <- function(X) {
    X %>%
      tolower %>%
      str_replace_all("[^[:alpha:]]+", "") %>%
      str_replace_all(fixed("city"), "")
  }

  households %<>% mutate(County = clean_county(County))
  crops %<>% mutate(County = clean_county(SubCounty))
  gender %<>% mutate(County = clean_county(County))

  k_shp %>%
    as("sf") %>%
    select(-Population) %>%
    mutate(County = clean_county(County)) %>%
    inner_join(households, by="County") %>%
    inner_join(gender, by="County") %>%
    inner_join(crops, by="County") ->
    k_data

#
# #
# # #

# CENTROIDS OF COUNTY
# https://stackoverflow.com/questions/46176660/how-to-calculate-centroid-of-polygon-using-sfst-centroid

  k_data$centroids <- st_transform(k_data) %>% 
    st_centroid() %>% 
    st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
    st_geometry()
    
#
# #
# # #

# CENTROIDS AS DATA FRAME
# https://stackoverflow.com/questions/64688622/sf-to-data-frame-why-as-spatial-needed-before-as-data-frame

  k_data_2 <- k_data %>% st_centroid() %>%  
    as_Spatial() %>%                
    as.data.frame()
    
#
# #
# # #

# SELECT COUNTYS TO NAME  
topcoffee <- subset(k_data_2, Coffee>40000)
topcoffee$County <- capitalize(topcoffee$County)
topcoffee[, c("County", "coords.x1", "coords.x2")]
topcoffee$County[topcoffee$County=="Muranga"] <- "Murang'a"

#
# #
# # #

# PLOT
library(colorspace) # http://colorspace.r-forge.r-project.org/articles/ggplot2_color_scales.html
library(ggrepel)    # https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html

TIDY <- ggplot(k_data) + 
    geom_sf(aes(fill=Coffee), color="black") +
   geom_text_repel(data = topcoffee, aes(x = coords.x1, y = coords.x2, label = County), 
                   family="Source Code Pro", size=8, color="firebrick4", 
                             #  Kiri  / Kisii / Muranga / Nyeri
                   nudge_x = c(+400000,   -5000,  -50000, +100000), 
                   nudge_y = c(+600000, -200000, -400000, +600000) ) +
   labs(title = "KENYA x COFFEE", 
        subtitle="Population growing/farming coffee by county",
        fill="",
        caption="Data: rKenyaCencus | Code: @pyyxxo | #TidyTuesday 2021/4") +
  scale_fill_continuous_sequential(palette = "Heat") +
   theme_void() + 
   theme(legend.position = "left",
         legend.text=element_text(size=16),
         text = element_text(family="Source Code Pro", color="black"),
         plot.caption  = element_text(size = 14, hjust=.5), 
         plot.title    = element_text(size = 32, hjust=0), 
         plot.subtitle = element_text(size = 24, hjust=0, color="gray30"), 
         plot.background = element_rect(fill = "tan", colour = NA),
         plot.margin=unit(c(0.5, 0, 0.5, 0),"cm") )

ggsave(TIDY, file="figure/TIDY_2021_04.png", width = 16, height = 9, units = "in", bg = "tan")
