# TidyTueday, Week 05, 2022 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-02-01/readme.md

rm(list=ls())

# libraries ----
library(ggplot2)
library(janitor)
library(extrafont)
library(ggtext)  
library(gridExtra)  

# color ----
# https://coolors.co/palette/8ecae6-219ebc-023047-ffb703-fb8500

COLOR1 <- "#8ECAE6"  # rott
COLOR2 <- "#219EBC"  # same
COLOR3 <- "#023047"  # chin
COLOR4 <- "#FFB703"  # bg
COLOR5 <- "#FB8500"  # other
COLOR6 <- "#000000"  # text
FONT1  <- "Anton"
FONT2  <- "Alata"

# data ----
breed_traits      <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
#breed_rank_all    <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank_all.csv')

# subset ----
breed <- breed_traits[,c(1:7,10:17)]
breed <- clean_names(breed)
breed <- subset(breed, affectionate_with_family >0 )
trait <- trait_description[c(1:6,9:16),]

VARS <- names(breed)

# LETTER POSITION / SELECT BREEDS
DOG1 <- subset(breed, breed == "Rottweilers")
DOG2 <- subset(breed, breed == "Chihuahuas")

# LOOP
for(i in 1:14){
  # plot ----
  PLOT <- ggplot(breed) + 
    geom_bar(aes_string(x = VARS[i+1]), fill=COLOR2, color=NA) +
    scale_x_continuous(limits = c(0.5,5.5), breaks=1:5, labels = c(trait$Trait_1[i], "", "<", "", trait$Trait_5[i])) +
    scale_y_continuous(limits = c(0,194), breaks=c(0,194/2,194), labels=c("0%", "50%", "100%")) +
    # TITLE
    labs(x = trait$Trait[i], y = "") +
    # THEME
    theme_minimal() +
    theme(legend.position   = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y      = element_text(size = 8,  hjust=.5, vjust=.5, color=COLOR3, family=FONT2),
          axis.text.x      = element_text(size = 10, hjust=.5, color=COLOR3, family=FONT2),
          axis.title.x     = element_text(size = 16, hjust=.5, color=COLOR3, family=FONT2),
          plot.background  = element_rect(fill = COLOR4, colour = NA),
          plot.margin      = unit(c(5,10,0, 0),"mm"))
  
  # LETTERS TO PLOT
  D1 <- as.numeric(DOG1[1,i+1])
  D2 <- as.numeric(DOG2[1,i+1])
  YY <- 180
  DF <- as.data.frame(cbind(D1,D2,YY))
  
  if(D1 != D2){
    PLOTX <- PLOT + 
      geom_text(data=DF, aes(x=D1,  y=YY), label="R", size=14, family=FONT1, hjust=.5, vjust=1, color=COLOR3) +
      geom_text(data=DF, aes(x=D2,  y=YY), label="C", size=14, family=FONT1, hjust=.5, vjust=1, color=COLOR3)
  }
  if(D1 == D2){
    PLOTX <- PLOT + 
      geom_text(data=DF, aes(x=D2,  y=YY), label="R&C", size=14, family=FONT1, hjust=.5, vjust=1, color=COLOR3)
  }
  assign(paste0("PLOT_", i), PLOTX)
}

# TITLE
TITLE <- ggplot(breed) + 
  geom_bar(aes_string(x = VARS[i+1]), fill=NA, color=NA) +
  scale_x_continuous(limits = c(0.5,5.5), breaks=1, labels = c("")) +
  scale_y_continuous(limits = c(0,194), breaks=c(0,194/2,194), labels=c("", "", "")) +
  geom_text(aes(x=1,  y=194), label="TRAITS: 194 breeds rated 1-5", 
            size=7, family=FONT1, hjust=0, vjust=1, color=COLOR2) +
  geom_text(aes(x=1,  y=194/2), label="[R]ottweilers vs\n[C]hihuahuas", 
            size=9, family=FONT1, hjust=0, vjust=.5, color=COLOR3) +
  geom_text(aes(x=1,  y=0), label="data source: American Kennel Club courtesy of KKakey\n@PYYXXO #TidyTuesday 2022/05", 
            size=3, family=FONT2, hjust=0, vjust=0, color=COLOR1) +
  # THEME
  theme_void() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = COLOR4, colour = NA),
        plot.margin = unit(c(0, 0, 0, 0),"mm"))
  
# PRINT
LAYOUT <- rbind(c( 1, 2, 3, 4, 5),
                c( 6, 7, 8, 9,10),
                c(11,12,13,14,15))

TIDY <- grid.arrange(TITLE, 
                     PLOT_1, 
                     PLOT_2, 
                     PLOT_3, 
                     PLOT_4, 
                     PLOT_5, 
                     PLOT_6, 
                     PLOT_7, 
                     PLOT_8, 
                     PLOT_9, 
                     PLOT_10, 
                     PLOT_11, 
                     PLOT_12, 
                     PLOT_13, 
                     PLOT_14, layout_matrix = LAYOUT)

ggsave(plot=TIDY, file="figure/TIDY_2022_05.png", width = 20, height = 8, units = "in", bg=COLOR4) 
