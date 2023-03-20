# TidyTueday, Week 12, 2023 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-03-21/readme.md

rm(list=ls())

# libraries ----
library(ggplot2)
library(gridExtra) # arrangeGrob
library(cowplot) # plot_grid

# data ----
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-21/languages.csv')

# stats subset
statssw <- subset(languages, pldb_id=="r" |
                             pldb_id=="spss" |
                             pldb_id=="sas" |
                             pldb_id=="stata")

# key variables
statssw <- statssw[,c("title", 
                      "appeared", 
                      "language_rank", 
                      "isbndb", 
                      "semantic_scholar", 
                      "github_language_repos", 
                      "wikipedia_daily_page_views", 
                      "wikipedia_backlinks_count", 
                      "wikipedia_revision_count", 
                      "number_of_users")]

# normalized values
statssw$var1 <- statssw$isbndb / max(statssw$isbndb)
statssw$var2 <- statssw$semantic_scholar / max(statssw$semantic_scholar)
statssw$var3 <- statssw$github_language_repos / max(statssw$github_language_repos, na.rm=T)
statssw$var4 <- statssw$wikipedia_daily_page_views / max(statssw$wikipedia_daily_page_views)
statssw$var5 <- statssw$wikipedia_revision_count / max(statssw$wikipedia_revision_count)
statssw$var6 <- statssw$number_of_users / max(statssw$number_of_users)

statssw$order <- order(statssw$language_rank)

# variables desciptions
desc1 <- "* Books"
desc2 <- "** Papers"
desc3 <- "GitHub Repos"
desc4 <- "*** Page views"
desc5 <- "*** Page revisions"
desc6 <- "Users"

# color/font ----
# https://coolors.co/
FONT   <- "Aleo"
COLOR1      <- "#e63946"
COLOR2      <- "#000000"
COLOR_Stata <- "#f1faee"
COLOR_SAS   <- "#a8dadc"
COLOR_SPSS  <- "#457b9d"
COLOR_R     <- "#1d3557"

# PLOT -----
VARS <- c("var1", "var2", "var3", "var4", "var5", "var6")
DESCS <- c(desc1, desc2, desc3, desc4, desc5, desc6)

for(i in 1:6){
  TIDY_TEMP <- ggplot(statssw, aes(x=order, y=.data[[VARS[i]]], color=title, fill=title)) +
    annotate("text", x=0, y=1, label= DESCS[i], vjust=1, hjust=1, size=6, family=FONT, angle=90) + 
    geom_col() +
    scale_y_continuous(limits=c(0,1)) +
    scale_x_continuous(breaks=statssw$order, labels=statssw$title, limits=c(0,4.5)) +
    scale_fill_manual( values = c(COLOR_Stata, COLOR_SAS, COLOR_SPSS, COLOR_R)) +
    scale_color_manual(values = c(COLOR_Stata, COLOR_SAS, COLOR_SPSS, COLOR_R)) +
    theme_void() +
    theme(legend.position = "none", 
          plot.background  = element_rect(colour = COLOR1, fill = COLOR1),
          panel.background = element_rect(colour = COLOR1, fill = COLOR1),
          axis.text.x  = element_text(hjust = .5, vjust=.5, colour = COLOR2, size = 16, family = FONT, angle=0),
          plot.margin = unit(c(2, 2, 2, 2),"mm"))
  
  assign(paste0("TIDY_TEMP_", i), TIDY_TEMP)
}

# TITLE
TIDY_TITLE <- ggplot() +
  labs(title="STATISTICAL SOFTWARES: Relative popularity on six scales",
       subtitle="Figures show comparison to the largest value of the four statistical languages on each subscale, respectively.") +
  #THEME:
  theme_void() +
  theme(legend.position = "none", 
        plot.background  = element_rect(colour = COLOR1, fill = COLOR1),
        panel.background = element_rect(colour = COLOR1, fill = COLOR1),
        plot.title    = element_text(hjust = 0, colour = COLOR2, size = 24, family = FONT),
        plot.subtitle = element_text(hjust = 0, colour = COLOR2, size = 15, family = FONT),
        plot.margin = unit(c(2, 2, 2, 2),"mm"))

# TITLE
TIDY_CAPTION<- ggplot() +
  labs(title="Graphic: P Y Y X X O /// Source:  Programming Language DataBase (with data from: * ISBNdb, ** Semantic Scholar, *** Wikipedia) /// #TidyTuesday 2023/12") +
  #THEME:
  theme_void() +
  theme(legend.position = "none", 
        plot.background  = element_rect(colour = COLOR1, fill = COLOR1),
        panel.background = element_rect(colour = COLOR1, fill = COLOR1),
        plot.title    = element_text(hjust = .5, colour = COLOR2, size = 11, family = FONT),
        plot.margin = unit(c(2, 2, 2, 2),"mm"))

# COMBINE
TIDY <- plot_grid(TIDY_TITLE, 
                  arrangeGrob(TIDY_TEMP_1, TIDY_TEMP_2, TIDY_TEMP_3, TIDY_TEMP_4, TIDY_TEMP_5, TIDY_TEMP_6, nrow=2), 
                  TIDY_CAPTION,
                  axis="l", align = "hv", nrow = 3, rel_heights = c(1/9, 7.5/9, .5/9))

ggsave(TIDY, file="figure/TIDY_2023_12.png", width = 16*(2/3), height = 9*(2/3), units = "in", bg = COLOR1) 

#end
