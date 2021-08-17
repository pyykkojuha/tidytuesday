# TidyTueday, Week 34, 2021 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-08-17/readme.md

# libraries ----
library(ggplot2)

# data ----
computer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv')

# edit ----
table(computer$type, computer$char_type)
computer$type[computer$type=="question"] <- "Question"
computer$type[computer$type=="command"] <- "Command"
computer$char_type[computer$char_type=="Person"] <- "PERSON SAYS"
computer$char_type[computer$char_type=="Computer"] <- "COMPUTER SAYS"
table(computer$type, computer$char_type)
df <- as.data.frame(table(computer$type, computer$char_type))

# plot ----
TIDY <- ggplot(df, aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(low = "black", high = "red") +
  labs(title="",
       x="STAR TREK .:. TYPES OF COMMANDS",
       y="",
       fill = "Frequency: ",
       caption="@pyyxxo | data source: SpeechInteraction.org | #TidyTuesday 2021/34") +
  annotate(geom="text", y=0.55, x=1:13, 
           label=df$Var1[1:13], 
           hjust=0, vjust=0, 
           angle=90, alpha=.85,
           size=10, 
           family="Press Start 2P", color="cornflowerblue") +
  guides(fill = guide_colourbar(barheight = unit(1.75 , "cm" ),
                                ticks.colour = "cornflowerblue",
                                ticks.linewidth = 2)) +  # https://stackoverflow.com/questions/21088480/change-ggplot2-colourbar-tick-marks-to-black
  theme_minimal() +
  theme(text = element_text(family="Press Start 2P", colour="cornflowerblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor  = element_blank(),
        legend.key.width = unit(5, 'cm'),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size=18,  vjust=.5),
        legend.text = element_text(size=18), 
        legend.position = "bottom",
        axis.title.x = element_text(size = 34, vjust=0,  hjust=0.55, colour="red"),
        axis.title.y = element_blank(),
        axis.text.x   = element_blank(),
        axis.text.y   = element_text(size = 14, angle=90,  vjust=.5, hjust=0.5, colour="blanchedalmond"),
        plot.background = element_rect(fill = "black", colour = NA),
        plot.title    = element_text(size = 24, hjust=.5), 
        plot.caption  = element_text(size = 8, hjust=.5, colour = "blanchedalmond", margin = unit(c(5, 0, 0, 0), "mm"), family="Press Start 2P"), 
        plot.margin = unit(c(.0, .5, 1, 1),"cm") )

ggsave(TIDY, file="figure/TIDY_2021_34.png", width = 16, height = 9, units = "in", bg="black") 
