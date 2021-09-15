# TidyTueday, Week 38, 2021 ----
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-09-14/readme.md

# libraries ----
library(ggplot2)
library(ghibli)

# data ----
billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')

# edit ----
thedream <- subset(billboard, performer == "The-Dream")
thedream$date <- as.Date(thedream$week_id, format = "%m/%d/%Y")
table(thedream$song)

lovehate <- subset(thedream, song == "Falsetto" | song == "I Luv Your Girl" | song == "Shawty Is A 10")
table(lovehate$song)

SINGLE1 <- "Shawty Is A 10"
SINGLE2 <- "Falsetto"
SINGLE3 <- "I Luv Your Girl"
          
lovehate$top <- 0
lovehate$top[lovehate$song == SINGLE1 & lovehate$week_position == c(min(thedream$week_position[thedream$song == SINGLE1]))] <- 1
lovehate$top[lovehate$song == SINGLE2 & lovehate$week_position == c(min(thedream$week_position[thedream$song == SINGLE2]))] <- 1
lovehate$top[lovehate$song == SINGLE3 & lovehate$week_position == c(min(thedream$week_position[thedream$song == SINGLE3]))] <- 1

COLORX<- ghibli_palettes$LaputaMedium[1:4] 
COLORS <- ghibli_palettes$LaputaMedium[5:7]   
FONT <- "VCR OSD Mono"

MONTHS <- c(as.Date("2007-09-01"),
              as.Date("2007-10-01"),
              as.Date("2007-11-01"),
              as.Date("2007-12-01"),
              as.Date("2008-01-01"),
              as.Date("2008-02-01"),
              as.Date("2008-03-01"),
              as.Date("2008-04-01"),
              as.Date("2008-05-01"),
              as.Date("2008-06-01"),
              as.Date("2008-07-01"),
              as.Date("2008-08-01"),
              as.Date("2008-09-01"),
              as.Date("2008-10-01"))

MONTHNAMES <- c("Sep 2007",
                "Oct 2007",
                "Nov 2007",
                "Dec 2007",
                "Jan 2008",
                "Feb 2008",
                "Mar 2008",
                "Apr 2008",
                "May 2008",
                "Jun 2008",
                "Jul 2008",
                "Aug 2008",
                "Sep 2008",
                "Oct 2008")

LINEDATA <- data.frame(cbind(MONTHNAMES, MONTHS, rep(-100,length(MONTHS)), rep(-1,length(MONTHS))))
colnames(LINEDATA) <- c("MONTHNAMES", "DATE", "HUNDRED", "ONE")
LINEDATA$DATE <- as.Date(MONTHS)

# plot ----
TIDY <- ggplot(data=lovehate, aes(x=date, y=-1*week_position)) +
  # LIMITS
  scale_x_date(limits=c(as.Date("2007-08-21"),as.Date("2008-10-01"))) +
  scale_y_continuous(limits=c(-100,-1)) +
  # TITLES
  labs(title="",
       x="THE-DREAM's LOVE/HATE in Billboard Hot 100",
       y="",
       caption="@PYYXXO | data source: Data.World | #TidyTuesday 2021/38") +
  # MONTHS
  annotate(geom="text", y=-95, x=as.Date(LINEDATA$DATE[1:13]+15), 
           label=LINEDATA$MONTHNAMES[1:13], 
           hjust=1, vjust=0.5, angle=270, alpha=1, size=11, family=FONT, color=COLORX[4]) +
  geom_segment(data=LINEDATA, aes(x=MONTHS-.5, xend=MONTHS-.5, y=as.numeric(HUNDRED), yend=as.numeric(ONE)), size=.75, alpha=1, color=COLORX[4], linetype='dashed') + 
  # 1 & 100 LINES & TEXT
  geom_hline(yintercept=c(-1,-100), color=COLORX[4], alpha=1, size=2) +
  annotate(geom="text", y=-99.1, x=as.Date("2007-08-30"), 
           label="#100", 
           hjust=1, vjust=0, angle=0, alpha=.85, size=11, family=FONT, color=COLORX[4]) +
  annotate(geom="text", y=-1.9, x=as.Date("2007-08-30"), 
           label="#1", 
           hjust=1, vjust=1, angle=0, alpha=.85, size=20, family=FONT, color=COLORX[4]) +
  # SINGLE TITLES
  annotate(geom="text", y=-9, x=as.Date(lovehate$date[lovehate$song == SINGLE1 & lovehate$top == 1]), 
           label=SINGLE1, 
           hjust=.5, vjust=.5, angle=0, alpha=1, size=16, family=FONT, color=COLORS[3]) +
  annotate(geom="text", y=-22, x=as.Date(lovehate$date[lovehate$song == SINGLE2 & lovehate$top == 1]), 
           label=SINGLE2, 
           hjust=.5, vjust=.5, angle=0, alpha=1, size=16, family=FONT, color=COLORS[1]) +
  annotate(geom="text", y=-12, x=as.Date(lovehate$date[lovehate$song == SINGLE3 & lovehate$top == 1]), 
           label=SINGLE3, 
           hjust=.5, vjust=.5, angle=0, alpha=1, size=16, family=FONT, color=COLORS[2]) +
  # POSITION LINES & POINTS
  geom_line(aes(color=song, group=song)) +
  geom_point(aes(color=song, group=song, size=as.factor(top), shape=as.factor(top))) +
  # PEAK POSITIONS POINTS
  annotate(geom="text", y=-lovehate$week_position[lovehate$top== 1], x=as.Date(lovehate$date[lovehate$top== 1]),
           label=lovehate$week_position[lovehate$top== 1], 
           hjust=.5, vjust=.5, angle=0, alpha=1, size=5.75, family=FONT, color=COLORX[3]) +
  # MANUAL COLOR/SHAPE/SIZE
  scale_shape_manual(values=c(20,19)) +
  scale_size_manual(values=c(3,11)) +
  scale_color_manual(values = COLORS) +
  # THEME
  theme_void() +
  theme(text = element_text(family=FONT, colour=COLORX[1]),
        panel.grid.major = element_blank(),
        panel.grid.minor  = element_blank(), 
        legend.position = "none",
        axis.title.x = element_text(size = 42, angle=0,  vjust=.5, hjust=.5, colour="ghostwhite"),
        axis.title.y = element_blank(),
        axis.text   = element_blank(),
        plot.background = element_rect(fill = COLORX[3], colour = NA),
        plot.caption  = element_text(size = 14, hjust=.5, colour = COLORX[4], margin = unit(c(5, 0, 0, 0), "mm"), family=FONT), 
        plot.margin = unit(c(0, .5, 1, .5),"cm") )

ggsave(TIDY, file="figure/TIDY_2021_38.png", width = 16, height = 9, units = "in", bg=COLORX[3]) 
