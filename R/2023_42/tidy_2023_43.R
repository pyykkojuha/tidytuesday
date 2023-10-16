# TidyTueday, Week 42, 2023
# PYYXXO
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-10-17/readme.md

library(ggplot2)
library(viridis)
library(ggtext)

# READIBITY IMPROVEMENT PACKAGE:
# https://github.com/nicucalcea/Ra11y#contrast
# remotes::install_github("nicucalcea/Ra11y")
library(Ra11y)

# fonts & colors ----
font1 <- "VCR OSD Mono"
font2 <- "Optima"
palette1 <- c("#cdb4db", "#ffc8dd", "#ffafcc", "#bde0fe", "#a2d2ff")

# load data ----
taylor_album_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_album_songs.csv')
taylor_all_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_all_songs.csv')
taylor_albums <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_albums.csv')

# data edit ----
## duration to m:ss ----
taylor_album_songs$duration_sec <- round(taylor_album_songs$duration_ms/1000,3)
taylor_album_songs$m <- floor(taylor_album_songs$duration_sec/60)
taylor_album_songs$s <- floor(taylor_album_songs$duration_sec - 60*taylor_album_songs$m)
taylor_album_songs$minsec <- "0:00"
# loop seconds with leading zero
for(i in 1:dim(taylor_album_songs)[1]){
  if(taylor_album_songs$s[i]>=10 & !is.na(taylor_album_songs$s[i])){
    taylor_album_songs$minsec[i] <- paste0(taylor_album_songs$m[i], ":", taylor_album_songs$s[i])
  }
  if(taylor_album_songs$s[i] <10 & !is.na(taylor_album_songs$s[i])){
    taylor_album_songs$minsec[i] <- paste0(taylor_album_songs$m[i], ":0", taylor_album_songs$s[i])
  }
}
# date & album name to help sort by date in y-axis
taylor_album_songs$label <- paste0("*<span style=\"color:#8F3A84;font-size:9px\">",  taylor_album_songs$album_release, 
                        "</span>*<br><span style=\"color:#000000;font-size:18px\">", taylor_album_songs$album_name, "</span>")

# plot ----
TIDY <- ggplot(subset(taylor_album_songs, bonus_track==FALSE), aes(y=label, x=track_number, fill=duration_sec)) +
  # TILES
  geom_tile(color=palette1[3], linewidth=0.75) +
  scale_fill_viridis_b(breaks = seq(3*60, 5*60, 60), option="plasma", labels =c("3:00", "4:00", "5:00")) +
  # TEXT (min:sec value that is legible on the background)
  geom_text(aes(label = minsec, !!!autocontrast), size = 3.4, family=font1) +
  # LABS
  labs(title = "*\"I mean, you only want two and a half minutes if you can get it, you know, three minutes maximum\"* (Someday by Shirley Ann Lee)",
       subtitle = "**TAYLOR SWIFT'S SONG LENGTHS BY AN ALBUM**",
       y = NULL, 
       x = "Track", 
       fill="Length",
       caption="Graph: **PYYXXO** | Source: **taylor R package** | #TidyTuesday **2023/42**") + 
  # SETTINGS
  scale_x_continuous(breaks=1:max(taylor_album_songs$track_number), expand=c(0.01,-0.01)) +
  coord_equal() +
  theme_void(base_family=font2) +
  theme(axis.text.x     = element_text(size = 10, family = font1, angle = 0, hjust = .5, margin = unit(c(0.5, 0, 0, 0),"mm")),
        axis.title.x    = element_text(size = 10, family = font1, hjust = 0.01, vjust = 1,  margin = unit(c(0.5, 0, 3, 0),"mm")),
        axis.text.y     = element_markdown(size = 12, hjust = 1, angle = 0) ,
        axis.title.y    = element_markdown(size = 12, hjust = 1, angle = 90) ,
        plot.title      = element_markdown(size = 19, hjust = 1,  margin = unit(c(0, 0, 1, 0),"mm")) ,
        plot.subtitle   = element_markdown(size = 18, hjust = 1),
        plot.caption    = element_markdown(colour = "gray20",  size = 9, hjust = 1, vjust = .5, family = font1), 
        plot.background = element_rect(color = palette1[3], fill = palette1[3]),
        legend.text     = element_text(size = 10, hjust = 0, vjust = .5, family=font1), 
        legend.position       = "right", 
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        plot.margin     = unit(c(0, 10, 0, 10),"mm"))

ggsave(TIDY, file="figure/TIDY_2023_42.png", width = 24*(2/3), height = 9*(2/3), units = "in") 
