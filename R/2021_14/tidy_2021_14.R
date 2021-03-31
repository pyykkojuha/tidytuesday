#
# @SushGoplan
#

library(tidyverse)
library(ggplot2)

tuesdata <- tidytuesdayR::tt_load('2021-03-30')

sephora <- tuesdata$sephora
ulta <- tuesdata$ulta
shades <- tuesdata$allShades
numbers <- tuesdata$allNumbers
categories <- tuesdata$allCategories

#rose_gold <- alpha("#b76e79",0.4)

shadesdata <- shades %>% 
  add_count(brand, sort = TRUE) %>% 
  filter(n %in% head(unique(n), 18)) %>% 
  arrange(-lightness,-hue) %>% 
  group_by(brand) %>% 
  mutate(x0 = row_number(),
         x1 = row_number() + 5,
         y0 = 0, 
         y1 = 1)

#
# @pyyxxo
#

rose_gold <- "#b76e79"

TIDY <- ggplot(shadesdata) + 
  geom_rect(aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = hex)) +
  theme_void() +
  scale_fill_identity()  +
  facet_wrap(~brand, ncol = 5) + 
  theme(text=element_text(family="mono"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = rose_gold, color = rose_gold, linetype = 'blank'), 
        strip.background = element_rect(fill = rose_gold, linetype = 'blank', color = rose_gold, size = 0),
        panel.border = element_blank(),
        panel.spacing = unit(0, "lines")) + 
  ggtitle("Shade ranges for some popular brands") +
  labs(caption = "Modified from @SushGopalan by @pyyxxo")

ggsave(TIDY, file="TIDY_2021_14.png", width = 12, height = 6, units = "in", bg = rose_gold)
