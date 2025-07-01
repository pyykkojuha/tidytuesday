# TidyTueday, Week 26, 2025
# PYYXXO
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-07-01/readme.md

# libraries ----
library(ggplot2)
library(ggtext)
library(dplyr) #left_join

# data ----
weekly_gas_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-01/weekly_gas_prices.csv')

# review ---
names(weekly_gas_prices)
length(unique(weekly_gas_prices$date))
table(weekly_gas_prices$fuel, weekly_gas_prices$formulation)
table(weekly_gas_prices$fuel, weekly_gas_prices$grade)
table(weekly_gas_prices$grade, weekly_gas_prices$formulation)

# edit ----
## Select regular fuel
comp <- subset(weekly_gas_prices, ((fuel=="gasoline" & formulation=="all" & grade=="all") | (fuel=="diesel" & grade=="all")))
comp <- comp[,c("date", "fuel", "price")]

## Wide format ----
comp_gas <- subset(comp, fuel=="gasoline")[,c("date", "price")]
  names(comp_gas)[names(comp_gas)=="price"] <- "gasoline"
comp_die <- subset(comp, fuel=="diesel")[,c("date", "price")]
  names(comp_die)[names(comp_die)=="price"] <- "diesel"
comp_wide <- left_join(comp_gas, comp_die, by="date")

## Price difference ----
comp_wide$diff <- comp_wide$diesel - comp_wide$gasoline

# colors ----
# https://coolors.co/palette/ffcdb2-ffb4a2-e5989b-b5838d-6d6875
COL1 <- "#ffcdb2"
COL2 <- "#ffb4a2"
COL3 <- "#e5989b"
COL4 <- "#b5838d"
COL5 <- "#6d6875"

# fonts ----
FONT1 <- "PT Mono"
FONT2 <- "Ultra"
FONT3 <- "Atkinson Hyperlegible"

# plot ----
TIDY <- ggplot(comp_wide) +
  # LINES:
  geom_area( aes(x=date, y=diff),     fill="black") +
  geom_line( aes(x=date, y=gasoline), linewidth=1, col=COL4) +
  geom_line( aes(x=date, y=diesel),   linewidth=1, col=COL5)  +
  # TEXTS:
  geom_text(x=as.Date("2008-01-01"), y=4.9, label="Diesel Price",               family=FONT1, size=8, color=COL5,    hjust=.5, vjust=0) +
  geom_text(x=as.Date("2009-01-01"), y=1.6, label="Gasoline Price",             family=FONT1, size=8, color=COL4,    hjust=.5, vjust=1) +
  geom_text(x=as.Date("2024-12-01"), y=-.3, label="How Much Diesel Costs More", family=FONT1, size=8, color="black", hjust=1, vjust=.5) +
  # LABS:
  scale_y_continuous(limits=c(-.5,6), breaks=seq(-2,6,.1), minor_breaks = NULL, labels=scales::dollar_format(), expand=c(0,0), sec.axis = dup_axis()) +
  scale_x_date(date_breaks="1 year",  date_labels =  "%Y", limits=c(as.Date("2000-01-01"), as.Date("2025-01-01")), expand=c(0,0), sec.axis = dup_axis()) +
  labs(y=NULL, 
       x=NULL,
       title="U.S. Weekly Fuel Prices",
       subtitle="2000–2024: Gasoline and Diesel Prices + Price Difference",
       caption="Graph: **PYYXXO** | Source: **The U.S. Energy Information Administration (EIA)** | #TidyTuesday **2025/26**") + 
  # THEME:
  theme_minimal(base_size = 16, base_family = FONT3) + 
  theme(plot.title    = element_markdown(size = 49, hjust = .5, margin = unit(c(0, 0, 5, 0),"mm"), family=FONT2),
        plot.subtitle = element_markdown(size = 20, hjust = .5, margin = unit(c(0, 0, 5, 0),"mm"), family=FONT2),
        plot.caption  = element_markdown(size = 14, hjust = .5, margin = unit(c(5, 0, 0, 0),"mm"), color=COL3), 
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        axis.ticks.y = element_blank(),
        axis.text.x.top    = element_text(angle = 270, hjust = 1, vjust=.5, color ="black", family=FONT1),
        axis.text.x.bottom = element_text(angle = 90,  hjust = 0, vjust=.5, color ="black", family=FONT1),
        axis.text.y.left   = element_text(angle = 0,   hjust = 1, vjust=.5, color ="black", family=FONT1),
        axis.text.y.right  = element_text(angle = 0,   hjust = 1, vjust=.5, color ="black", family=FONT1),
        axis.title.y=element_text(angle=270, hjust=0, vjust=0),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = COL1),
        legend.position = "none",
        strip.text = element_text(size = 16), 
        plot.background =  element_rect(fill = COL2, color = COL2),
        plot.margin = unit(c(15, 5, 15, 5),"mm"))

ggsave(TIDY, file="figure/TIDY_2025_26.png", width = 16*(2/3), height = (1350/1080)*16*(2/3), units = "in") 

#end 
