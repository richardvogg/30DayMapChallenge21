library(ggplot2)
library(dplyr)
library(edgebundle)
library(igraph)
library(tradestatistics)
library(purrr)
library(scales)
library(ggnewscale) # two size scales - one for points and one for lines
library(cowplot) # add an image to the plot


## in this script I outsourced the data prepraration and plotting part
## to the external script helper_function, as I intend to use it in the future.
## If you are interested in seeing the script without the function,
## check day06_red.

sf::sf_use_s2(FALSE)
options(scipen = 10)

sysfonts::font_add_google(name = "Merienda","Merienda")
showtext::showtext_auto()

# get code for commodities
tradestatistics::ots_commodity_code("olive")


world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
  filter(iso_a3 != "ATA") %>% #filter out Antartica
  select(sovereignt, continent, iso_a3) %>%
  mutate(iso_a3 = tolower(iso_a3)) 

source("day07_green/helper_function.R")

edge_bundle_map(year = 1998, commodities = '1509', path_to_img = "day07_green/olives.png",
                plot_col = "olivedrab3")

ggsave("day07_green/plot.png",width=15, height=7.21, device="png") 


