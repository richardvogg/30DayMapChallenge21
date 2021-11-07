library(ggplot2)
library(dplyr)
library(edgebundle)
library(igraph)
library(tradestatistics)
library(purrr)
library(scales)
library(ggnewscale) # two size scales - one for points and one for lines
library(cowplot) # add an image to the plot


sf::sf_use_s2(FALSE)
options(scipen = 10)

sysfonts::font_add_google(name = "Merienda","Merienda")
showtext::showtext_auto()

# get code for commodities
tradestatistics::ots_commodity_code("wine")

#or check
View(tradestatistics::ots_commodities_shortnames)



world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
  filter(iso_a3 != "ATA") %>% #filter out Antartica
  select(sovereignt, continent, iso_a3) %>%
  mutate(iso_a3 = tolower(iso_a3)) 

source("day07_green/helper_function.R")

i = 1
for(y in seq(1990, 2019)) {
  
  print(edge_bundle_map(year = y, commodities = '0304', path_to_img = "",
                  plot_col = "blue"))

  ggsave(paste0("day08_blue/plot",i,".png"),width=15, height=7.21, device="png",
         dpi = 150)
  i = i+1
}

img_frames <- paste0("day08_blue/plot", seq(1,30), ".png")
magick::image_write_gif(magick::image_read(img_frames),
                        path = "day08_blue/fish_exports.gif",
                        delay = 0.6)
