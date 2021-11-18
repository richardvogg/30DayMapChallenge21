library(sf)
library(ggplot2)
library(forcats)
library(dplyr)
library(stringr)
library(rnaturalearth)

sysfonts::font_add_google(name = "Piazzolla", "Piazzolla")
showtext::showtext_auto()

## data from: https://www.bgr.bund.de/DE/Themen/Wasser/Produkte/produkte_node.html

grwater <- read_sf("../datasets/ERGW1000_v1/shp/ergw1000_gwerg__v11_poly.shp")

grwater_tr <- st_transform(grwater, 4326) %>%
  st_make_valid()


cities <- maps::world.cities %>%
  filter(country.etc=="Germany") %>% 
  filter(name %in% c("Berlin", "Munich", "Hamburg", "Cologne", 
                     "Frankfurt", "Stuttgart", "Dortmund", "Leipzig", "Nuremberg"),
         pop > 100000)



rivers <- ne_download(scale = 10, type = 'rivers_lake_centerlines', 
                         category = 'physical') %>%
  st_as_sf() %>%
  st_intersection(grwater_tr %>% st_union())

# Viz

grwater_tr %>% 
  mutate(entn_bru = factor(str_replace(entn_bru, "meist ", ""))) %>% 
  mutate(entn_bru = fct_relevel(entn_bru, c("< 2 (l/s)", "< 5 (l/s)",
                                "5 - 15 (l/s)", "15 - 40 (l/s)",
                                "> 40 (l/s)"))) %>%
ggplot() +
  geom_sf(aes(fill = entn_bru), col = NA) +
  scale_fill_manual(values = c("#F6DFEB", "#EDFFEC", "#CAF7E3", "#9FD8DF", "skyblue1"),
                    na.translate = FALSE) +
  geom_sf(data = rivers, col = "blue", size = 0.5, alpha = 0.7) +
  geom_label(data = cities, aes(x = long, y = lat, label = toupper(name)), size = 4,
             alpha = 0.6, label.size = 0, family = "Piazzolla", vjust = -0.1) +
  
  labs(title = "Ground water accessibility in Germany",
       caption = "Data: BGR") +
  coord_sf() +
  expand_limits(x = 16) +
  theme_void() +
  theme(panel.background = element_rect(fill = "grey10"),
        plot.background = element_rect(fill = "grey10"),
        text = element_text(colour = "white", family = "Piazzolla"),
        legend.position = c(0.9, 0.3),
        plot.title = element_text(size = 25, hjust = 0.5, vjust = -0.1),
        plot.caption = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank())

ggsave("day18_water/plot.png", width = 7, height = 9.16, device="png") 
