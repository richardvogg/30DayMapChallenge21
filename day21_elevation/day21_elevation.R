library(elevatr)
library(sf)
library(raster)
library(ggfx)


sysfonts::font_add_google(name = "Pacifico", "Pacifico")
showtext::showtext_auto()

osmdata::getbb("Tenerife")

tenerife <- st_bbox(c(xmin = -16.95, ymin = 27.97, xmax = -16.06, ymax = 28.62), crs = st_crs(4326)) %>%
  st_as_sfc() %>%
  elevatr::get_elev_raster(z = 9, clip = "locations")

#if the program is too slow - lower z (zoom).
canary <- st_bbox(c(xmin = -18.35, ymin = 27.39, xmax = -13.32, ymax = 29.52), crs = st_crs(4326)) %>%
  st_as_sfc() %>%
  elevatr::get_elev_raster(z = 7, clip = "locations")

canary_df <- rasterToPoints(canary) %>% data.frame()
colnames(canary_df) <- c("lon", "lat", "alt")

map <- canary_df %>%
  mutate(alt = ifelse(alt < 0 , -1000, alt)) %>%
ggplot(aes(lon, lat)) + 
  geom_contour_filled(aes(z = alt), breaks = seq(-1000, 3700, 100)) +
  scale_fill_manual(values = viridis::magma(47)) +
    annotate("text", -16, 29.2, label = "The Canary Islands", size = 16, 
             fontface = "bold", color = "#FEB87EFF", hjust = 0.5, family = "Pacifico") +
  coord_quickmap() +
  theme_void()

legend <- data.frame(id = 1:47) %>%
  ggplot(aes(x = id, y = 1, fill = id)) + 
  geom_tile(col = "grey") +
  geom_text(aes(x = 50, y = 1, label = "3715 m"), col = "#FCFDBFFF", hjust = 0) +
  scale_fill_viridis_c(option = "magma") +
  xlim(10, 65) + 
  theme_void()

map + inset_element(legend, 0.75, 0.02, 0.9, 0.05) &
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black"))

ggsave("day21_elevation/plot.png", width = 12.45, height = 6, device="png") 
