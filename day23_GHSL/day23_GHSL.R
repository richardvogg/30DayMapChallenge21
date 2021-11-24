library(raster)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)

#load fonts
sysfonts::font_add_google(name = "Piazzolla", "Piazzolla")
showtext::showtext_auto()


#Data from: https://ghsl.jrc.ec.europa.eu/download.php?ds=pop

ghs <- raster("../datasets/GHS/GHS_BUILT_LDS20142.tif")

bbox <- osmdata::getbb("Bochum")
bbox[1,] <- c(6.3, 8.2)
bbox[2,] <- c(51.2, 52)

e <- as(extent(bbox), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
e <- projectExtent(e,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
ruhrgebiet <- crop(ghs, e)

crs(ruhrgebiet) <- "+init=EPSG:4326"
r <- projectRaster(ruhrgebiet, crs = "+init=EPSG:4326 +proj=longlat +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


cities <- maps::world.cities %>%
  filter(country.etc=="Germany") %>% 
  filter(pop > 60000,
         lat > 51.2,
         lat < 51.8,
         long > 6.3,
         long < 8.2) %>%
  mutate(pop_norm = (pop - 60000)/(max(pop)-60000))

cities_sf <- cities %>%
  st_as_sf(coords = c("long", "lat"), dim = "XY") %>%
  as('Spatial') 

crs(cities_sf) <- "+proj=longlat +datum=WGS84 +no_defs" 

cities_final <- cities_sf %>%
  st_as_sf() %>%
  st_transform("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

ruhr_df <- as.data.frame(r, xy = TRUE) %>%
  setNames(c("lon", "lat", "ghs")) %>%
  filter(!is.na(ghs))



ruhr_plot <- ruhr_df %>% 
  ggplot() +
  geom_raster(aes(lon, lat, fill = ghs)) +
  geom_sf_label(data = cities_final, aes(label = name, size = pop_norm), alpha = 0.5,
                family = "Piazzolla") +
  scale_fill_viridis_c(option = "F") +
  scale_size_continuous(range = c(2, 6)) +
  annotate("label", 540000, 6067000, label = "The Ruhrgebiet", size = 15, fill = "grey",
           alpha = 0.6,
           label.padding = unit(1.55, "lines"),
           fontface = "bold", color = "white", hjust = 0.5, family = "Piazzolla") +
  labs(caption = "Data: GHSL Global Human Settlement Layer") +
  coord_quickmap() +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "grey"))


## Show surroundings

minimap <- ggplot() + borders(regions = "Germany", fill = "grey90") +
  coord_quickmap() +
  geom_rect(aes(xmin = 6.3, xmax = 8.2, ymin = 51.2, ymax = 51.8), 
            col = "#F06043FF", size = 1, fill = NA) +
  theme_void() +
  theme(plot.background = element_rect(fill=alpha('white', 0.5)))


# combining plots

ruhr_plot + inset_element(minimap, 0.8, 0.7, 0.9, 0.9)

ggsave("day23_GHSL/plot.png",width=17,height=9.5, device="png") 
