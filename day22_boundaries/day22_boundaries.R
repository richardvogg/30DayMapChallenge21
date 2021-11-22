library(osmdata)
library(dplyr)
library(ggplot2)
library(sf)
library(ggfx)

sysfonts::font_add_google(name = "Pacifico", "Pacifico")
showtext::showtext_auto()

bbox <- getbb("Ixtapaluca")
bbox[1,] <- c(-98.91, -98.89)
bbox[2,] <- c(19.29, 19.31)

bbox <- opq(bbox)


roads <- bbox %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf() %>%
  .$osm_lines

test <-st_centroid(roads) %>%
  dplyr::select(osm_id) %>%
  {do.call(rbind, st_geometry(.))} %>%
  as_tibble() %>%
  setNames(c("lon", "lat")) %>%
  mutate(left_right = case_when(
    lon < -98.9020 ~ "a",
    lon < -98.9014 ~ "b",
    TRUE ~ "c"
  ))

roads <- roads %>%
  cbind(test)

circle <- st_sfc(st_point(c(-98.9, 19.30)), crs = 4326) %>% 
  st_transform(4485) %>%
  st_buffer(dist = 800) %>%
  st_transform(4326)

roads %>%
  st_intersection(circle) %>%
ggplot() +
  with_outer_glow(
    geom_sf(data = circle, fill = "white", size = 1), colour = "green",expand = 2
  ) +
  
  geom_sf(aes(col = left_right), size = 0.5) +
  
  annotate("text", -98.90, 19.311, label = "Ixtapaluca", size = 16, 
           fontface = "bold", color = "green4", hjust = 0.5, family = "Pacifico") +
  annotate("text", -98.90, 19.309, label = "Mexico City", size = 9, 
          color = "green4", hjust = 0.5, family = "Pacifico") +
  scale_color_manual(values = c("grey", "black", "green4")) +
  expand_limits(y = 19.312) +
  labs(caption = "© OpenStreetMap contributors") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "grey20"),
        plot.caption = element_text(colour = "green4"))

ggsave("day22_boundaries/plot.png", width = 6.6, height = 9, device="png") 
