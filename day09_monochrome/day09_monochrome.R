library(ggplot2)
library(dplyr)
library(osmdata) #Open Street Map
library(roughsf)

sysfonts::font_add_google(name = "Crimson Text","Crimson Text")
showtext::showtext_auto()

bbox <- getbb("Versailles")
bbox[1,] <- c(2.077395, 2.125758)
bbox[2,] <- c(48.799911, 48.826757)


bbox <- opq(bbox)

test <- bbox %>% osmdata_sf() %>% .$osm_polygons %>% 
  select(osm_id, name, amenity, building, highway, lanes, landuse, 
         leisure, natural, place, surface, geometry) 

roads <- bbox %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

roads <- roads$osm_lines %>%
  filter(highway %in% c("footway"))


test_color <- test %>%
  mutate(fill = case_when(
    !is.na(leisure) ~ "a",
    landuse %in% c("farmland", "meadow") ~ "b",
    natural %in% c("scrub", "grassland", "wood") ~ "c",
    natural %in% c("heath", "slope", "sand") ~ "d",
    ((place == "square") | (amenity == "parking")) ~ "e",
    !is.na(building) ~ "f",
    natural == "water" ~ "g",
    
    TRUE ~ "z"
  ))

ggplot(roads) +
  
  geom_sf(col = "grey50", size = 3) +
  geom_sf(col="black", size = 1.5) +
  geom_sf(col="white", size = 0.5) +
  
  coord_sf(xlim = c(2.107, 2.123), ylim = c(48.8015, 48.8105)) +
  scale_size_manual(values = c(0, 0, 0, 0, 0, 0, 0,0)) +
  labs(title = "Versailles Park, in front of the castle",
       caption = "© OpenStreetMap contributors") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(size = 35, family = "Crimson Text", colour = "grey90"),
        plot.caption = element_text(colour = "grey90"),
        plot.background = element_rect(fill = "grey10"))


ggsave("day09_monochrome/plot.png", width=10, height=10, device="png") 
