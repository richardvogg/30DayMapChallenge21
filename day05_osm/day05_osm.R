library(ggplot2)
library(dplyr)
library(osmdata) #Open Street Map
library(stringr)
library(ggfx) #white glow behind text

sysfonts::font_add_google(name = "Crimson Text","Crimson Text")
showtext::showtext_auto()

bbox <- getbb("Würzburg")
bbox[1,] <- c(9.9287, 9.9347)
bbox[2,] <- c(49.7634,49.7669)

bbox <- opq(bbox)

test <- bbox %>% osmdata_sf() %>% .$osm_polygons %>% 
  select(osm_id, name, amenity, building, highway, lanes, landuse, 
         leisure, natural, place, surface, geometry) 

roads <- bbox %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

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

ggplot(test_color) +
  geom_sf(aes(fill = fill, size = fill), col = "grey", alpha = 0.5) +
  
  geom_sf(data=roads$osm_lines,col="white", size = 1.5) +
  geom_sf(data=roads$osm_lines,col="grey20", size = 0.5) +
  
  geom_sf_text(aes(label = str_wrap(name, 15)), check_overlap = TRUE) +

  coord_sf(xlim = c(9.9287, 9.9336), ylim = c(49.7634,49.7665)) +
  scale_fill_manual(values = c("grey90", "#D0F1BF", "#64B96A", "wheat", "grey50", 
                               "grey30", "blue", NA)) +
  scale_size_manual(values = c(0, 0, 0, 0, 0, 1, 1,0)) +
  labs(title = "Botanical Garden, Würzburg, Germany",
       caption = "© OpenStreetMap contributors") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(size = 35, family = "Crimson Text"))

ggsave("day05_osm/plot.png",width=11,height=12, device="png") 


#individual picking

building <- bbox %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf()

wege <- bbox %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

water <- bbox %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

meadow <- bbox %>%
  add_osm_feature(key = "landuse", value = "meadow") %>%
  osmdata_sf()

sand <- bbox %>%
  add_osm_feature(key = "natural", value = "sand") %>%
  osmdata_sf()

woods <- bbox %>%
  add_osm_feature(key = "leisure") %>%
  osmdata_sf()


ggplot(data=woods$osm_polygons) +
  geom_sf(fill = "green4", alpha = 0.5) +
  geom_sf(data=meadow$osm_polygons, fill = "green3", alpha = 0.5)+
  geom_sf(data=sand$osm_polygons, fill = "yellow")+
  geom_sf(data=building$osm_polygons, fill = "grey", size = 1)+
  geom_sf(data=water$osm_polygons, fill = "skyblue", col = "blue") +
  
  geom_sf(data=wege$osm_lines,col="white", size = 1.5) +
  geom_sf(data=wege$osm_lines,col="grey20", size = 0.5) +
  
  with_outer_glow(
    geom_sf_text(data = building$osm_polygons, aes(label=str_wrap(name, 18)), size=4, check_overlap = TRUE),
    colour = "white", sigma = 1
  ) +
  with_outer_glow(
    geom_sf_text(data = woods$osm_polygons, aes(label=str_wrap(name, 15)), size = 4),
    colour = "white"
  ) +
    
  coord_sf(xlim = c(9.9287, 9.9336), ylim = c(49.7634,49.7665)) +
  theme_void()

