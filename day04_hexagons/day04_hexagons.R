library(ggplot2)
library(dplyr)
library(geogrid)
library(tmap)
library(stringr)
library(patchwork)

sysfonts::font_add_google(name = "Lato", "Lato")
showtext::showtext_auto()


##Load data
#Global Burden of Diseases data: http://ghdx.healthdata.org/gbd-results-tool

data("World")

gbd <- read.csv("../datasets/IHME-GBD_2019.csv") %>%
  mutate(iso_a3 = countrycode::countryname(location, destination = "iso3c")) %>%
  group_by(iso_a3) %>%
  top_n(1, val) %>%
  select(iso_a3, location, cause, val)

final_df <- World %>%
  filter(iso_a3 != "ATA", continent == "Africa") %>% 
  left_join(gbd)


#Create geogrid 
new_cells_hex <- calculate_grid(shape = final_df, grid_type = "hexagonal", seed = 3)
final_df_hex <- assign_polygons(final_df, new_cells_hex)


#scale hexagons to 90% of their size
africa_sfc = st_geometry(final_df_hex)
centroid_sfc = st_centroid(africa_sfc)
final_df_scale = (africa_sfc - centroid_sfc) * 0.9 + centroid_sfc


#Create hex map
hex_map <- final_df_hex %>%
  st_set_geometry(final_df_scale) %>%
  ggplot() + geom_sf(aes(fill = cause), size = 1, col = "white") +
  geom_sf_text(aes(label = str_wrap(name, 10)), size = 4, col = "grey10", family = "Lato") +
  scale_fill_brewer(palette = "Set1") +
  expand_limits(x = c(-20, 60)) +
  labs(title = "Main death causes per country in Africa 2019",
       fill = "",
       caption = glue::glue("Global Burden of Disease Collaborative Network.
Global Burden of Disease Study 2019 (GBD 2019) Results.
Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020.
Available from http://ghdx.healthdata.org/gbd-results-tool.")) +
  theme_void() +
  theme(plot.background = element_rect(fill = "lightblue"),
        legend.position = c(0.85, 0.85),
        plot.title = element_text(size = 30, hjust = 0.1),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        text = element_text(family = "Lato"))

#Create real map
real_map <- final_df %>%
  ggplot() + geom_sf(aes(fill = cause), col = "white") +
  geom_sf_text(aes(label = substr(name,1,1))) +
  scale_fill_brewer(palette = "Set1") +
  labs(subtitle = "Real map for comparison") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(colour = "black", size = 1),
        plot.subtitle = element_text(hjust = 0.1),
        text = element_text(family = "Lato"))

#Combine plots
hex_map + inset_element(real_map, 0.02, 0 , 0.35, 0.5)

ggsave("day04_hexagons/plot.png",width=11,height=12, device="png") 
