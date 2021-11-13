library(rnaturalearth)
library(sf)
library(ggfx)
library(scales)
library(patchwork)


sysfonts::font_add_google(name = "Cabin", "Cabin")
showtext::showtext_auto()

world <- ne_countries(country = "Australia", returnclass = "sf") 
  
reefs <- ne_download(scale = 10, type = "reefs", category = "physical") %>%
  st_as_sf() %>%
  st_intersection(world %>%
                    st_buffer(5))

cities <- maps::world.cities %>%
  filter(country.etc %in% c("Australia")) %>%
  top_n(25, pop)

australia <- ggplot(world) +
  geom_sf(fill = "grey40") +
  geom_rect(xmin = 140, xmax = 155, ymin = -26, ymax = -8, col = "#FCFFA4FF", size = 1, fill = NA) +
  expand_limits(y = -7) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#000004FF", colour = "white"))

ggplot(world) + 
  geom_sf(fill = "grey40") +
  with_outer_glow(
    geom_sf(data = reefs, col = "#FCFFA4FF", size = 2), colour = "#FCA50AFF"
  ) +
  geom_point(data = cities, aes(x = long, y = lat, size = pop)) +
  geom_text(data = cities, aes(x = long, y = lat, label = name), hjust = 1, 
            nudge_x = -0.2, family = "Cabin", col = "grey90") +
  coord_sf(xlim = c(140, 155), ylim = c(-26, -8)) +
  labs(title = "The Great Barrier Reef", 
       caption = "Data: Natural Earth") +
  theme_map() +
  theme(plot.background = element_rect(fill = "#000004FF"),
        legend.position = "none",
        text = element_text(family = "Cabin"),
        plot.title = element_text(size = 24, colour = "#FCFFA4FF"),
        plot.caption = element_text(colour = "#FCFFA4FF")) +
  inset_element(australia, 0.65, 0.75, 0.99, 0.99)

ggsave("day13_naturalearth/plot.png", width=7.1, height=10, device="png")   


