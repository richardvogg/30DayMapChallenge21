library(ggplot2)
library(dplyr)
library(sf)
library(ggfx)

sysfonts::font_add_google(name = "Piazzolla", "Piazzolla")
showtext::showtext_auto()

rails <- read_sf("../datasets/Strecken/Shapefiles/strecken_polyline.shp")

cities <- maps::world.cities %>%
  filter(country.etc=="Germany") %>% 
  filter(name %in% c("Berlin", "Munich", "Hamburg", "Cologne", 
                     "Frankfurt", "Stuttgart", "Dortmund", "Leipzig", "Nuremberg"),
         pop > 100000)

ggplot(rails) + 
  borders(regions = "Germany", fill = "darkblue") +
  geom_sf(col = "goldenrod1", size = 1.5) +
  geom_sf(col = "red", size = 0.4) +
  geom_label(data = cities, aes(x = long, y = lat, label = toupper(name)), size = 4,
             alpha = 0.6, label.size = 0, family = "Piazzolla", vjust = -0.5) +
  labs(title = "Rails in Germany",
       caption = "Data from: data.deutschebahn.com") +
  theme_void() +
  theme(text = element_text(family = "Piazzolla"),
        plot.title = element_text(size = 25),
        plot.caption = element_text(size = 10),
<<<<<<< HEAD
        plot.background = element_rect(fill = "grey90"))
=======
        plot.background = element_rect(fill = "grey90"))
>>>>>>> 0e1ac8f (added day 2)
