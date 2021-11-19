library(dplyr)
library(ggplot2)
library(osmdata)
library(ggbump)
library(patchwork)

options(scipen = 10) 

sysfonts::font_add_google(name = "Nunito", "Nunito")
showtext::showtext_auto()


#I first tried to extract from the pdf tables, but it was not worth the effort,
#so I copied the values by hand
tourist_df <- data.frame(name = c("Gran Canaria", "Tenerife", "Fuerteventura",
                    "La Palma", "Lanzarote"),
           tourists = c(317388, 472718, 169166, 22089,  247078))

bbox <- getbb("Canary Islands")

islands <- rnaturalearth::ne_countries(scale = 10, country = "Spain") %>%
  st_as_sf() %>%
  st_crop(xmin = -18.1, xmax = -13.4, ymin = 27.6, ymax = 29.6) %>%
  st_cast("POLYGON") %>%
  mutate(name = c("El Hierro", "Gran Canaria", "La Gomera", "Tenerife", "Fuerteventura",
                  "La Palma", "Lanzarote", "")) %>%
  left_join(tourist_df)



islands_with_data <- islands %>%
  filter(!is.na(tourists))


ranking <- st_geometry(islands_with_data) %>% 
  st_point_on_surface() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  mutate(name = islands_with_data$name,
         tourists = islands_with_data$tourists) %>%
  filter(!is.na(tourists)) %>%
  bind_cols(tibble(fine_cap = BBmisc::normalize(rank(islands_with_data$tourists), range = c(29.6, 31.3), method = "range"),
                   xend = -23,
                   x_axis_start = xend + 10,
                   fine_cap_x = BBmisc::normalize(islands_with_data$tourists, range = c(first(x_axis_start), -11), method = "range"),
                   val_txt = paste0(format(islands_with_data$tourists, digits = 3, nsmall = 0))))

islands <- islands %>% 
  left_join(ranking)


map <- ggplot() + 
  geom_sf(data = islands, fill = "grey20", color = "white") +
  # Sigmoid from country to start of barchart
  geom_sigmoid(data = ranking, 
               aes(x = X, y = Y, xend = x_axis_start - .2, yend = fine_cap, group = name, color = fine_cap), 
               smooth = 10, size = 2, alpha = 0.7) + 
  # Line from xstart to value
  geom_segment(data = ranking, 
               aes(x = x_axis_start, y = fine_cap, xend = fine_cap_x, yend = fine_cap, color = fine_cap), size = 2, 
               lineend = "round", alpha = 0.7) + 
  # dot on centroid of country in map
  geom_point(data = ranking, 
             aes(x = X, y = Y, color = fine_cap), size = 3) +
  # Country text
  geom_text(data = ranking, aes(x = x_axis_start, y = fine_cap, label = name, color = fine_cap), 
            hjust = 0, size = 7, nudge_y = 0.1, family = "Nunito") +
  # Value text
  geom_text(data = ranking, aes(x = x_axis_start, y = fine_cap, label = round(as.numeric(val_txt),-3),
                                                                                    color = fine_cap), 
            hjust = 0, size = 5, nudge_y = -0.1, family = "Nunito") +
  annotate("text", -18, 30.2, label = str_wrap("Tourists on the Canary Islands, April 2019", 30), size = 12, 
           fontface = "bold", color = "black", hjust = 0, family = "Nunito") +
  annotate("text", -17.5, 27.5, label = str_wrap("No data was collected for El Hierro and La Gomera, the smallest of the islands", 50), 
           size = 5, color = "black", hjust = 0, family = "Nunito") +
  coord_sf(ylim = c(27.5, 31.5)) +
  scale_color_gradient(low = "purple", high = "blue") +
  theme_void() +
  labs(caption = "Data: turismodeislascanarias.com, Inspiration: @davsjob (ggbump)") + 
  theme(plot.margin = margin(.5, 0.1, .5, .5, "cm"),
        legend.position = "none",
        plot.background = element_rect(fill = "grey70"),
        plot.caption = element_text(color = "grey20", size = 12, family = "Nunito"),
        plot.title = element_blank(),
        plot.subtitle = element_blank())


## Show surroundings

minimap <- ggplot() + borders(fill = "grey90") +
  coord_quickmap(xlim = c(-30, 20), ylim = c(10, 60)) +
  geom_rect(aes(xmin = -18.5, xmax = -13, ymin = 27.1, ymax = 30), col = "red", size = 1, fill = NA) +
  theme_void() +
  theme(plot.background = element_rect(fill = NA, colour = "black", size = 2))



map + inset_element(minimap, 0.7, 0.01, 0.95, 0.45)


ggsave("day19_islands/plot.png", width = 13, height = 9, device="png") 


### getting the data (not manual way, only a start with tabulizer)
library(tabulizer)

tables <- extract_tables("https://turismodeislascanarias.com/sites/default/files/promotur_frontur_abril_2021_en.pdf")

tourism <- as.data.frame(tables[[1]])

total <- data.frame(tourism[3:6, ])
names(total) <- tourism[2, ]

