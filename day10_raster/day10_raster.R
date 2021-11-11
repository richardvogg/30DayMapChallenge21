library(raster)
library(afrilearndata)

sysfonts::font_add_google(name = "Jura", "Jura")
showtext::showtext_auto()

pop <- afripop2020 %>%
  as.data.frame(xy = TRUE) %>%
  rename(pop = ppp_2020_1km_Aggregated) %>%
  filter(!is.na(pop)) %>%
  mutate(pop_disc = cut(pop, breaks = c(-1, 2, 20, 200, 2000, 25000),
                        labels = c("low", " ", "  ", "   ", "high")))

a <- extent(pop) #gives xmax, ymax, xmin, ymin

cities <- maps::world.cities %>%
  filter(lat > a@ymin, lat < a@ymax,
         long > a@xmin, long < a@xmax,
         !country.etc %in% c("Iraq", "Saudi Arabia")) %>%
  top_n(10, pop) %>%
  mutate(rank = rank(-pop))
  
top10 <- ggplot(cities, aes(x = 1, y = -rank)) +
  geom_point(, size = 8, col = "yellow") +
  geom_text(aes(label = rank), family = "Jura") +
  geom_text(aes(x = 1.04, label = paste0(name, ", ", country.etc)), hjust = 0, family = "Jura") +
  expand_limits(x = 1.6) +
  theme_void()

ggplot(pop) +
  geom_tile(aes(x, y, fill = pop_disc)) +
  geom_point(data = cities, aes(long, lat), size = 8, col = "yellow") +
  geom_text(data = cities, aes(long, lat, label = rank), family = "Jura") +
  scale_fill_viridis_d(option = "C") +
  labs(title = "Top 10 largest cities in Africa",
       caption = "Data: afrilearndata package",
       fill = "Population \ndensity") +
  coord_quickmap() +
  theme_map() +
  theme(
    legend.position = c(0.8, 0.85),
    panel.background = element_rect(fill = "#8ecae6"),
    text = element_text(family = "Jura"),
    plot.title = element_text(size = 25)) +
  inset_element(top10, 0.02, 0.05, 0.4, 0.5)

ggsave("day10_raster/plot.png", width=8, height=9.2, device="png")   
  
  
