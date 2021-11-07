library(ggplot2)
library(dplyr)
library(edgebundle)
library(igraph)
library(tradestatistics)
library(purrr)
library(scales)
library(ggnewscale)
library(cowplot)

sf::sf_use_s2(FALSE)
options(scipen = 10)

sysfonts::font_add_google(name = "Merienda","Merienda")
showtext::showtext_auto()

tradestatistics::ots_commodity_code("tomato")

world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
  filter(iso_a3 != "ATA") %>% #filter out Antartica
  select(sovereignt, iso_a3) %>%
  mutate(iso_a3 = tolower(iso_a3))

tomatoes <- ots_create_tidy_data(years = 2018, reporters = "all", partners = "all", 
                                 commodities = '0702', table = "yrpc") %>%
  filter(trade_value_usd_exp > 0, 
         !is.na(partner_fullname_english),
         !is.na(reporter_fullname_english))# %>%

#get summary info for each country
tomatoes_summ <- tomatoes %>%
  group_by(reporter_iso) %>%
  summarise(exp_val = sum(trade_value_usd_exp),
            exp_minus_imp = sum(trade_value_usd_exp) - sum(trade_value_usd_imp)) %>%
  filter(reporter_iso %in% world$iso_a3)

#remove countries which are not on the world map of naturalearth
tomatoes <- tomatoes %>%
  filter(partner_iso %in% world$iso_a3, reporter_iso %in% world$iso_a3)


#get centroid for each country
centroids <- world %>% st_centroid(of_largest_polygon = TRUE) %>%
  select(iso_a3, geometry)

#translate center points to lat, lon -> vertices
verts <- centroids %>%
  mutate(lat = unlist(map(centroids$geometry,1)),
          lon = unlist(map(centroids$geometry,2))) %>%
  st_drop_geometry() %>%
  left_join(tomatoes_summ, by = c("iso_a3" = "reporter_iso")) %>%
  mutate(exp_val = ifelse(is.na(exp_val), 0, exp_val)) %>%
  mutate(exp_minus_imp = case_when(
    exp_minus_imp > 10000 ~ "mainly export",
    exp_minus_imp < -10000 ~ "mainly import",
    TRUE ~ "balanced"
  ))



edges <- tomatoes %>%
  select(from = reporter_iso, 
         to = partner_iso, 
         val = trade_value_usd_exp)
  
g <- igraph::graph_from_data_frame(edges, vertices = verts)


xy <- cbind(V(g)$lon, V(g)$lat)

fbundle <- edge_bundle_force(g, xy, compatibility_threshold = 0.6)
fbundle$weight <- E(g)$val[fbundle$group]


plot1 <- ggplot() +
  geom_sf(data = world, fill = "white") +
  geom_path(data = fbundle, aes(y, x, group = group, size = weight), 
            col = "red") +
  
  geom_path(data = fbundle, aes(y, x, group = group), 
            col = "white", size = 0.01) +
  scale_size_continuous(range = c(0.1, 2), guide = FALSE) +
  new_scale("size") +
  
  geom_point(data = verts, aes(lat, lon, col = exp_minus_imp, size = exp_val)) +
  scale_size_continuous("Export value", range = c(1, 8), 
                        label = label_dollar(scale = 1e-9, suffix = "B")) +
  scale_color_manual("Export/Import", values = c("grey", "orange", "blue")) +
  labs(title = 'Exports of "Tomatoes; fresh or chilled", 2018',
       caption = "Data: OpenTradeStatistics through the tradestatistics package") +
  ggraph::theme_graph(background = "grey20") +
  theme(plot.title = element_text(color = "red", family = "Merienda", size = 30),
        plot.caption = element_text(color = "white"),
        legend.text = element_text(size = 13, color = "white"),
        legend.title = element_text(size = 15, color = "red"),
        legend.position = c(0.15, 0.25))


logo_file <- "day06_red/tomato.png"

ggdraw(plot1) + 
  draw_image(logo_file, x = 0.2, y = 0.7, hjust = 1, vjust = 1, width = 0.15, height = 0.2)

ggsave("day06_red/plot.png",width=15,height=7.21, device="png") 
