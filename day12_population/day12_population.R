library(cartogram)
library(dplyr)
library(sf)
library(readxl)
library(ggplot2)
library(patchwork)

sysfonts::font_add_google(name = "Nunito", "Nunito")
showtext::showtext_auto()

test <- read_xlsx(path = "../datasets/chl_admpop_2021.xlsx", sheet = 2) %>%
  select(ADM1_EN, ADM1_PCODE, Year, pop = T_TL) %>%
  mutate(codregion = as.numeric(gsub("CL", "", ADM1_PCODE)))

chile <- read_sf("../datasets/Regiones/Regional.shp") %>%
  st_simplify(dTolerance = 1000) %>%
  filter(!Region %in% c("Zona sin demarcar"))

mask <- st_centroid(chile, of_largest_polygon = TRUE) %>%
  st_buffer(1000000) %>%
  st_union()

final <- chile %>%
  st_buffer(0) %>% 
  st_intersection(mask) %>%
  left_join(test)

chile_cartogram <- cartogram_cont(final, "pop", itermax=5)



carto <- ggplot(chile_cartogram) +
  geom_sf(aes(fill = pop), col = NA) +
  scale_fill_viridis_c(option = "C") +
  labs(subtitle = "Cartogram") +
  theme(legend.position = "none")

normal <- ggplot(final) +
  geom_sf(aes(fill = pop), col = NA) +
  scale_fill_viridis_c(option = "C") +
  labs(subtitle = "Normal") +
  theme(legend.position = "none")


carto + normal +
  plot_annotation(title = "Population in Chile",
                  caption = "Data: data.humdata.org") &
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "grey90"),
        text = element_text(family = "Nunito"),
        plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 16)
        )

ggsave("day12_population/plot.png", width= 6, height=12, device="png") 
