library(ggplot2)
library(dplyr)
library(sf)
library(patchwork)

sysfonts::font_add_google(name = "Merienda", "Merienda")
sysfonts::font_add_google(name = "Nunito", "Nunito")
showtext::showtext_auto()

Europe <- rnaturalearth::ne_countries(scale = "medium", returnclass = 'sf') %>%
  filter(continent == "Europe")

keep <- c(1, 0.2, 0.05, 0.01)

plots <- lapply(keep, function(x) {
  Europe_simpl <- rmapshaper::ms_simplify(Europe, keep = x,
                                          keep_shapes = TRUE)
  
  
  plot <- ggplot(Europe_simpl) + 
    geom_sf(aes(fill = iso_a3)) +
    scale_fill_viridis_d(option = "H") +
    coord_sf(xlim = c(-11,37.5), ylim = c(35,70))+
    labs(subtitle = paste0(x*100, "% of the points")) +
    theme_void() +
    theme(legend.position = "none")
  
  return(plot)
  
})


plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] +
  plot_annotation(title = "Simplify Europe",
                  caption = "Using the Visvalingam (ms_simplify) algorithm from {rmapshaper} \n explained in https://geocompr.robinlovelace.net/geometric-operations.html") +
  plot_layout(nrow = 1) &
  theme(plot.title = element_text(size = 35, family = "Merienda"),
        plot.subtitle = element_text(size = 16, family = "Nunito"),
        plot.caption = element_text(size = 12, family = "Nunito"))
