library(ggplot2)
library(dplyr)
library(raster)
library(biscale)
library(cowplot)

# data from: https://data.terrapop.org/terraclip#

crop_de <- raster("../datasets/cropland_germany.tiff")
pasture_de <- raster("../datasets/pasture_germany.tiff")

crop_df <- as.data.frame(crop_de, xy = TRUE) %>%
  filter(!is.na(cropland_germany))

final_df <- as.data.frame(pasture_de, xy = TRUE) %>%
  filter(!is.na(pasture_germany)) %>%
  left_join(crop_df, by = c("x", "y"))

data <- bi_class(final_df, pasture_germany, cropland_germany, dim = 3)

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "More pasture land",
                    ylab = "More cropland",
                    size = 12)


map <- ggplot() +
  borders(regions = "Germany") +
  geom_tile(data = data , aes(x = x, y = y, fill = bi_class), show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(title = "Distribution of cropland and pasture land \nin Germany",
       caption = "Data: https://data.terrapop.org/terraclip") +
  coord_quickmap() +
  bi_theme() +
  expand_limits(x = c(3, 15)) +
  theme(axis.title = element_blank(),
        plot.title = element_text(size = 25),
        plot.caption = element_text(size = 12),
        panel.grid = element_blank())

ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.25, 0.65, 0.2, 0.2)

ggsave("day17_land/plot.png",width=11,height=12, device="png") 
