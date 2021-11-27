library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(rnaturalearth)
library(countrycode)
library(roughsf)
library(cowplot)

## Create the legend first

#boxes
geometry <- st_sfc(st_polygon(x = list(matrix(c(-51.5, -39, -50, -39, -50, -40.5, -51.5, -40.5, -51.5, -39), 
                           ncol = 2, byrow = TRUE))), crs = 4326)

legend1 <- st_sf(gu_a3 = NA, continent = NA, geometry = geometry)

geometry <- st_sfc(st_polygon(x = list(matrix(c(-51.5, -42, -50, -42, -50, -43.5, -51.5, -43.5, -51.5, -42), 
                                              ncol = 2, byrow = TRUE))), crs = 4326)

legend2 <- st_sf(gu_a3 = NA, continent = NA, geometry = geometry)

geometry <- st_sfc(st_polygon(x = list(matrix(c(-51.5, -45, -50, -45, -50, -46.5, -51.5, -46.5, -51.5, -45), 
                                              ncol = 2, byrow = TRUE))), crs = 4326)

legend3 <- st_sf(gu_a3 = NA, continent = NA, geometry = geometry)

geometry <- st_sfc(st_polygon(x = list(matrix(c(-51.5, -48, -50, -48, -50, -49.5, -51.5, -49.5, -51.5, -48), 
                                              ncol = 2, byrow = TRUE))), crs = 4326)

legend4 <- st_sf(gu_a3 = NA, continent = NA, geometry = geometry)

#labels
labels <- data.frame(name = c("around 4 years", "around 1 year", "less than 2 weeks", 
                              "only at the airport"))
labels$geometry <- st_sfc(
  st_point(c(-48, -39.5)), st_point(c(-48, -42.5)),
  st_point(c(-48, -45.5)), st_point(c(-48, -48.5))
)
labels <- st_sf(labels)
st_crs(labels) <- 4326
labels$size <- 0
labels$color <- "white"
labels$label <- labels$name
labels$label_pos <- "e"



## Get country shapefiles

world <- ne_countries(scale = "small", returnclass = 'sf')

viridis_cols = viridis::viridis(5)

## Join everything

world_final <- world %>%
  dplyr::select(gu_a3, continent, geometry) %>%
  filter(continent == "South America") %>%
  st_cast("POLYGON") %>%
  rbind(legend1) %>%
  rbind(legend2) %>%
  rbind(legend3) %>%
  rbind(legend4) %>%
  mutate(fill = viridis_cols[c(NA, NA, 2, 1, 5, 5, 2, 1, NA, NA, 4, NA, NA, NA, NA, 5, 4, 2, 1)],
         fillweight = c(NA, NA, 0.5, 0.1, 2, 2, 0.5, 0.1, NA, NA, 1, NA, NA, NA, NA, 2, 1, 0.5, 0.1),
         fillstyle = "hatch")


# Plot

map <- roughsf(
  list(world_final, labels), simplification = 2,
  title = "Where I have been how long", 
  caption = "Map by ",
  title_font = "30px Arial",
  font = "16px Arial",
  caption_font = "20px",
  
  width = 600, height = 800
)

map

#save_roughsf(map, file = "day26_choropleth/plot.png")

#Error in pagedown::chrome_print(tfile, output = file, format = format,  : 
#The browser is not executable: /Applications/Google Chrome.app/Contents/MacOS/Google Chrome




## Other ideas

library(readxl)
library(countrycode)

df2021 <- read_xlsx(paste0("../datasets/Fragile State Index/fsi-2021.xlsx")) %>%
  dplyr::select(Country, Year, Total) %>%
  mutate(Countrycode = countryname(Country, destination = "iso3c"))



world <- ne_countries(scale = "small", returnclass = 'sf')

world_final <- world %>%
  dplyr::select(gu_a3, continent, geometry) %>%
  filter(continent == "South America") %>%
  left_join(df2021, by=c("gu_a3"="Countrycode")) %>%
  filter(!is.na(Country)) %>%
  st_cast("POLYGON")

viridis_cols = viridis::viridis(10)

world_final$fill = viridis_cols[as.numeric(cut(world_final$Total, 10))]

map = roughsf(
  world_final, simplification = 2,
  title = "The Fragile State Index", 
  caption = "...",
  title_font = "40px",
  caption_font = "20px",
  width = 800, height = 1000
)

map
