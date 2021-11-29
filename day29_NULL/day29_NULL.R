library(readxl)
library(countrycode)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(roughsf)

#without countrycode

df2021 <- read_xlsx(paste0("../datasets/Fragile State Index/fsi-2020.xlsx")) %>%
  dplyr::select(Country, Year, Total)

world <- ne_countries(scale = "small", returnclass = 'sf')

world_final <- world %>%
  dplyr::select(name_sort, continent, geometry) %>%
  left_join(df2021, by=c("name_sort"="Country")) %>% 
  filter(!is.na(Total)) %>%
  st_cast("POLYGON")

viridis_cols = viridis::viridis(10)

world_final$fill = viridis_cols[as.numeric(cut(world_final$Total, 10))]

map = roughsf(
  world_final, simplification = 2,
  title = "The Fragile State Index", 
  caption = "without countrycode::countryname()",
  title_font = "30px Arial",
  caption_font = "20px Arial",
  width = 1200, height = 600
)

map

# with countrycode (changed lines are marked with ######)

df2021 <- read_xlsx(paste0("../datasets/Fragile State Index/fsi-2021.xlsx")) %>%
  dplyr::select(Country, Year, Total) %>%
  mutate(Countrycode = countryname(Country, destination = "iso3c")) %>% ##########
  mutate(Countrycode = ifelse(Countrycode == "SSD", "SDS", Countrycode))



world <- ne_countries(scale = "small", returnclass = 'sf')

world_final <- world %>%
  dplyr::select(gu_a3, continent, geometry) %>%
  left_join(df2021, by=c("gu_a3"="Countrycode")) %>% ###########
  filter(!is.na(Country)) %>%
  st_cast("POLYGON")

viridis_cols = viridis::viridis(10)

world_final$fill = viridis_cols[as.numeric(cut(world_final$Total, 10))]

map = roughsf(
  world_final, simplification = 2,
  title = "The Fragile State Index", 
  caption = "with countrycode::countryname()",
  title_font = "30px Arial",
  caption_font = "20px Arial",
  width = 1200, height = 600
)

map
