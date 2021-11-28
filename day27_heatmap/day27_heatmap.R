library(ggplot2)
library(dplyr)
library(forcats)
library(patchwork)

sysfonts::font_add_google(name = "Open Sans", "OpenSans")
showtext::showtext_auto()


##Load data
#Global Burden of Diseases data: http://ghdx.healthdata.org/gbd-results-tool

gbd <- read.csv("../datasets/IHME-GBD_2019_Stroke.csv") %>%
  mutate(iso_a3 = countrycode::countryname(location, destination = "iso3c"),
         continent = countrycode::countryname(location, destination = "continent")) %>%
  dplyr::select(location, year, cause, val, continent, iso_a3) %>% 
  filter(cause == "Drug use disorders") %>%
  group_by(location) %>%
  mutate(max_death = max(val)) %>%
  filter(max_death > 2)

gbd %>%
  mutate(val = ifelse(val > 5, 5, val)) %>%
  ggplot(aes(y = fct_reorder(location, max_death), x = year)) +
  geom_tile(aes(fill = val)) +
  geom_point(aes(x = 1989, col = continent), size = 5) +
  scale_color_brewer(type = "qual", palette = 1) +
  scale_fill_gradient(low = "grey", high = "tomato", 
                      breaks = 1:5, labels = c("<1",2,3,4,">5")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0,0)) +
  expand_limits(x = 2021) +
  labs(title = "Deaths by drug use disorders per 100,000 habitants",
       subtitle = "Showing selected countries with high death rates",
       caption = glue::glue("Global Burden of Disease Collaborative Network.
       Global Burden of Disease Study 2019 (GBD 2019) Results.
       Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020.
       Available from http://ghdx.healthdata.org/gbd-results-tool.")) +
  guides(colour = guide_legend(order = 1), 
         fill = guide_legend(order = 2)) +
  #theme_void() +
  theme(text = element_text(family = "OpenSans"),
    axis.text.y = element_text(size = 11, hjust = 1),
        axis.text.x = element_text(size = 14),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
    panel.border = element_blank(),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 15),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.margin=margin(t = 0.5, b = 0.5, unit='cm'),
        legend.spacing = unit(8, "cm"),
        legend.position = "top")

ggsave("day27_heatmap/plot.png", width=14, height=10, device="png") 

