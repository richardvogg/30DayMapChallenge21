library(ggplot2)
library(dplyr)
library(ggrepel)
library(ggtext)

sysfonts::font_add_google(name = "Piazzolla", "Piazzolla")
showtext::showtext_auto()

#data can be downloaded here: https://earthquake.usgs.gov/earthquakes/search/

df <- read.csv("../datasets/Earthquakes Chile Nov 2021.csv") %>% 
  mutate(year=substr(time,1,4)) %>%
  filter(year>=2000) %>%
  mutate(country=place %>% 
           strsplit(split=", ") %>% 
           lapply(function(x) x <- x[2]) %>%
           unlist()
  ) %>%
  filter(country=="Chile") %>%
  mutate(region=place %>% 
           strsplit(split="of |, ") %>% 
           lapply(function(x) x <- x[length(x)-1]) %>%
           unlist()
  )


seacolor <- "#8ecae6"
textcol <- "#023047"
small <- "#ffb703"
big <- "#fb8500"

ggplot() + 
  borders(regions=c("Chile","Argentina","Peru", "Bolivia", "Paraguay", "Brazil", "Uruguay"), 
          fill="grey95", col = "grey80")+
  geom_point(data=subset(df, mag <= 7.5),
             aes(x = longitude, y = latitude), size = 2, col=small, alpha=0.1)+
  geom_text_repel(data=subset(df, mag>7.5 & longitude<(-70) & latitude>=(-40)),
                  aes(x = longitude,y=latitude,label=paste(year,"\n",toupper(region))),
                  nudge_x = -9 , col=textcol , family="Piazzolla", size = 3.5)+
  geom_text_repel(data=subset(df, mag>7.5 & (longitude > (-70) | latitude < (-40))),
                  aes(x = longitude, y = latitude, label = paste0(year,"\n",toupper(region))),
                  nudge_x = 8, col=textcol, family="Piazzolla", size = 3.5)+
  geom_point(data=subset(df, mag > 7.5),
             aes(x = longitude, y = latitude, size = mag), col = big)+
  geom_text(data=subset(df, mag > 7.5),
             aes(x = longitude, y = latitude, label = mag), size = 3, col = "#b35f00")+
  
  scale_size_continuous(breaks = c(7.5, 8.8), range = c(8, 15))+
  labs(x="",y="",
       title = "Earthquakes in Chile in the 21st century",
       subtitle="Events over 7.5 magnitude highlighted",
       caption="Data from: https://earthquake.usgs.gov/earthquakes/search/")+
  theme_light()+
  theme(legend.position="none",
        text = element_text(family = "Piazzolla", colour = textcol),
        plot.background = element_rect(fill = "grey90"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill=seacolor),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size=14),
        plot.caption = element_text(size=10),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  coord_quickmap(xlim = c(-90,-55), ylim = c(-56,-17))





### South America

df <- read.csv("../datasets/earthquakes_nov_2021.csv", skip = 1, sep = ";") %>% 
  mutate(year=substr(time,1,4)) %>%
  select(-X)

ggplot() + 
  borders(regions=c("Chile","Argentina","Peru", "Bolivia", "Paraguay", "Brazil",
                    "Venezuela", "Colombia", "Uruguay", "Ecuador", "Guyana",
                    "Suriname", "French Guiana", "Panama", "Costa Rica", "Nicaragua"), 
          fill="grey95", col = "grey80")+
  geom_point(data=subset(df, mag <= 7.5),
             aes(x = longitude, y = latitude), size = 1, col=small, alpha=0.1)+
  geom_point(data=subset(df, mag > 7.5),
             aes(x = longitude, y = latitude), col = big, size = 3) +
  geom_richtext(data = NULL, aes(x = -60, y = -10, 
                                 label = glue::glue("**Earthquakes <br> in South America <br>
                                                    in the 21st <br> century**")), 
                hjust = 0, fill = NA, label.color = NA, size = 6, family = "Piazzolla", color = textcol) +
  theme_light()+
  theme(legend.position="none",
        text = element_text(family = "Piazzolla", colour = textcol),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill=seacolor),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size=14),
        plot.caption = element_text(size=10),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  coord_quickmap(xlim = c(-85,-30), ylim = c(-56,10))
  