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


#88CCEE,#CC6677,#DDCC77,#117733,#332288,#AA4499,#44AA99,#999933,#882255,#661100,#6699CC,#888888

seacolor <- "#8ecae6"
textcol <- "#023047"
small <- "#ffb703"
big <- "#fb8500"

ggplot() + 
  borders(regions=c("Chile","Argentina","Peru", "Bolivia", "Paraguay", "Brazil"), 
          fill="grey95", col = "grey80")+
  geom_point(data=subset(df, mag <= 7.5),
             aes(x = longitude, y = latitude), size = 2, col=small, alpha=0.1)+
  geom_point(data=subset(df, mag > 7.5),
             aes(x = longitude, y = latitude), col = big, size = 3)+
  geom_text_repel(data=subset(df, mag>7.5 & longitude<(-70) & latitude>=(-40)),
                  aes(x = longitude,y=latitude,label=paste(year,"|",mag,"\n",region)),
                  nudge_x = -7 , col=textcol , family="Piazzolla", segment.colour = textcol)+
  geom_text_repel(data=subset(df, mag>7.5 & (longitude > (-70) | latitude < (-40))),
                  aes(x = longitude, y = latitude, label = paste(year,"|",mag,"\n",region)),
                  nudge_x = 6, col=textcol, family="Piazzolla", segment.colour = textcol)+
  geom_richtext(data = NULL, aes(x = -69, y = -32, 
                                 label = glue::glue("**Earthquakes <br> in Chile <br>
                                                    in the 21st <br> century**")), 
                hjust = 0, fill = NA, label.color = NA, size = 6, family = "Piazzolla", color = textcol) +
  scale_size_continuous(breaks = c(5,6,7,8.8),range = c(0.5,3))+
  labs(x="",y="",
       caption="Data from: https://earthquake.usgs.gov/earthquakes/search/")+
  theme_light()+
  theme(legend.position="none",
        text = element_text(family = "Piazzolla", colour = textcol),
        plot.background = element_rect(fill = "grey90"),
        panel.background = element_rect(fill=seacolor),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size=14),
        plot.caption = element_text(size=10),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  coord_quickmap(xlim = c(-85,-60), ylim = c(-56,-17))
