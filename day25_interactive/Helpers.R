library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggtext)
library(maps) # get lat and lon for cities

data("world.cities")

cities <- world.cities %>%
  filter(country.etc %in% c("Ghana","Burkina Faso","Niger"), pop > 200000)


make_map <- function(df) {
  df %>%
    group_by(x,y) %>%
    summarise(precip=sum(prec1981_2010,na.rm=TRUE),.groups="drop") %>% 
    mutate(precip_bin = cut(precip,breaks=c(0,100,800,3300),
                            labels=c("<100mm","100-800mm",">800mm"))) %>%
    ggplot()+
    geom_point(aes(x=x,y=y,col=precip_bin))+
    borders(regions=c("Ghana","Burkina Faso","Niger$"))+
    coord_quickmap()+
    ggrepel::geom_text_repel(data=subset(cities,lat<10),aes(x=long,y=lat,label=name),nudge_x=(-7))+
    ggrepel::geom_text_repel(data=subset(cities,lat>10),aes(x=long,y=lat,label=name),nudge_x=(-3),nudge_y=(3))+
    scale_colour_manual(values = c("khaki3","cadetblue2","dodgerblue1"))+
    labs(colour = "Average yearly \nprecipitation")+
    theme_void()+
    theme(legend.position = c(0.7,0.15),
          legend.title = element_text(size=16),
          legend.text = element_text(size=14))
}

plot_selected_point_in_map <- function(df,longi,lati) {
  pts <- data.frame(lon=longi,lat=lati)
  
  g <- make_map(df)+geom_point(data=pts,aes(x = lon, y= lat),color="red",size=4)
  
  return(g)
}




plot_30y_avg_vs_today <- function(df,longi,lati) {
  
  df_summ <- df %>%
    filter(x %in% longi, y %in% lati) %>%
    group_by(month) %>%
    summarise(prec2011_19 = mean(prec2011_19,na.rm=TRUE),
              prec1981_2010 = mean(prec1981_2010,na.rm=TRUE)) %>%
    mutate(month=factor(month.abb[month],levels=month.abb))
  
  max_diff <- df_summ %>%
    mutate(diff = prec2011_19 - prec1981_2010) %>%
    top_n(1,abs(diff)) %>%
    mutate(type=ifelse(diff<0,"deficit","surplus"))
  
  g <- df_summ %>%
    ggplot(aes(x=month,group=1))+
    geom_line(size=2,aes(y=prec2011_19),col="darkturquoise")+
    geom_line(aes(y=prec1981_2010),col="blueviolet",size=2)+
    geom_segment(data=max_diff,aes(y=prec1981_2010,yend=prec2011_19,xend=month),
                 arrow=arrow(),size=2,col="red")+
    geom_text_repel(data=max_diff,aes(y=prec1981_2010+diff/2,
                                      label=paste0(type,": \n",round(abs(diff)),"mm\n",
                                                   round(abs(diff)/prec1981_2010,3)*100,"%")),
                    col="red", size=7,
                    nudge_x=1)+
    scale_color_identity(name="compare",guide="legend")+
    labs(title="Precipitation comparison between <span style='color:blueviolet'>**long-term average (1981-2010)**</span> <br> and the <span style='color:darkturquoise'>**last years (2011-2019)**</span>", 
         y="Precipitation in mm",x="")+
    theme_light()+
    theme(plot.title = element_markdown(lineheight = 1.1,size=18),
          axis.title = element_text(size=16),
          axis.text = element_text(size=14))
  
  return(g)
}
