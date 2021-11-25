library(raster)
library(dplyr)
library(ggplot2)
library(tmap)
library(sf)

#data from ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip


#Long term mean
ltm <- brick("C:/Richard/R and Python/Environmental Data Science/GIS/ChileClimate/Data_raw/precip.day.1981-2010.ltm.nc") %>%
  rotate()

#Selected countries
data("World")
countries <- World %>%
  filter(name %in% c("Ghana","Niger","Burkina Faso")) %>%
  st_transform(crs(ltm)@projargs)




crs(ltm) <- crs(countries)

summarise_precip <- function(df) {
  out <- df %>% 
    mask(countries) %>% 
    rasterToPoints() %>% 
    data.frame() %>% 
    tidyr::pivot_longer(cols=-c(x,y),names_to="date",values_to="value") %>% 
    mutate(month=substr(date,7,8) %>% as.numeric()) %>%
    group_by(x,y,month) %>% 
    summarise(precip=sum(value)) %>% 
    ungroup()
  
  return(out)
}

ltm_df <- summarise_precip(ltm)
rm(ltm)

for(year in 2011:2019) {
  dat <- brick(paste0("C:/Richard/R and Python/Environmental Data Science/GIS/ChileClimate/Data_raw/precip.",year,".nc")) %>%
    rotate()
  
  crs(dat) <- crs(countries)
  
  assign(paste0("df",year),summarise_precip(dat))
}


mean_10y <- rbind(df2011,df2012,df2013,df2014,df2015,df2016,df2017,df2018,df2019) %>%
  group_by(x,y,month) %>%
  summarise(prec2011_19=mean(precip))

final <- mean_10y %>%
  inner_join(ltm_df,by=c("x","y","month")) %>%
  rename(prec1981_2010 = precip)


data.table::fwrite(final,"data/prec_summary.csv")