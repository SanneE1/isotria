setwd("C:/cloud/Dropbox/isotria_idiv/data")
library(leaflet)
library(measurements)
library(dplyr)

# convert lat/lon in decimal form
conv_plot_coord <- function(lat_in, lon_in, from_unit){
  
  coord_df <- data.frame( lat = conv_unit(lat_in,  
                                          from = from_unit, 
                                          to = 'dec_deg'),
                          lon = conv_unit(lon_in, 
                                          from = from_unit, 
                                          to = 'dec_deg'),
                          stringsAsFactors = F) %>% 
                mutate(   lat = as.numeric(lat),
                          lon = as.numeric(lon) )
  
  return(coord_df)
  
}

# site level information
site_df <- list( 
              conv_plot_coord('43 35 47.63', '-70 50 02.59', 'deg_min_sec'),
              conv_plot_coord('43 23 03.93', '-70 46 34.82', 'deg_min_sec'),
              conv_plot_coord('44 23 57.93', '-70 00 05.95', 'deg_min_sec'),
              conv_plot_coord('44 08 22.11', '-70 39 56.67', 'deg_min_sec') 
              ) %>% 
                Reduce(function(...) bind_rows(...), .) %>% 
                mutate( site = c('Abbotts Mountain', 
                                 'Bauneg Beg', 
                                 'Kents Hill', 
                                 'Deer Hill') )

# test locations (they do make sense)
leaflet(data = site_df ) %>% 
  addTiles() %>% 
  addCircleMarkers(~lon, ~lat)

# write out data
write.csv(site_df, 'site_coords.csv', row.names=F)
