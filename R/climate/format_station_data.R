# semi-automatic download of PRISM climate data
setwd("C:/")
options(stringsAsFactors = F)
library(dplyr)
library(tidyr)
library(leaflet)
library(archive)
library(raster)
library(prism)
library(RCurl)
library(htmlwidgets)


# actual isotria sites location
iso_sites <- read.csv('C:/cloud/Dropbox/isotria_idiv/data/site_coords.csv') %>% 
                mutate(site = paste0('site_',site) ) %>% 
                mutate( site = gsub(' ','_',site) )

# station weather data files
file_root <- 'C:/cloud/Dropbox/isotria_idiv/data/climate/station_original/'
zip_name  <- list.files(file_root) %>% grep('.zip',.,value=T)
file_dest <- gsub('.zip','.csv',zip_name)


# unpack the files and write them in correct directory
unpack_file <- function(ii){
  
  # extract .csv in temp dir
  archive_extract( archive( paste0(file_root,zip_name[ii]) ), "temp_dir")
  
  # get the path for csv file
  file.rename( from = paste0('temp_dir/', file_dest[ii]),
               to   = paste0(file_root,file_dest[ii]) )
  
}

# execute function
sapply(1:length(file_dest), unpack_file)


# format station data:
file_l <- list.files('C:/cloud/Dropbox/isotria_idiv/data/climate/station_original/') %>% 
            grep('.csv',.,value=T) %>% 
            paste0(file_root, .)

# station information
stat_df <- lapply(file_l, function(x) 
                            read.csv(x,nrows=5) %>% 
                            t
                 ) %>% 
              Reduce(function(...) rbind(...), .) %>% 
              as.data.frame %>% 
              setNames( c("Station_Network", 
                          "Station_ID", 
                          "Station_Name", 
                          "lat", "lon" ) ) %>% 
              mutate( lat          = as.numeric(lat),
                      lon          = as.numeric(lon),
                      Station_Name = sapply(strsplit(Station_Name," "), `[`, 1) )


# create maps --------------------------------------------------------

# test positions      
leaflet(data = stat_df) %>% 
  addTiles(  ) %>% 
  addCircleMarkers(~lon, ~lat) #%>% 
  # addPopups(~lon, ~lat, ~as.character(Station_Name) )

# relevant weather stations
leaflet(data = stat_df[-c(2,3),]) %>% 
  addTiles(  ) %>% 
  addCircleMarkers(~lon, ~lat) %>% 
  addPopups(~lon, ~lat, ~as.character(Station_Name),
            options = popupOptions( maxWidth = 100, minWidth = 10) )

# site positions
leaflet(data = mutate(iso_sites, 
                      site = gsub('site_','',site) ) 
        ) %>% 
  addTiles(  ) %>% 
  addCircleMarkers(~lon, ~lat) %>% 
  addPopups(~lon, ~lat, ~as.character(site),
            options = popupOptions( maxWidth = 100, minWidth = 10) )

#-c(2,3,6,7)
stat_df %>% 
  mutate( col = 'blue') %>% 
  rename( site = 'Station_Name') %>% 
  bind_rows( iso_sites ) %>% 
  mutate( col = replace(col, is.na(col), 'red') ) %>%
  .[-c(2,3,6),] %>% 
  leaflet %>% 
  addTiles %>%
  addCircleMarkers(~lon, ~lat, color = ~col )
  

# stations to keep
stat_keep <- stat_df[-c(2,3),] 

# File for Pati: station + demographic data (such messy code!)
bind_rows( rename(stat_keep, location_name = Station_Name ) %>% 
              mutate( data_type = 'weather_station') %>% 
              dplyr::select(-Station_Network, -Station_ID),
           rename(iso_sites, location_name = site ) %>% 
              mutate( data_type = 'demographic_data',
                      location_name = gsub('site_', '', location_name)) 
           ) %>% 
  write.csv( 'C:/cloud/Dropbox/isotria_idiv/data/weather_demography_locations.csv',
             row.names=F)


# format climate data ---------------------------------------------------

# read climate data
clim_df <- lapply(file_l, 
                  function(x) read.csv(x,skip=13) ) %>% 
              setNames( stat_df$Station_Name ) %>% 
              .[-c(2,3)]

# function to introduce NAs
replace_na <- function(x){
    replace(x, x=='T' | x=='M' | x=='S', NA) %>% as.numeric
}


# introduce NAs and make columns numeric 
format_site_data <- function(x, clim_var){
  
   x %>% 
      separate(Day, c('year','month','day'),sep='-') %>% 
      mutate( Precipitation   = replace_na(Precipitation),
              Snow.Depth      = replace_na(Snow.Depth),
              Snow.Fall       = replace_na(Snow.Fall),
              Min.Temperature = replace_na(Min.Temperature),
              Max.Temperature = replace_na(Max.Temperature),
              year            = as.numeric(year),
              month           = as.numeric(month),
              day             = as.numeric(day)
              ) %>% 
      mutate( mean_temp = (Min.Temperature + Min.Temperature)/2 ) 

}


# format and store new files
out_formatted <- 'C:/cloud/Dropbox/isotria_idiv/data/climate/station_formatted/'

# store formatted data
lapply(clim_df, format_site_data) %>% 
  Map( function(x,y) write.csv(x, 
                               paste0(out_formatted, y, '.csv'),
                               row.names=F), 
       ., 
       names(.) )

# write down site data
write.csv(stat_keep, 'C:/cloud/Dropbox/isotria_idiv/data/station_data.csv', row.names=F)
