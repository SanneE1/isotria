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
file_root <- 'C:/cloud/Dropbox/isotria_idiv/data/climate/stations/'
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
file_l <- list.files('C:/cloud/Dropbox/isotria_idiv/data/climate/stations/') %>% 
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
stat_keep <- stat_df[-c(2,3,6),] 


# format climate data ----------------------------------------------

# read climate data
clim_df <- lapply(file_l, 
                  function(x) read.csv(x,skip=13) ) %>% 
              setNames( stat_df$Station_Name ) %>% 
              .[-c(2,3,7)]

# function to introduce NAs
replace_na <- function(x){
    replace(x, x=='T' | x=='M' | x=='S', NA) %>% as.numeric
}


# percent of missing snowfall data -------------------------------------
percent_missing <- function(x, clim_var){
  
  tot_n     <- x %>% nrow 
  
  missing_n <- x %>% 
                  separate(Day, c('year','month','day'),sep='-') %>% 
                  mutate( Snow.Depth      = replace_na(Snow.Depth),
                          Snow.Fall       = replace_na(Snow.Fall),
                          year            = as.numeric(year),
                          month           = as.numeric(month),
                          day             = as.numeric(day) ) %>% 
                  dplyr::select(clim_var) %>% 
                  is.na %>% 
                  sum
  
  return(missing_n / tot_n)
  
}

# missing snow data 
miss_sd <- lapply(clim_df, percent_missing, 'Snow.Depth') %>% 
              unlist %>% 
              t %>% t %>% 
              as.data.frame %>% 
              tibble::add_column(site = row.names(.), .before=1) %>% 
              setNames( c('site','snow_depth_missing') )

miss_sf <- lapply(clim_df, percent_missing, 'Snow.Fall') %>% 
              unlist %>% 
              t %>% t %>% 
              as.data.frame %>% 
              tibble::add_column(site = row.names(.), .before=1) %>% 
              setNames( c('site','snow_fall_missing') )

miss_snow <- left_join( miss_sd, miss_sf )

write.csv(miss_snow, 
          'C:/cloud/Dropbox/isotria_idiv/results/climate/prop_missing_snow.csv', 
          row.names=F)


# pairs plot on snowfall anomalies ---------------------------------------

# introduce NAs and make columns numeric 
format_anom <- function(x, clim_var){
  
  clim_m <- x %>% 
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
              mutate( mean_temp = (Min.Temperature + Min.Temperature)/2 ) %>% 
              group_by(year,month) %>% 
              summarise( Precipitation   = sum(Precipitation, na.rm=T),
                         Snow.Depth      = mean(Snow.Depth, na.rm=T),
                         Snow.Fall       = sum(Snow.Fall, na.rm=T),
                         mean_temp       = mean(mean_temp, na.rm=T),
                         Min.Temperature = mean(Min.Temperature, na.rm=T),
                         Max.Temperature = mean(Max.Temperature, na.rm=T) 
                         ) %>% 
              ungroup %>% 
              subset( year > 1987 & year < 2001 ) %>% 
              mutate(  date              = as.Date(paste('1',paste(month,year,sep='/'),sep='/'),
                                                   "%d/%m/%Y") 
                       ) %>% 
              dplyr::select( year, month, clim_var ) %>% 
              spread( month, clim_var ) 
    
  # monthly anomalies
  out <- apply(clim_m[,-1], 2, FUN = scale, center = T, scale = T) %>%
            as.data.frame %>%
            bind_cols( clim_m[,"year", drop=F] ) %>%
            dplyr::select( year, 1:12 ) %>% 
            gather(month, value, -year) %>% 
            mutate( value = replace(value, 
                                    is.nan(value),
                                    NA) ) %>% 
            setNames( c('year', 'month', clim_var) ) 
  
  return(out)  
  
}

# monthly data format
site_anom  <- function(clim_df, clim_var){
  
  lapply(clim_df, format_anom, clim_var) %>% 
    # introduce site name into the clim variable name
    Map(function(x,y) setNames(x, 
                               c(names(x)[1], names(x)[2], y) 
                               ),
                      .,
                      names(clim_df) ) %>% 
    # merge all four datasets in same data frame
    Reduce(function(...) left_join(...), .)
  
}


# anomalies
anom_sf    <- site_anom(clim_df, 'Snow.Fall')
anom_sd    <- site_anom(clim_df, 'Snow.Depth')
anom_ppt   <- site_anom(clim_df, 'Precipitation')
anom_airt  <- site_anom(clim_df, 'mean_temp')

# correlation between anomalies
dplyr::select(anom_sd, -year, -month, -SANFORD) %>% 
  na.omit %>% cor

dplyr::select(anom_sf, -year, -month) %>% 
  na.omit %>% cor

dplyr::select(anom_ppt, -year, -month) %>% 
  na.omit %>% cor

dplyr::select(anom_airt, -year, -month) %>% 
  na.omit %>% cor


tiff('C:/cloud/Dropbox/isotria_idiv/results/climate/anom_site_snowdepth.tiff',
     unit="in", width=6.3, height=6.3, res=600,compression="lzw")
dplyr::select(anom_sd, -year, -month, -SANFORD) %>% 
  pairs(main='Snow Depth', oma=c(2,2,2.5,1),
        mgp=c(3,0.5,0), line.main= 1)
dev.off()

# store plots
tiff('C:/cloud/Dropbox/isotria_idiv/results/climate/anom_site_snowfall.tiff',
     unit="in", width=6.3, height=6.3, res=600,compression="lzw")
dplyr::select(anom_sf, -year, -month) %>% 
  pairs(main='Snow Fall', oma=c(2,2,2.5,1),
        mgp=c(3,0.5,0), line.main= 1)
dev.off()

tiff('C:/cloud/Dropbox/isotria_idiv/results/climate/anom_site_ppt.tiff',
     unit="in", width=6.3, height=6.3, res=600,compression="lzw")
dplyr::select(anom_ppt, -year, -month) %>% 
  pairs(main='Precipitation', oma=c(2,2,2.5,1),
        mgp=c(3,0.5,0), line.main= 1)
dev.off()

tiff('C:/cloud/Dropbox/isotria_idiv/results/climate/anom_site_airt.tiff',
     unit="in", width=6.3, height=6.3, res=600,compression="lzw")
dplyr::select(anom_airt, -year, -month) %>% 
  pairs(main='Mean temperature', oma=c(2,2,2.5,1),
        mgp=c(3,0.5,0), line.main= 1)
dev.off()



# format climate data ------------------------------------------------

# introduce NAs and make columns numeric 
format_clim <- function(x, clim_var){
  
  clim_m <- x %>% 
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
              mutate( mean_temp = (Min.Temperature + Min.Temperature)/2 ) %>% 
              group_by(year,month) %>% 
              summarise( Precipitation   = sum(Precipitation, na.rm=T),
                         Snow.Depth      = mean(Snow.Depth, na.rm=T),
                         Snow.Fall       = sum(Snow.Fall, na.rm=T),
                         mean_temp       = mean(mean_temp, na.rm=T),
                         Min.Temperature = mean(Min.Temperature, na.rm=T),
                         Max.Temperature = mean(Max.Temperature, na.rm=T) 
                         ) %>% 
              ungroup %>% 
              subset( year > 1983 & year < 2001 ) %>% 
              mutate(  date              = as.Date(paste('1',paste(month,year,sep='/'),sep='/'),
                                                   "%d/%m/%Y") 
                       ) %>% 
              dplyr::select( year, month, clim_var )
    
  return(clim_m)  
  
}

# monthly data format
site_month  <- function(clim_df, clim_var){
  
  lapply(clim_df, format_clim, clim_var) %>% 
    # introduce site name into the clim variable name
    Map(function(x,y) setNames(x, 
                               c(names(x)[1], names(x)[2], y) 
                               ),
                      .,
                      names(clim_df) ) %>% 
    # merge all four datasets in same data frame
    Reduce(function(...) left_join(...), .)
  
}

# anomalies
month_sf    <- site_month(clim_df, 'Snow.Fall')
month_sd    <- site_month(clim_df, 'Snow.Depth')
month_ppt   <- site_month(clim_df, 'Precipitation')
month_airt  <- site_month(clim_df, 'mean_temp')


# station monthly anomalies data frame
stat_month_df <- list( gather(month_sf,   site, value, BRIDGTON:PORTLAND) %>% 
                        mutate( variable = 'snow_fall' ),
                      gather(month_sd,   site, value, BRIDGTON:PORTLAND) %>% 
                        mutate( variable = 'snow_depth' ),
                      gather(month_ppt,  site, value, BRIDGTON:PORTLAND) %>% 
                        mutate( variable = 'ppt' ),
                      gather(month_airt, site, value,     BRIDGTON:PORTLAND) %>% 
                        mutate( variable = 'tmean' ) ) %>% 
                  Reduce(function(...) rbind(...), .)

stat_month_df %>% subset(variable=='ppt')

# store anomalies
write.csv(stat_month_df, 
          'C:/cloud/Dropbox/isotria_idiv/data/climate/station_monthly.csv',
          row.names=F)
         

# NAs by month ---------------------------------------------------

# introduce NAs and make columns numeric 
count_nas <- function(x, clim_var){
  
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
    mutate( mean_temp = (Min.Temperature + Min.Temperature)/2 ) %>% 
    dplyr::select( c('year','month', clim_var) ) %>% 
    filter( .[,3] %>% is.na ) %>% 
    count(year,month)
              
}

na_n_df <- lapply(clim_df, count_nas, 'Snow.Depth') %>% 
              Map( function(x, y) setNames(x, c('year','month', paste0('n_',y)) ),
                   ., names(clim_df) ) %>% 
              Reduce( function(...) full_join(...), .) %>% 
              subset( year > 1983 & year < 2001) %>% 
              subset( month > 10 | month < 5 ) %>% 
              as.data.frame

