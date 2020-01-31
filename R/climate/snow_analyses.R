# semi-automatic download of PRISM climate data
setwd("C:/cloud/Dropbox/isotria_idiv/")
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
iso_sites <- read.csv('data/site_coords.csv') %>% 
                mutate(site = paste0('site_',site) ) %>% 
                mutate( site = gsub(' ','_',site) )

# climate stations data
stat_df   <- read.csv('data/station_data.csv')

# relevant weather stations
leaflet(data = stat_df) %>% 
  addTiles( ) %>% 
  addCircleMarkers(~lon, ~lat) %>% 
  addPopups(~lon, ~lat, ~as.character(Station_Name) )

# site positions
leaflet(data = mutate(iso_sites, 
                      site = gsub('site_','',site) ) 
        ) %>% 
  addTiles( ) %>% 
  addCircleMarkers(~lon, ~lat) %>% 
  addPopups(~lon, ~lat, ~as.character(site),
            options = popupOptions( maxWidth = 100, minWidth = 10) )

#-c(2,3,6,7)
stat_df %>% 
  mutate( col = 'blue') %>% 
  rename( site = 'Station_Name') %>% 
  bind_rows( iso_sites ) %>% 
  mutate( col = replace(col, is.na(col), 'red') ) %>%
  leaflet %>% 
  addTiles %>%
  addCircleMarkers(~lon, ~lat, color = ~col )
  

# format climate data ----------------------------------------------

file_l <- paste0( 'data/climate/station_formatted/',
                  list.files( 'data/climate/station_formatted/') )

# read climate data
clim_df <- lapply(file_l, function(x) read.csv(x) ) %>% 
              setNames( gsub('data/climate/station_formatted/|.csv','',file_l) ) 

# function to introduce NAs
replace_na <- function(x){
    replace(x, x=='T' | x=='M' | x=='S', NA) %>% as.numeric
}


# percent of missing snowfall data -------------------------------------
percent_missing <- function(x, clim_var){
  
  tot_n     <- x %>% nrow 
  
  missing_n <- x %>% 
                  dplyr::select(clim_var) %>% 
                  is.na %>% 
                  sum
  
  return(missing_n / tot_n)
  
}

# missing snow data 
miss_sd   <- lapply(clim_df, percent_missing, 'Snow.Depth') %>% 
                unlist %>% 
                t %>% t %>% 
                as.data.frame %>% 
                tibble::add_column(site = row.names(.), .before=1) %>% 
                setNames( c('site','snow_depth_missing') )

miss_sf   <- lapply(clim_df, percent_missing, 'Snow.Fall') %>% 
                unlist %>% 
                t %>% t %>% 
                as.data.frame %>% 
                tibble::add_column(site = row.names(.), .before=1) %>% 
                setNames( c('site','snow_fall_missing') )

miss_snow <- left_join( miss_sd, miss_sf )

write.csv(miss_snow, 
          'C:/cloud/Dropbox/isotria_idiv/results/climate/prop_missing_snow.csv', 
          row.names=F)

# NAs by month ---------------------------------------------------

# introduce NAs and make columns numeric 
prop_nas <- function(x, clim_var, time_group){
  
  na_df  <- x %>% 
              dplyr::select( c(time_group, clim_var) ) %>% 
              filter( .[,2] %>% is.na ) %>% 
              count_(time_group) %>% 
              setNames( c(time_group, 'na_n') )
  
  # total count   
  tot_df <- x %>% 
              dplyr::select( c(time_group, clim_var) ) %>% 
              count_(time_group) %>% 
              setNames( c(time_group, 'tot_n') )
    
  left_join(tot_df, na_df) %>% 
    mutate( prop_na = na_n / tot_n )
  
}

# nas by year and month
na_yr <- lapply(clim_df, prop_nas, 'Snow.Depth', 'year' ) %>% 
              lapply( function(x) dplyr::select(x, year,prop_na) ) %>% 
              Map( function(x, y) setNames(x, c('year', paste0('na_prop_',y)) ),
                   ., names(clim_df) ) %>% 
              Reduce( function(...) full_join(...), .) %>% 
              subset( year > 1980 & year < 2001) %>% 
              as.data.frame

na_mon <- lapply(clim_df, function(x) subset(x, year > 1980 & year < 2001) ) %>% 
              lapply(prop_nas, 'Snow.Depth', 'month' ) %>% 
              lapply( function(x) dplyr::select(x, month,prop_na) ) %>% 
              Map( function(x, y) setNames(x, c('month', paste0('na_prop_',y)) ),
                   ., names(clim_df) ) %>% 
              Reduce( function(...) full_join(...), .) %>% 
              as.data.frame

# plots
tiff('C:/cloud/Dropbox/isotria_idiv/results/climate/na_prop_snow_depth.tiff',
     unit="in", width=4.5, height=6.3, res=600,compression="lzw")

par(mfrow=c(2,1), mar=c(3,3,0.5,0.5), mgp=c(1.8,0.7,0) )
# proportion missing by year
matplot(na_yr$year, na_yr[,-1], type='l',
        ylab = 'proportion missing data',
        xlab = 'year',
        lty=1, lwd=2)
legend('topright', bty='n', 
       lty=1, lwd=2, col=1:4, 
       gsub('na_prop_','',names(na_yr)[-1]) )

mon_seq     <- c(8:12,1:7)
na_mon_ordr <- na_mon[mon_seq,] %>% 
                  mutate( month = 1:12)
matplot(na_mon_ordr$month, na_mon_ordr[,-1], type='l',
        ylab = 'proportion missing data',
        xlab = 'month',
        lty=1, lwd=2, xaxt='n')
axis(1, at = c(3,6,9), c('Oct','Jan','Apr') )

dev.off()


# daily correlation between stations? --------------------------------------------

na_intro <- function(clim_x){
  
  clim_x %>% 
    mutate( Snow.Depth    = replace_na(Snow.Depth),
            Snow.Fall     = replace_na(Snow.Fall),
            mean_temp     = replace_na(mean_temp) )
  
}

cia = lapply(clim_df, na_intro)

  
