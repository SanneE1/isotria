# download a set of relevant climate means for a grid of conterminous USA coordinates
# 1. Format coordinates
# 2. Extract chelsa data
# 3. Extract PET data (CGIAR-CS; http://www.cgiar-csi.org/data)
library(RCurl)
library(testthat)
library(raster)
library(dplyr)
library(tidyr)
library(measurements)
library(leaflet)
options(stringsAsFactors = F)

# 1. Format coordinates -------------------------------------------

# read and format county data
count_usa <- read.csv('data/sdm/coord_usa_counties.csv')
count_iso <- read.csv('data/sdm/isotria_counties.csv') %>% 
              mutate( county = gsub(' \\([0-9]{5}\\)','',county) ) %>% 
              mutate( county = trimws(county) ) %>% 
              mutate( state  = trimws(state) ) %>% 
              mutate( presence = 1 ) 
count_crd <- full_join( count_iso, count_usa ) %>% 
              dplyr::select(state, county, latitude, longitude, presence) %>% 
              group_by(state, county) %>% 
              summarise( lat = mean(latitude),
                         lon = mean(longitude),
                         presence = mean(presence) ) %>% 
              mutate( presence = replace(presence, is.na(presence), 0 ) ) %>% 
              ungroup %>% 
              subset( state %in% unique(count_iso$state) ) 

# annual climatologies: what to download
annual_df <- data.frame( variable =  c('mean diurnal range', 
                                       'isothermality', 
                                       'temperature seasonality', 
                                       'temperature annual range [C/10]',
                                       'precipitation seasonality',
                                       'annual mean temperature [C/10]',
                                       'annual precipitation'),
                         var      = c('mdr', 'iso', 'tmps', 'tmpar', 
                                      'ps', 'mat', 'map'),
                         links_v = c('https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/bioclim/integer/CHELSA_bio10_02.tif',
                                     'https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/bioclim/integer/CHELSA_bio10_03.tif',
                                     'https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/bioclim/integer/CHELSA_bio10_04.tif',
                                     'https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/bioclim/integer/CHELSA_bio10_07.tif',
                                     'https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/bioclim/integer/CHELSA_bio10_15.tif',
                                     'https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/bioclim/integer/CHELSA_bio10_01.tif',
                                     'https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/bioclim/integer/CHELSA_bio10_12.tif'),
                         stringsAsFactors = F )

# download data frame
sapply(annual_df$links_v, url.exists)

# coordinates
coord_df   <- count_crd
site_coord <- matrix(c(coord_df$lon,
                       coord_df$lat),
                      dimnames = list(rep('value',nrow(coord_df)),
                                      c('Long','Lat') ),
                      byrow = FALSE, nrow = nrow(coord_df) )
         

# 2. Extract CHELSA DATA ---------------------------------------------------------------

# extract year and monthly data
extract_year_month <- function( ii ){
  
  # download file
  file_path <- annual_df$links_v[ii]
  download.file( file_path, destfile = 'temp.tif', mode = "wb")
  
  # get climate information 
  
  # read raster file
  raster_file <- grep('.tif',list.files(), value=T)
  repP        <- raster( raster_file )
  
  # extract info from raster file
  values_clim <- raster::extract(repP, site_coord, method = 'bilinear')
  
  # remove file you just downloaded
  file.remove( grep('.tif$',list.files(),value=T)[1] )
  
  out_df      <- data.frame( variable = annual_df$variable[ii],
                             var      = annual_df$var[ii],
                             value    = values_clim,
                             stringsAsFactors = F) %>% 
                   bind_cols( as.data.frame(site_coord) )
  
  return( out_df )
  
}

# download and format data
# mat_l    <- lapply(1:2, extract_year_month)
mat_l    <- lapply(1:nrow(annual_df), extract_year_month)
chels_df <- bind_rows(mat_l) %>% 
              rename( lon = Long,
                      lat = Lat ) %>% 
              subset( !is.na(lon) ) %>% 
              left_join( count_crd ) 

# 3. Extract PET data ---------------------------------------------

# downloaded from http://www.cgiar-csi.org/data
# File at https://figshare.com/articles/Global_Aridity_Index_and_Potential_Evapotranspiration_ET0_Climate_Database_v2/7504448/3
repP        <- raster( 'data/sdm/et0_yr.tif' )

# extract info from raster file
values_clim <- raster::extract(repP, site_coord, method = 'bilinear')

pet_df  <- data.frame( variable = 'Potential evapotranspiration (mm)',
                        var      = 'pet',
                        value    = values_clim,
                        stringsAsFactors = F) %>% 
              bind_cols( as.data.frame(site_coord) ) %>% 
              rename( lon = Long,
                      lat = Lat ) %>% 
              subset( !is.na(lon) ) %>% 
              left_join( count_crd )

# presence_absence
pa_df    <- dplyr::select( chels_df, state, county, presence )

# put it all together
means_df <- bind_rows( chels_df, pet_df ) %>% 
              dplyr::select(-variable) %>%
              unique %>% 
              spread( var, value ) %>% 
              mutate( p_pet = map - pet )


means_df$mat %>% hist
abline(v=subset(means_df, state == 'ME')$mat[1])

plot(map ~ mat, data=means_df)
plot( map ~ pet, data=means_df)              

means_df %>% 
  mutate( p_pet = map - pet ) %>% 
  .$p_pet %>% 
  hist
abline(v=)

# ppet from Maine (where our populations come from)
ppet_me    <- subset(means_df, state == 'ME' &
                     presence == 1)$p_pet

# calculate percentile of p_pet of counties from maine
percentile <- ecdf(means_df$p_pet)

percentile(ppet_me[5])
quantile(means_df$p_pet, ppet_me)


# store values
write.csv(means_df, 'data/sdm/climate_means_counties.csv', row.names=F)

