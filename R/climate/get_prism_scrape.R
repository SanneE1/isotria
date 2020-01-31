# semi-automatic download of PRISM climate data
setwd("C:/")
library(dplyr)
library(tidyr)
library(archive)
library(raster)
library(prism)
library(RCurl)

site_df     <- read.csv('C:/cloud/Dropbox/isotria_idiv/data/site_coords.csv') 
site_coord  <- dplyr::select(site_df, lon,lat) %>% as.matrix



archive_extract( archive(file_dest[ii]), "temp_dir")

# PRISM set up -------------------------------------------------------------------------------

# what do we need from PRISM?
prism_df <- expand.grid( variable = c('ppt','tmean'),
                          year     = c(1984:2000),
                          month    = c(paste0('0',1:9),paste0(10:12)),
                          stringsAsFactors = F) %>% 
                arrange(variable,year,month)

# set up reading path
read_dir  <- 'ftp://prism.oregonstate.edu/monthly/'

# produce file name based on index 'ii'
produce_file_name <- function(ii){
  
  if( prism_df$variable[ii] == 'ppt'){
    file_root  <- paste0(prism_df$variable[ii],'/',prism_df$year[ii],
                         '/PRISM_',prism_df$variable[ii],'_stable_4kmM3_',
                          prism_df$year[ii],prism_df$month[ii],'_bil.zip')
  }
  
  if( prism_df$variable[ii] == 'tmean'){
    file_root  <- paste0(prism_df$variable[ii],'/',prism_df$year[ii],
                         '/PRISM_',prism_df$variable[ii],'_stable_4kmM2_',
                          prism_df$year[ii],prism_df$month[ii],'_bil.zip')
  }
  
  return(file_root)
  
}

# get all file links (from file name)
file_names <- lapply(1:nrow(prism_df), produce_file_name) %>% unlist
file_links <- paste0(read_dir,file_names)
file_dest  <- gsub("tmean/[0-9]{4}/|ppt/[0-9]{4}/","",file_names) %>% 
                paste0('C:/',.)


# Extract PRISM DATA ------------------------------------------------------------------------

# extract year and monthly data
extract_year_month <- function(ii){
  
  # extac with archive 
  # devtools::install_github('jimhester/archive')
  file_path <- file_links[ii]
  
  download.file( file_path, destfile = file_dest[ii], mode = "wb")
  archive_extract( archive(file_dest[ii]), "temp_dir")
  # # extract with 7z directly. This does extract directly in getwd()
  # system('"C:\\Program Files\\7-Zip\\7z" x "C:\\cloud\\MEGA\\Projects\\sApropos\\analyses\\CHELSA_temp_1979_01.7z"')
  
  # get climate information ----------------------------------------------------------------
  
  # read raster
  raster_file <- grep('.bil$',list.files('temp_dir'), value=T)
  rast_stack  <- raster(paste0('temp_dir/',raster_file) )
  
  # extract info 
  values_clim <- raster::extract(rast_stack, site_coord,layer=1) #, method = 'bilinear')
  clim_df     <- data.frame( variable = prism_df$variable[ii],
                             year     = prism_df$year[ii],
                             month    = prism_df$month[ii],
                             value    = values_clim,
                             site     = site_df$site,
                             stringsAsFactors = F)
  
  file.remove( paste0('temp_dir/',list.files('temp_dir/')) )
  file.remove( file_dest[ii] )
  
  print(ii)
  
  return(clim_df)

}

# # extract year and monthly data
# check_links <- function(ii){
# 
#   url.exists(file_links_good[ii])
# 
# }
# url.exists(file_links_good[8])
# check_links <- sapply(1:length(file_links_good),check_links)

start <- Sys.time()
# climate_all <- lapply(1:2, extract_year_month)
climate_all <- lapply(1:nrow(prism_df), extract_year_month)
Sys.time() - start

climate_df <- climate_all %>% Reduce(function(...) rbind(...), .)

write.csv(climate_df,
          'C:/cloud/Dropbox/isotria_idiv/data/climate/prism_isotria.csv', 
          row.names=F)
