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

# climate data
prism_df <- read.csv('C:/cloud/Dropbox/isotria_idiv/data/climate/prism_isotria.csv')
site_df  <- read.csv('C:/cloud/Dropbox/isotria_idiv/data/climate/station_monthly.csv')

# format
prism_df <- prism_df %>% 
              rename( prism_val = value ) %>% 
              mutate( site = replace(site, 
                                     site == 'Abbotts Mountain' | 
                                     site == 'Bauneg Beg',
                                     'SANFORD')) %>% 
              mutate( site = replace(site, 
                                     site == 'Deer Hill',
                                     'BRIDGTON')) %>%
              mutate( site = replace(site, 
                                     site == 'Kents Hill',
                                     'AUGUSTA') )
site_df    <- site_df %>%  rename( station_val = value )

# merge
prism_stat <- left_join(prism_df, site_df ) %>% 
                  mutate(  date = as.Date(paste('1',paste(month,year,sep='/'),sep='/'),
                                                "%d/%m/%Y")
                        )
  
matplot_prism_stat <- function(clim_var, site_var, legend_logic){
  
  mat_df <- prism_stat %>% 
            subset(variable == clim_var & site == site_var & year > 1990)
  matplot(mat_df$date, dplyr::select(mat_df,prism_val,station_val),
          type='l', main = mat_df$site %>% unique,
          ylab =  mat_df$variable %>% unique)
  if(legend_logic == T)
  legend('topleft',c('prism','station'),col=1:2,lty=1:2,bty='n')

}


tiff('C:/cloud/Dropbox/isotria_idiv/results/climate/prism_vs_station.tiff',
     unit="in", width=6.3, height=9, res=600,compression="lzw")

par(mfcol=c(3,2),mar=c(2,3,1,0.1),mgp=c(2,0.5,0))
matplot_prism_stat('ppt', 'SANFORD', F)
matplot_prism_stat('ppt', 'AUGUSTA', T)
matplot_prism_stat('ppt', 'BRIDGTON', F)

matplot_prism_stat('tmean', 'SANFORD', F)
matplot_prism_stat('tmean', 'AUGUSTA', F)
matplot_prism_stat('tmean', 'BRIDGTON', F)

dev.off()
