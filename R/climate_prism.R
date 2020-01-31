# get PRISM monthly data
# PACKAGE MALFUNCTIONS. Data is incomplete (e.g. tmean for Feb 2001 on)
library(prism)
library(fs)
library(dplyr)

# decide where to download your data
options(prism.path = "C:/cia")

# what to get? variables
vars_df <- expand.grid( month_num = 1:12, 
                        clim_var  = c('tmean','ppt'),
                        stringsAsFactors = F )

# get monthly data, one month at a time, one  from 2002: 
get_monthly_data <- function(ii){

  
  get_prism_dailys(vars_df$clim_var[ii], 
                   minDate   = '1981-02-1', 
                   maxDate   = '1981-02-10', 
                   keepZip   = F)
  
  # store data on your computer
  get_prism_monthlys(vars_df$clim_var[ii], 
                     years   = 2001:2002, 
                     mon     = vars_df$month_num[ii], 
                     keepZip = F)

  # Point Reynes location
  location <- c(-122.959824, 38.109108) 
  
  # grab the files we need
  to_slice <- grep("PRISM_",ls_prism_data()$files,value=T)
  to_slice <- grep(vars_df$clim_var[ii],to_slice, value = T)
  
  # format data in a data fram
  prismfile   <- to_slice[1:10]
  meta_d      <- unlist(prism_md(prismfile, returnDate = T))
  meta_names  <- unlist(prism_md(prismfile))[1]
  param_name  <- strsplit(meta_names, "-")[[1]][3]
  pstack      <- prism_stack(prismfile)
  clim_df     <- unlist(raster::extract(pstack, 
                                        matrix(location, nrow = 1), 
                                        buffer = 10) ) %>% 
                    as.data.frame %>% 
                    mutate( date = as.Date(meta_d),
                            clim_var = vars_df$clim_var[ii] ) %>% 
                    setNames( c('value', 'date', "clim_var") ) 

  # delete the files
  dir_delete( paste0('C:/Users/ac22qawo/Documents/',to_slice) )

  return(clim_df)
  
}

# list of climatic variables
clim_airt <- lapply(1:2, get_monthly_data)
clim_prec <- lapply(1, get_monthly_data)

:nrow(vars_df)