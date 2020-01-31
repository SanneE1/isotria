# detrend population-level climate; put it in "long" form
prism_clim_form <- function(clim_x, yearz, m_back, m_obs){
  
  # "spread" the 12 months
  clim_m    <- clim_x %>%  
                select( -variable ) %>% 
                spread( month, value )
  
  # create anomalies from the spreaded months
  clim_detr <- apply(clim_m[,-1], 2, FUN = scale, center = T, scale = T) %>%
                as.data.frame() %>%
                bind_cols( clim_m[,"year", drop=F] ) %>%
                dplyr::select( c("year", 1:12) )
  
  yr_range  <- range(yearz)
  
  # detrended climate in "long" form
  long_out  <- clim_detr %>%
                  subset(year < (yr_range[2]+1) & year > (yr_range[1] - 6) ) %>%
                  gather('month', 'clim_value', '1':'12') %>%
                  setNames(c("year", "month_num", "clim_value") ) %>% 
                  mutate(month_num = factor(month_num, levels = paste0(1:12)) ) %>% 
                  arrange(year, month_num)
  
  # select temporal extent
  clim_back <- function(yrz, m_obs, dat){
    id <- which(dat$year == yrz & dat$month_num == m_obs)
    r  <- c( id:(id - (m_back-1)) )
    return(dat[r,"clim_value"])
  }
  
  # climate data in matrix form 
  year_by_month_mat <- function(dat, years){
    do.call(rbind, dat) %>% 
      as.data.frame %>%
      tibble::add_column(year = years, .before=1)
  }
  
  # calculate monthly precipitation values
  clim_x_l  <- lapply(yearz, clim_back, m_obs, long_out)
  x_clim    <- year_by_month_mat(clim_x_l, yearz)
  return(x_clim)

}
