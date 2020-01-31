# format ICtab object to a data frame
ICtab_to_df <- function(x){
  data.frame( model  = attributes(x)$row.names,
              weight = x$weight )
}

# read data ---------------------------------------------------

# replace 0 with NAs
replace_0 <- function(x) replace(x, x == 0, NA)

# demography 
isotria <- read.csv('data/istoria_long.csv') %>% 
              mutate( size_t0 = replace_0(size_t0),
                      size_t1 = replace_0(size_t1) ) %>% 
              mutate( log_size_t0  = log( size_t0 ),
                      log_size_t1  = log( size_t1 ),
                      log_size_t02 = log( size_t0 )^2,
                      log_size_t12 = log( size_t1 )^2 )

# snow
snow_df   <- read.csv('data/snow_depth_m_anomalies.csv')

# prism data
prism_df  <- read.csv('C:/cloud/Dropbox/isotria_idiv/data/climate/prism_isotria.csv') %>% 
                group_by(variable,year,month) %>% 
                summarise( value = mean(value) ) %>% 
                ungroup


# format climate --------------------------------------------------------------------------
years     <- isotria$year_t1 %>% unique
m_obs     <- 6
m_back    <- 36 

# precipitation
ppt_anom  <- prism_df %>% 
                subset(variable == 'ppt' ) %>% 
                prism_clim_form(years, m_back, m_obs) %>% 
                mutate( ppt_t0  = select(.,V1:V12) %>% rowSums,
                        ppt_tm1 = select(.,V13:V24) %>% rowSums,
                        ppt_tm2 = select(.,V25:V36) %>% rowSums ) %>% 
                select( year, ppt_t0, ppt_tm1, ppt_tm2 )

# temperature
tmp_anom  <- prism_df %>% 
                subset(variable == 'tmean' ) %>% 
                prism_clim_form(years, m_back, m_obs) %>%
                mutate( tmp_t0  = select(.,V1:V12) %>% rowSums,
                        tmp_tm1 = select(.,V13:V24) %>% rowSums,
                        tmp_tm2 = select(.,V25:V36) %>% rowSums ) %>% 
                select( year, tmp_t0, tmp_tm1, tmp_tm2 )

# snow depth
snow_an <- snow_df %>% 
                mutate( snw_t0  = select(., V1:V5) %>% rowSums,
                        snw_tm1 = select(., V6:V10) %>% rowSums,
                        snw_j0  = select(., V3) %>% rowSums,
                        snw_jm1 = select(., V8) %>% rowSums ) 

# small paranoid test
expect_true( all( snow_an$snw_j0  == snow_an$V3 ) )
expect_true( all( snow_an$snw_jm1 == snow_an$V8 ) )

# final data frame
snow_anom <- select(snow_an, year, snw_t0, snw_tm1, snw_j0, snw_jm1 )

# all climate anomalies
clim_anom <- Reduce( function(...) full_join(...), 
                     list( ppt_anom, tmp_anom, snow_anom ) ) %>% 
                arrange( year ) %>% 
                rename( year_t1 = year )


# put it all together ---------------------------------------------------------------
isotria_clim <- left_join(isotria, clim_anom) %>% 
                  # factors for analyses
                  mutate( Site    = as.factor(Site),
                          year_t1 = as.factor(year_t1) )
