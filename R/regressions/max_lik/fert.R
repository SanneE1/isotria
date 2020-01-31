rm(list=ls())
setwd("C:/cloud/Dropbox/isotria_idiv")
library(dplyr)
library(tidyr)
library(testthat)
library(lme4)

library(bbmle)
options(stringsAsFactors = F)
source('C:/cloud/Dropbox/isotria_idiv/analysis/climate/make_anomalies.R')

detach("package:glmmADMB")

# read and format data
source('analysis/regressions/max_lik/read_format_data.R')

# structure model selection -----------------------------------------------

# format flowering data
fert_df   <- isotria_clim %>% 
                subset( !is.na(size_t1) ) %>%
                subset( size_t1 != 0 )

plot(n_flower_t1 ~ log_size_t1 , data=fert_df)


# candidate models
candidate_mods <- list(
  
  n_flower_t1 ~ log_size_t1 + (1 | year_t1),
  n_flower_t1 ~ log_size_t1 + log_size_t12 + (1 | year_t1),
  
  n_flower_t1 ~ log_size_t1 + (log_size_t0 | year_t1),
  n_flower_t1 ~ log_size_t1 + log_size_t12 + (log_size_t0 | year_t1)
)

detach('package:glmmADMB', unload=T)

# fit models
fert_m    <- lapply( candidate_mods, 
                     function(x) glmer(x, data=na.omit(fert_df), 
                                          family=poisson(link = "log")) ) %>% 
                setNames( c('size',   'size2',
                            'size_i', 'size2_i') )

# model selection
fert_sel  <- AICtab(fert_m, weights=T)

# store results 
write.csv( ICtab_to_df(fert_sel), 'results/ml_mod_sel/fert/fert_structure_sel.csv')


# climate model selection --------------------------------------------------------

candidate_mods <- list(
  
  # null
  n_flower_t1 ~ log_size_t1 * Site + (1 | year_t1),
  
  # precipitation
  n_flower_t1 ~ log_size_t1 * Site + ppt_t0  + (1 | year_t1),
  n_flower_t1 ~ log_size_t1 * Site + ppt_tm1 + (1 | year_t1),
  n_flower_t1 ~ log_size_t1 * Site + ppt_tm2 + (1 | year_t1),
  
  # temperature
  n_flower_t1 ~ log_size_t1 * Site + tmp_t0  + (1 | year_t1),
  n_flower_t1 ~ log_size_t1 * Site + tmp_tm1 + (1 | year_t1),
  n_flower_t1 ~ log_size_t1 * Site + tmp_tm2 + (1 | year_t1),
  
  # annual snow depth
  n_flower_t1 ~ log_size_t1 * Site + snw_t0  + (1 | year_t1),
  n_flower_t1 ~ log_size_t1 * Site + snw_tm1 + (1 | year_t1),
  
  # january snow depth
  n_flower_t1 ~ log_size_t1 * Site + snw_j0  + (1 | year_t1),
  n_flower_t1 ~ log_size_t1 * Site + snw_jm1 + (1 | year_t1)
  
)

# fit all models
flow_clim_m <- lapply( candidate_mods[1], 
                       function(x) glmer.nb(x, data=flow_df) ) %>% 
                setNames( c('null',
                            'ppt_t0', 'ppt_tm1', 'ppt_tm2',
                            'tmp_t0', 'tmp_tm1', 'tmp_tm2',
                            'snw_t0', 'snw_tm1',
                            'snw_j0', 'snw_jm1' ) )

# model selection
flow_clim_sel <- AICtab(flow_clim_m, weights=T)

write.csv( ICtab_to_df(flow_clim_sel), 'results/ml_mod_sel/flow/flow_clim_sel.csv')

