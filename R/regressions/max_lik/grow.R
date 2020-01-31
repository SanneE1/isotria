rm(list=ls())
setwd("C:/cloud/Dropbox/isotria_idiv")
library(dplyr)
library(tidyr)
library(testthat)
library(lme4)
library(bbmle)
options(stringsAsFactors = F)
source('C:/cloud/Dropbox/isotria_idiv/analysis/climate/make_anomalies.R')

# read and format data
source('C:/cloud/Dropbox/isotria_idiv/analysis/regressions/max_lik/read_format_data.R')

# ICtab object to a data frame
ICtab_to_df <- function(x){
  data.frame( model  = attributes(x)$row.names,
              weight = x$weight )
}

# structure model selection -----------------------------------------------

# format survival data
grow_df   <- isotria_clim %>% 
                subset( !is.na(size_t0) & !is.na(surv_t1) ) %>%
                mutate( log_size_t0 = log(size_t0),
                        log_size_t1 = log(size_t1) ) %>% 
                subset( !(log_size_t0 %in% -Inf ) ) %>% 
                subset( !(log_size_t1 %in% -Inf ) ) %>% 
                subset( !( is.na(log_size_t1) ) )

# candidate models
candidate_mods <- list(
  log_size_t1 ~ log_size_t0 + (1 | year_t1),
  log_size_t1 ~ log_size_t0 + log_size_t02 + (1 | year_t1),
  # log_size_t1 ~ log_size_t0 + Site + (1 | year_t1),
  # log_size_t1 ~ log_size_t0 * Site + (1 | year_t1),
  # log_size_t1 ~ log_size_t0 * Site + log_size_t02 + (1 | year_t1),
  
  log_size_t1 ~ log_size_t0 + (log_size_t0 | year_t1),
  log_size_t1 ~ log_size_t0 + log_size_t02 + (log_size_t0 | year_t1)
  # log_size_t1 ~ log_size_t0 + Site + (log_size_t0 | year_t1),
  # log_size_t1 ~ log_size_t0 * Site + (log_size_t0 | year_t1),
  # log_size_t1 ~ log_size_t0 * Site + log_size_t02 + (log_size_t0 | year_t1)
)

# fit models
grow_m    <- lapply( candidate_mods, 
                     function(x) lmer(x, data=grow_df) ) %>% 
                setNames( c('size',   'size2',
                            'size_i', 'size2_i') )
                # setNames( c('size',  'size2',  'site',  'ss',  'ss2',
                #             'size_i','size2_i','site_i','ss_i','ss2_i') )

# model selection
grow_sel <- AICtab(grow_m, weights=T)

# store results 
write.csv( ICtab_to_df(grow_sel), 'results/ml_mod_sel/grow/grow_structure_sel.csv')


# climate model selection --------------------------------------------------------

candidate_mods <- list(
  
  # null
  log_size_t1 ~ log_size_t0 + (log_size_t0 | year_t1),
  
  # precipitation
  log_size_t1 ~ log_size_t0 + ppt_t0  + (log_size_t0 | year_t1),
  log_size_t1 ~ log_size_t0 + ppt_tm1 + (log_size_t0 | year_t1),
  log_size_t1 ~ log_size_t0 + ppt_tm2 + (log_size_t0 | year_t1),
  
  # temperature
  log_size_t1 ~ log_size_t0 + tmp_t0  + (log_size_t0 | year_t1),
  log_size_t1 ~ log_size_t0 + tmp_tm1 + (log_size_t0 | year_t1),
  log_size_t1 ~ log_size_t0 + tmp_tm2 + (log_size_t0 | year_t1),
  
  # annual snow depth
  log_size_t1 ~ log_size_t0 + snw_t0  + (log_size_t0 | year_t1),
  log_size_t1 ~ log_size_t0 + snw_tm1 + (log_size_t0 | year_t1),
  
  # january snow depth
  log_size_t1 ~ log_size_t0 + snw_j0  + (log_size_t0 | year_t1),
  log_size_t1 ~ log_size_t0 + snw_jm1 + (log_size_t0 | year_t1)
  
)

# fit models
grow_clim_m <- lapply( candidate_mods, 
                       function(x) lmer(x, data = grow_df) ) %>% 
                setNames( c('null',
                            'ppt_t0', 'ppt_tm1', 'ppt_tm2',
                            'tmp_t0', 'tmp_tm1', 'tmp_tm2',
                            'snw_t0', 'snw_tm1',
                            'snw_j0', 'snw_jm1' ) )

# model selection
grow_clim_sel <- AICtab(grow_clim_m, weights=T)

# best (climate) model
best_mod      <- grow_clim_m %>% 
                  .[[attributes(grow_clim_sel)$row.names[2]]] %>% 
                  coef %>% 
                  .$year_t1

# store reusults
write.csv( ICtab_to_df(grow_clim_sel), 'results/ml_mod_sel/grow/grow_clim_sel.csv')
write.csv( best_mod, 'results/ml_mod_sel/grow/grow_best_mod.csv')


# plot -----------------------------------------------------------------

coefs <- grow_clim_m %>% 
            .[[attributes(grow_clim_sel)$row.names[2]]] %>%
            fixef 

# quantiles of climate predictor
clim_quant <- grow_df$snw_jm1 %>% unique %>% quantile

x_seq <- seq( min(grow_df$log_size_t0, na.rm=T),
              max(grow_df$log_size_t0, na.rm=T),
              length.out = 100 )

y_low  <- coefs[1] + 
          coefs[2] * x_seq + 
          coefs[3] * clim_quant['25%']

y_high <- coefs[1] + 
          coefs[2] * x_seq + 
          coefs[3] * clim_quant['75%']


tiff('results/ml_mod_sel/grow.tiff', 
     unit="in", width=6.3, height=6.3, res=400, compression="lzw")

par( mar = c(3.5,3.5,2,0.2), mgp = c(2.1,0.7,0), cex.lab = 2 )
plot(log_size_t1 ~ log_size_t0, data=grow_df,
     ylab = 'log_size_t1',
     main = 'NOTE: best model has no climate..')
lines(x_seq, y_low, lwd=2, col = 'red')
lines(x_seq, y_high, lwd=2, col = 'blue')

legend('topleft',
       c('low jan snow tm1',
         'high jan snow tm1'), lwd = 2,
       col = c('red','blue'), bty = 'n', cex = 2)

dev.off()
