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
source('analysis/regressions/max_lik/read_format_data.R')

# structure model selection -----------------------------------------------

# format flowering data
flow_df   <- isotria_clim %>% 
                subset( !is.na(size_t1) ) %>%
                subset( size_t1 != 0 ) %>% 
                mutate( log_size_t1 = log(size_t1) )
 
# candidate models
candidate_mods <- list(
  flower_t1 ~ log_size_t1 + (1 | year_t1),
  flower_t1 ~ log_size_t1 + log_size_t12 + (1 | year_t1),
  # flower_t1 ~ log_size_t1 + Site + (1 | year_t1),
  # flower_t1 ~ log_size_t1 * Site + (1 | year_t1),
  # flower_t1 ~ log_size_t1 * Site + log_size_t02 + (1 | year_t1),
  
  flower_t1 ~ log_size_t1 + (log_size_t1 | year_t1),
  flower_t1 ~ log_size_t1 + log_size_t12 + (log_size_t1 | year_t1)
  # flower_t1 ~ log_size_t1 + Site + (log_size_t0 | year_t1),
  # flower_t1 ~ log_size_t1 * Site + (log_size_t0 | year_t1),
  # flower_t1 ~ log_size_t1 * Site + log_size_t02 + (log_size_t0 | year_t1)
)

# fit models
flow_m    <- lapply( candidate_mods, 
                     function(x) glmer(x, data=flow_df, family='binomial') ) %>%
                setNames( c('size',   'size2',
                            'size_i', 'size2_i') )
                # setNames( c('size',  'size2',  'site',  'ss',  'ss2',
                #             'size_i','size2_i','site_i','ss_i','ss2_i') )

# model selection
# NOTE: CHOSE N. 2, fit of quadratic model is unrealistic
flow_sel  <- AICtab(flow_m, weights=T)

# store results 
write.csv( ICtab_to_df(flow_sel), 'results/ml_mod_sel/flow/flow_structure_sel.csv')


# climate model selection --------------------------------------------------------

candidate_mods <- list(
  
  # null
  flower_t1 ~ log_size_t1 + log_size_t12 + (log_size_t1 | year_t1),
  
  # precipitation
  flower_t1 ~ log_size_t1 + log_size_t12 + ppt_t0  + (log_size_t1 | year_t1),
  flower_t1 ~ log_size_t1 + log_size_t12 + ppt_tm1 + (log_size_t1 | year_t1),
  flower_t1 ~ log_size_t1 + log_size_t12 + ppt_tm2 + (log_size_t1 | year_t1),
  
  # temperature
  flower_t1 ~ log_size_t1 + log_size_t12 + tmp_t0  + (log_size_t1 | year_t1),
  flower_t1 ~ log_size_t1 + log_size_t12 + tmp_tm1 + (log_size_t1 | year_t1),
  flower_t1 ~ log_size_t1 + log_size_t12 + tmp_tm2 + (log_size_t1 | year_t1),
  
  # annual snow depth
  flower_t1 ~ log_size_t1 + log_size_t12 + snw_t0  + (log_size_t1 | year_t1),
  flower_t1 ~ log_size_t1 + log_size_t12 + snw_tm1 + (log_size_t1 | year_t1),
  
  # january snow depth
  flower_t1 ~ log_size_t1 + log_size_t12 + snw_j0  + (log_size_t1 | year_t1),
  flower_t1 ~ log_size_t1 + log_size_t12 + snw_jm1 + (log_size_t1 | year_t1)
  
)

# fit all models
flow_clim_m <- lapply( candidate_mods, 
                       function(x) glmer(x, data=flow_df, family='binomial') ) %>% 
                setNames( c('null',
                            'ppt_t0', 'ppt_tm1', 'ppt_tm2',
                            'tmp_t0', 'tmp_tm1', 'tmp_tm2',
                            'snw_t0', 'snw_tm1',
                            'snw_j0', 'snw_jm1' ) )

# model selection
flow_clim_sel <- AICtab(flow_clim_m, weights=T)

# best model
best_mod      <- flow_clim_m %>% 
                  .[[attributes(flow_clim_sel)$row.names[1]]] %>% 
                  coef %>% 
                  .$year_t1

# store reusults
write.csv( ICtab_to_df(flow_clim_sel), 'results/ml_mod_sel/flow/flow_clim_sel.csv')
write.csv( best_mod, 'results/ml_mod_sel/flow/flow_best_mod.csv')


# plot -----------------------------------------------------------

coefs <- flow_clim_m %>% 
            .[[attributes(flow_clim_sel)$row.names[1]]] %>%
            fixef 

# quantiles of climate predictor
clim_quant <- flow_df$tmp_t0 %>% unique %>% quantile

x_seq <- seq( min(flow_df$log_size_t1, na.rm=T),
              max(flow_df$log_size_t1, na.rm=T),
              length.out = 100 )

y_low  <- coefs[1] + 
          coefs[2] * x_seq + 
          coefs[3] * x_seq^2 + 
          coefs[4] * clim_quant['25%']

y_high <- coefs[1] + 
          coefs[2] * x_seq + 
          coefs[3] * x_seq^2 + 
          coefs[4] * clim_quant['75%']


tiff('results/ml_mod_sel/flow.tiff', 
     unit="in", width=6.3, height=6.3, res=400, compression="lzw")

par( mar = c(3.5,3.5,0.2,0.2), mgp = c(2.1,0.7,0), cex.lab = 2 )
plot(jitter(flower_t1) ~ log_size_t1, data=flow_df,
     ylab = 'flowering probability')
lines(x_seq, boot::inv.logit(y_low), lwd=2, col = 'blue')
lines(x_seq, boot::inv.logit(y_high), lwd=2, col = 'red')

legend(0.6,0.85, 
       c('low tmp t0','high tmp t0'), lwd = 2,
       col = c('blue','red'), bty = 'n', cex = 2)

dev.off()

