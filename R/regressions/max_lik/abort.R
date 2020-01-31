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
abor_df   <- isotria_clim %>% 
                subset( !is.na(size_t1) ) %>%
                subset( size_t1 != 0 ) %>% 
                subset( !(n_fruit_t1 %in% 2) )
  

# candidate models
candidate_mods <- list(
  
  n_fruit_t1 ~ log_size_t1 + (1 | year_t1),
  n_fruit_t1 ~ log_size_t1 + log_size_t12 + (1 | year_t1),
  
  n_fruit_t1 ~ log_size_t1 + (log_size_t1 | year_t1),
  n_fruit_t1 ~ log_size_t1 + log_size_t12 + (log_size_t1 | year_t1)
  
)

# fit models
abor_m    <- lapply( candidate_mods, 
                     function(x) glmer(x, data=abor_df, 
                                          family='binomial') ) %>% 
                setNames( c('size',   'size2',
                            'size_i', 'size2_i') )

# model selection
# I chose size_i because more parsimonium and virtually same weight as size2_i
abor_sel  <- AICtab(abor_m, weights=T)

# store results 
write.csv( ICtab_to_df(abor_sel), 'results/ml_mod_sel/abor/abor_structure_sel.csv')


# climate model selection --------------------------------------------------------

candidate_mods <- list(
  
  # null
  n_fruit_t1 ~ log_size_t1 + (log_size_t1 | year_t1),
  
  # precipitation
  n_fruit_t1 ~ log_size_t1 + ppt_t0  + (log_size_t1 | year_t1),
  n_fruit_t1 ~ log_size_t1 + ppt_tm1 + (log_size_t1 | year_t1),
  n_fruit_t1 ~ log_size_t1 + ppt_tm2 + (log_size_t1 | year_t1),
  
  # temperature
  n_fruit_t1 ~ log_size_t1 + tmp_t0  + (log_size_t1 | year_t1),
  n_fruit_t1 ~ log_size_t1 + tmp_tm1 + (log_size_t1 | year_t1),
  n_fruit_t1 ~ log_size_t1 + tmp_tm2 + (log_size_t1 | year_t1),
  
  # annual snow depth
  n_fruit_t1 ~ log_size_t1 + snw_t0  + (log_size_t1 | year_t1),
  n_fruit_t1 ~ log_size_t1 + snw_tm1 + (log_size_t1 | year_t1),
  
  # january snow depth
  n_fruit_t1 ~ log_size_t1 + snw_j0  + (log_size_t1 | year_t1),
  n_fruit_t1 ~ log_size_t1 + snw_jm1 + (log_size_t1 | year_t1)
  
)

# fit all models
abor_clim_m <- lapply( candidate_mods, 
                       function(x) glmer(x, data=abor_df,
                                         family='binomial') ) %>% 
                setNames( c('null',
                            'ppt_t0', 'ppt_tm1', 'ppt_tm2',
                            'tmp_t0', 'tmp_tm1', 'tmp_tm2',
                            'snw_t0', 'snw_tm1',
                            'snw_j0', 'snw_jm1' ) )

# model selection
abor_clim_sel <- AICtab(abort_clim_m, weights=T)

# best model
best_mod      <- abort_clim_m %>% 
                  .[[attributes(abor_clim_sel)$row.names[1]]] %>% 
                  coef %>% 
                  .$year_t1


# store reusults
write.csv( ICtab_to_df(abor_clim_sel), 'results/ml_mod_sel/abor/abor_clim_sel.csv')
write.csv( best_mod, 'results/ml_mod_sel/abor/abor_best_mod.csv')


# plot -----------------------------------------------------------

coefs <- abor_clim_m %>% 
            .[[attributes(abor_clim_sel)$row.names[1]]] %>%
            fixef 

# quantiles of climate predictor
clim_quant <- abor_df$tmp_t0 %>% unique %>% quantile

x_seq <- seq( min(abor_df$log_size_t1, na.rm=T),
              max(abor_df$log_size_t1, na.rm=T),
              length.out = 100 )

y_low  <- coefs[1] + 
          coefs[2] * x_seq + 
          coefs[3] * clim_quant['25%']

y_high <- coefs[1] + 
          coefs[2] * x_seq + 
          coefs[3] * clim_quant['75%']


tiff('results/ml_mod_sel/abor.tiff', 
     unit="in", width=6.3, height=6.3, res=400, compression="lzw")

par( mar = c(3.5,3.5,0.2,0.2), mgp = c(2.1,0.7,0), cex.lab = 2 )
plot(jitter(n_fruit_t1) ~ log_size_t1, data=abor_df,
     ylab = 'fuiting success probability')
lines(x_seq, boot::inv.logit(y_low), lwd=2, col = 'blue')
lines(x_seq, boot::inv.logit(y_high), lwd=2, col = 'red')

legend(0.6,0.85, 
       c('low tmp t0','high tmp t0'), lwd = 2,
       col = c('blue','red'), bty = 'n', cex = 2)

dev.off()
