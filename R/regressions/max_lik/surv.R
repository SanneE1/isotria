rm(list=ls())
library(dplyr)
library(tidyr)
library(testthat)
library(lme4)
library(bbmle)
options(stringsAsFactors = F)
source('R/climate/make_anomalies.R')

# read and format data
source('R/regressions/max_lik/read_format_data.R')


# structure model selection -----------------------------------------------

# format survival data
surv_df <- isotria_clim %>% 
              subset( !(is.na(log_size_t0) & is.na(surv_t1)) ) %>%
              subset( !(size_t0 %in% 0) ) %>% 
              subset( year_t1 < 1998 )

# candidate models
candidate_mods <- list(
  surv_t1 ~ log_size_t0 + (1 | year_t1),
  surv_t1 ~ log_size_t0 + log_size_t02 + (1 | year_t1),
  # surv_t1 ~ log_size_t0 + Site + (1 | year_t1),
  # surv_t1 ~ log_size_t0 * Site + (1 | year_t1),
  # surv_t1 ~ log_size_t0 * Site + log_size_t02 + (1 | year_t1),
  
  surv_t1 ~ log_size_t0 + (log_size_t0 | year_t1),
  surv_t1 ~ log_size_t0 + log_size_t02 + (log_size_t0 | year_t1)
  # surv_t1 ~ log_size_t0 + Site + (log_size_t0 | year_t1),
  # surv_t1 ~ log_size_t0 * Site + (log_size_t0 | year_t1),
  # surv_t1 ~ log_size_t0 * Site + log_size_t02 + (log_size_t0 | year_t1)
)

# fit models
surv_m    <- lapply( candidate_mods, 
                     function(x) glmer(x, data=surv_df, family='binomial') ) %>%
                setNames( c('size',   'size2',
                            'size_i', 'size2_i') )
                # setNames( c('size',  'size2',  'site',  'ss',  'ss2',
                #             'size_i','size2_i','site_i','ss_i','ss2_i') )

# model selection
surv_sel <- AICtab(surv_m, weights=T)

# store results 
write.csv( ICtab_to_df(surv_sel), 'results/ml_mod_sel/surv/surv_structure_sel.csv')


# climate model selection --------------------------------------------------------

candidate_mods <- list(

  # null
  surv_t1 ~ log_size_t0 + log_size_t02 + (log_size_t0 | year_t1),
  
  # precipitation
  surv_t1 ~ log_size_t0 + log_size_t02 + ppt_t0  + (log_size_t0 | year_t1),
  surv_t1 ~ log_size_t0 + log_size_t02 + ppt_tm1 + (log_size_t0 | year_t1),
  surv_t1 ~ log_size_t0 + log_size_t02 + ppt_tm2 + (log_size_t0 | year_t1),
  
  # temperature
  surv_t1 ~ log_size_t0 + log_size_t02 + tmp_t0  + (log_size_t0 | year_t1),
  surv_t1 ~ log_size_t0 + log_size_t02 + tmp_tm1 + (log_size_t0 | year_t1),
  surv_t1 ~ log_size_t0 + log_size_t02 + tmp_tm2 + (log_size_t0 | year_t1),
  
  # annual snow depth
  surv_t1 ~ log_size_t0 + log_size_t02 + snw_t0  + (log_size_t0 | year_t1),
  surv_t1 ~ log_size_t0 + log_size_t02 + snw_tm1 + (log_size_t0 | year_t1),
  
  # january snow depth
  surv_t1 ~ log_size_t0 + log_size_t02 + snw_j0  + (log_size_t0 | year_t1),
  surv_t1 ~ log_size_t0 + log_size_t02 + snw_jm1 + (log_size_t0 | year_t1)
  
)

# fit all models
surv_clim_m <- lapply( candidate_mods, 
                       function(x) glmer(x, data=surv_df, family='binomial') ) %>% 
                setNames( c('null',
                            'ppt_t0', 'ppt_tm1', 'ppt_tm2',
                            'tmp_t0', 'tmp_tm1', 'tmp_tm2',
                            'snw_t0', 'snw_tm1',
                            'snw_j0', 'snw_jm1' ) )

# remove models which failed to converge
rm_mod <- sapply(surv_clim_m, function(x) x@optinfo$conv$lme4 %>% .$code) %>% 
            unlist %>% 
            names

# remaining models  
surv_clim_m <- surv_clim_m %>% 
                  .[ -which( names(.) %in% rm_mod ) ]

# model selection
surv_clim_sel <- AICtab(surv_clim_m, weights=T)

# best model
best_mod      <- surv_clim_m %>% 
                  .[[attributes(surv_clim_sel)$row.names[1]]] %>% 
                  coef %>% 
                  .$year_t1
          
# store reusults
write.csv( ICtab_to_df(surv_clim_sel), 'results/ml_mod_sel/surv/surv_clim_sel.csv')
write.csv( best_mod, 'results/ml_mod_sel/surv/surv_best_mod.csv')



# plot -----------------------------------------------------------

coefs <- surv_clim_m %>% 
            .[[attributes(surv_clim_sel)$row.names[1]]] %>%
            fixef 

# quantiles of climate predictor
clim_quant <- surv_df$ppt_t0 %>% unique %>% quantile

x_seq <- seq( min(surv_df$log_size_t0, na.rm=T),
              max(surv_df$log_size_t0, na.rm=T),
              length.out = 100 )

y_low  <- coefs[1] + 
          coefs[2] * x_seq + 
          coefs[3] * x_seq^2 + 
          coefs[4] * clim_quant['25%']

y_high <- coefs[1] + 
          coefs[2] * x_seq + 
          coefs[3] * x_seq^2 + 
          coefs[4] * clim_quant['75%']


tiff('results/ml_mod_sel/surv.tiff', 
     unit="in", width=6.3, height=6.3, res=400, compression="lzw")

par( mar = c(3.5,3.5,0.2,0.2), mgp = c(2.1,0.7,0), cex.lab = 2 )
plot(jitter(surv_t1) ~ log_size_t0, data=surv_df,
     ylab = 'survival rate')
lines(x_seq, boot::inv.logit(y_low), lwd=2, col = 'red')
lines(x_seq, boot::inv.logit(y_high), lwd=2, col = 'blue')

legend(2.2,0.6, 
       c('low ppt t0','high ppt t0'), lwd = 2,
       col = c('red','blue'), bty = 'n', cex = 2)

dev.off()

