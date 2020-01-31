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

# format survival data
dorm_df <- isotria_clim %>% 
              subset( !(is.na(log_size_t0) & is.na(surv_t1)) ) %>%
              subset( !(size_t0 %in% 0) )

# candidate models
candidate_mods <- list(
  
  dormancy_t1 ~ (1 | year_t1),
  
  dormancy_t1 ~ log_size_t0 + (1 | year_t1),
  dormancy_t1 ~ log_size_t0 + log_size_t02 + (1 | year_t1),
  # dormancy_t1 ~ log_size_t0 + Site + (1 | year_t1),
  # dormancy_t1 ~ log_size_t0 * Site + (1 | year_t1),
  # dormancy_t1 ~ log_size_t0 * Site + log_size_t02 + (1 | year_t1),
  
  dormancy_t1 ~ log_size_t0 + (log_size_t0 | year_t1),
  dormancy_t1 ~ log_size_t0 + log_size_t02 + (log_size_t0 | year_t1)
  # dormancy_t1 ~ log_size_t0 + Site + (log_size_t0 | year_t1),
  # dormancy_t1 ~ log_size_t0 * Site + (log_size_t0 | year_t1),
  # dormancy_t1 ~ log_size_t0 * Site + log_size_t02 + (log_size_t0 | year_t1)
)

# fit models
dorm_m    <- lapply( candidate_mods, 
                     function(x) glmer(x, data=dorm_df, family='binomial') ) %>%
                setNames( c('null',
                            'size',   'size2',
                            'size_i', 'size2_i') )
                # setNames( c('size',  'size2',  'site',  'ss',  'ss2',
                #             'size_i','size2_i','site_i','ss_i','ss2_i') )

# model selection
surv_sel <- AICtab(dorm_m, weights=T)

# store results 
write.csv( ICtab_to_df(surv_sel), 'results/ml_mod_sel/surv/surv_structure_sel.csv')


# climate model selection --------------------------------------------------------

candidate_mods <- list(

  # null
  dormancy_t1 ~ log_size_t0 + (1 | year_t1),
  
  # precipitation
  dormancy_t1 ~ log_size_t0 + ppt_t0  + (1 | year_t1),
  dormancy_t1 ~ log_size_t0 + ppt_tm1 + (1 | year_t1),
  dormancy_t1 ~ log_size_t0 + ppt_tm2 + (1 | year_t1),
  
  # temperature
  dormancy_t1 ~ log_size_t0 + tmp_t0  + (1 | year_t1),
  dormancy_t1 ~ log_size_t0 + tmp_tm1 + (1 | year_t1),
  dormancy_t1 ~ log_size_t0 + tmp_tm2 + (1 | year_t1),
  
  # annual snow depth
  dormancy_t1 ~ log_size_t0 + snw_t0  + (1 | year_t1),
  dormancy_t1 ~ log_size_t0 + snw_tm1 + (1 | year_t1),
  
  # january snow depth
  dormancy_t1 ~ log_size_t0 + snw_j0  + (1 | year_t1),
  dormancy_t1 ~ log_size_t0 + snw_jm1 + (1 | year_t1)
  
)

# fit all models
dorm_clim_m <- lapply( candidate_mods, 
                       function(x) glmer(x, data=dorm_df, family='binomial') ) %>% 
                setNames( c('null',
                            'ppt_t0', 'ppt_tm1', 'ppt_tm2',
                            'tmp_t0', 'tmp_tm1', 'tmp_tm2',
                            'snw_t0', 'snw_tm1',
                            'snw_j0', 'snw_jm1' ) )

# model selection
dorm_clim_sel <- AICtab(dorm_clim_m, weights=T)

# I CHOSE SECOND BEST best model, because lag time does not seem to be important
best_mod      <- dorm_clim_m %>% 
                  .[[attributes(dorm_clim_sel)$row.names[2]]] %>% 
                  coef %>% 
                  .$year_t1
          
# store reusults
write.csv( ICtab_to_df(dorm_clim_sel), 'results/ml_mod_sel/dorm/dorm_clim_sel.csv')
write.csv( best_mod, 'results/ml_mod_sel/dorm/dorm_best_mod.csv')


# plot -----------------------------------------------------------

coefs1 <- dorm_clim_m %>% 
            .[[attributes(dorm_clim_sel)$row.names[1]]] %>%
            fixef 
coefs2 <- dorm_clim_m %>% 
            .[[attributes(dorm_clim_sel)$row.names[2]]] %>%
            fixef 

# quantiles of climate predictor
clim_quant1 <- dorm_df$ppt_tm2 %>% unique %>% quantile
clim_quant2 <- dorm_df$snw_j0 %>% unique %>% quantile

x_seq <- seq( min(dorm_df$log_size_t0, na.rm=T),
              max(dorm_df$log_size_t0, na.rm=T),
              length.out = 100 )

y_low1  <- coefs1[1] + 
           coefs1[2] * x_seq + 
           coefs1[3] * clim_quant1['25%']
y_high1 <- coefs1[1] + 
           coefs1[2] * x_seq + 
           coefs1[3] * clim_quant1['75%']

y_low2  <- coefs2[1] + 
           coefs2[2] * x_seq + 
           coefs2[3] * clim_quant2['25%']
y_high2 <- coefs2[1] + 
           coefs2[2] * x_seq + 
           coefs2[3] * clim_quant2['75%']


tiff('results/ml_mod_sel/dorm.tiff', 
     unit="in", width=6.3, height=9, res=400, compression="lzw")

par( mfrow = c(2,1), 
     mar = c(3.5,3.5,1.5,0.2), mgp = c(2.1,0.7,0), 
     cex.lab = 2, cex.main = 2 )

plot(jitter(dormancy_t1) ~ log_size_t0, data=dorm_df,
     ylab = 'probability of dormancy',
     main = "Best model")
lines(x_seq, boot::inv.logit(y_low1), lwd=2, col = 'red')
lines(x_seq, boot::inv.logit(y_high1), lwd=2, col = 'blue')

legend(1.8,0.8, 
       c('low ppt tm2','high ppt tm2'), lwd = 2,
       col = c('red','blue'), bty = 'n', cex = 2)


plot(jitter(dormancy_t1) ~ log_size_t0, data=dorm_df,
     ylab = 'probability of dormancy',
     main = "Second best model")
lines(x_seq, boot::inv.logit(y_low2), lwd=2, col = 'red')
lines(x_seq, boot::inv.logit(y_high2), lwd=2, col = 'blue')

legend(1.8,0.8, 
       c('low jan snow t0','high jan snow t0'), lwd = 2,
       col = c('red','blue'), bty = 'n', cex = 2)

dev.off()

