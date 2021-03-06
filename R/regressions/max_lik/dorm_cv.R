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
              subset( !(size_t0 %in% 0) ) %>% 
              mutate( year_t1 = year_t1 %>% as.character %>% as.numeric ) 

years   <- dorm_df$year_t1 %>% unique %>% sort

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

# brier score
brier_score <- function(x){

  sum2 <- test_df %>% 
            mutate( pred = boot::inv.logit(x) ) %>% 
            mutate( sum2 = (resp - pred)^2 ) %>% 
            subset( !is.na(sum2) )
  
  sum2$sum2
  
}

dorm_scor <- list()

# fit all models
for(ii in 1:length(years)){
  
  train_df <- subset(dorm_df, !(year_t1 %in% years[ii]) )
  test_df  <- subset(dorm_df,   year_t1 == years[ii]) %>% 
                mutate( resp = dormancy_t1 )
  
  # fit all models
  dorm_clim_m <- lapply( candidate_mods, 
                         function(x) glmer(x, data=train_df, family='binomial') ) %>% 
                  setNames( c('null',
                              'ppt_t0', 'ppt_tm1', 'ppt_tm2',
                              'tmp_t0', 'tmp_tm1', 'tmp_tm2',
                              'snw_t0', 'snw_tm1',
                              'snw_j0', 'snw_jm1' ) )
  
  dorm_pred       <- lapply(dorm_clim_m, predict, newdata=test_df, re.form=NA)
  dorm_scor[[ii]] <- lapply(dorm_pred, brier_score) %>% as.data.frame
  
}

# get scores
score_df <- Reduce(function(...) rbind(...), dorm_scor) %>% 
              colMeans %>% 
              as.data.frame %>% 
              tibble::add_column(model = row.names(.), .before=1) %>% 
              rename( score = "." ) %>% 
              arrange( score )
  
score_df$score %>% plot

# fit best model with all data
best_mod <- glmer(dormancy_t1 ~ log_size_t0 + ppt_tm2 + (1 | year_t1),
                  data = dorm_df, family='binomial')

write.csv(coef(best_mod)$year_t1, 
          'results/ml_mod_sel/dorm/dorm_best_mod_cv.csv',
          row.names=F)
write.csv(score_df, 'results/ml_mod_sel/dorm/dorm_clim_sel_cv.csv', row.names = F)

# plot -----------------------------------------------------------

coefs1 <- best_mod %>% fixef
coefs2 <- glmer(dormancy_t1 ~ log_size_t0 + snw_j0 + (1 | year_t1),
                data = dorm_df, family='binomial') %>% fixef

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


tiff('results/ml_mod_sel/dorm_cv.tiff',
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

