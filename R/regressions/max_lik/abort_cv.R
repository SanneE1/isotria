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

# brier score - appropriate score for logistic regression
brier_score <- function(x, resp){

  sum2 <- test_df %>% 
            mutate( pred = boot::inv.logit(x) ) %>% 
            mutate( sum2 = (resp - pred)^2 ) %>% 
            subset( !is.na(sum2) )
  
  sum2$sum2
  
}

abor_scor <- list()

# fit all models
for(ii in 1:length(years)){
  
  train_df <- subset(abor_df, !(year_t1 %in% years[ii]) )
  test_df  <- subset(abor_df,   year_t1 == years[ii] ) %>% 
                # IMPORTANT: set correct response variable!!!
                mutate( resp = n_fruit_t1 )
                          
  # fit all models
  abor_clim_m <- lapply( candidate_mods, 
                         function(x) glmer(x, data=train_df, family='binomial') ) %>% 
                  setNames( c('null',
                              'ppt_t0', 'ppt_tm1', 'ppt_tm2',
                              'tmp_t0', 'tmp_tm1', 'tmp_tm2',
                              'snw_t0', 'snw_tm1',
                              'snw_j0', 'snw_jm1' ) )
  
  abor_pred       <- lapply(abor_clim_m, predict, newdata=test_df, re.form=NA)
  abor_scor[[ii]] <- lapply(abor_pred, brier_score) %>% as.data.frame
  
}

# get scores
score_df <- Reduce(function(...) rbind(...), abor_scor) %>% 
              colMeans %>% 
              as.data.frame %>% 
              tibble::add_column(model = row.names(.), .before=1) %>% 
              rename( score = "." ) %>% 
              arrange( score )
  
score_df$score %>% plot

# fit best model with all data
best_mod <- glmer(n_fruit_t1 ~ log_size_t1 + ppt_tm2 + (log_size_t1 | year_t1),
                  data = abor_df, family='binomial')
      
write.csv(coef(best_mod)$year_t1, 
          'results/ml_mod_sel/flow/flow_best_mod_cv.csv',
          row.names=F)
write.csv(score_df, 'results/ml_mod_sel/flow/flow_clim_sel_cv.csv', row.names = F)



# plot -----------------------------------------------------------

coefs1 <- best_mod %>% fixef
coefs2 <- glmer(n_fruit_t1 ~ log_size_t1 + tmp_t0 + (1 | year_t1),
                data = abor_df, family='binomial') %>% fixef

# quantiles of climate predictor
clim_quant1 <- abor_df$ppt_tm2 %>% unique %>% quantile
clim_quant2 <- abor_df$tmp_t0 %>% unique %>% quantile

x_seq <- seq( min(abor_df$log_size_t1, na.rm=T),
              max(abor_df$log_size_t1, na.rm=T),
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



tiff('results/ml_mod_sel/abor_cv.tiff',
     unit="in", width=6.3, height=6.3, res=400, compression="lzw")

par( mfrow = c(2,1),
     mar = c(3.5,3.5,0.2,0.2), mgp = c(2.1,0.7,0),
     cex.lab = 1.2 )

plot(jitter(n_fruit_t1) ~ log_size_t1, data=abor_df,
     ylab='fruiting success probability')
lines(x_seq, boot::inv.logit(y_low1), lwd=2, col = 'red')
lines(x_seq, boot::inv.logit(y_high1), lwd=2, col = 'blue')

legend(2.8,0.8,
       c('low ppt_tm2','high ppt_tm2'), lwd = 2,
       col = c('red','blue'), bty = 'n', cex = 1.2)


plot(jitter(n_fruit_t1) ~ log_size_t1, data=abor_df,
     ylab='fruiting success probability')
lines(x_seq, boot::inv.logit(y_low2), lwd=2, col = 'red')
lines(x_seq, boot::inv.logit(y_high2), lwd=2, col = 'blue')

legend(2.8,0.8,
       c('low tmp_t0','high tmp_t0'), lwd = 2,
       col = c('red','blue'), bty = 'n', cex = 1.2)

dev.off()
