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
surv_df <- isotria_clim %>% 
              subset( !(is.na(log_size_t0) & is.na(surv_t1)) ) %>%
              subset( !(size_t0 %in% 0) ) %>% 
              mutate( year_t1 = year_t1 %>% as.character %>% as.numeric ) %>% 
              subset( year_t1 < 1998 )

years <- surv_df$year_t1 %>% unique %>% sort

# climate model selection --------------------------------------------------------

candidate_mods <- list(

  # null
  surv_t1 ~ log_size_t0 + (log_size_t0 | year_t1),
  
  # precipitation
  surv_t1 ~ log_size_t0 + ppt_t0  + (log_size_t0 | year_t1),
  surv_t1 ~ log_size_t0 + ppt_tm1 + (log_size_t0 | year_t1),
  surv_t1 ~ log_size_t0 + ppt_tm2 + (log_size_t0 | year_t1),
  
  # temperature
  surv_t1 ~ log_size_t0 + tmp_t0  + (log_size_t0 | year_t1),
  surv_t1 ~ log_size_t0 + tmp_tm1 + (log_size_t0 | year_t1),
  surv_t1 ~ log_size_t0 + tmp_tm2 + (log_size_t0 | year_t1),
  
  # annual snow depth
  surv_t1 ~ log_size_t0 + snw_t0  + (log_size_t0 | year_t1),
  surv_t1 ~ log_size_t0 + snw_tm1 + (log_size_t0 | year_t1),
  
  # january snow depth
  surv_t1 ~ log_size_t0 + snw_j0  + (log_size_t0 | year_t1),
  surv_t1 ~ log_size_t0 + snw_jm1 + (log_size_t0 | year_t1)
  
)

# brier score
brier_score <- function(x){

  sum2 <- test_df %>% 
            mutate( pred = boot::inv.logit(x) ) %>% 
            mutate( sum2 = (resp - pred)^2 ) %>% 
            subset( !is.na(sum2) )
  
  sum2$sum2
  
}

surv_scor <- list()

# fit all models
for(ii in 1:length(years)){
  
  train_df <- subset(surv_df, !(year_t1 %in% years[ii]) )
  test_df  <- subset(surv_df,   year_t1 == years[ii]) %>% 
                mutate( resp = surv_t1 )
  
  # fit all models
  surv_clim_m <- lapply( candidate_mods, 
                         function(x) glmer(x, data=train_df, family='binomial') ) %>% 
                  setNames( c('null',
                              'ppt_t0', 'ppt_tm1', 'ppt_tm2',
                              'tmp_t0', 'tmp_tm1', 'tmp_tm2',
                              'snw_t0', 'snw_tm1',
                              'snw_j0', 'snw_jm1' ) )
  
  surv_pred       <- lapply(surv_clim_m, predict, newdata=test_df, re.form=NA)
  surv_scor[[ii]] <- lapply(surv_pred, brier_score) %>% as.data.frame
  
}

# get scores
score_df <- Reduce(function(...) rbind(...), surv_scor) %>% 
              colMeans %>% 
              as.data.frame %>% 
              tibble::add_column(model = row.names(.), .before=1) %>% 
              rename( score = "." ) %>% 
              arrange( score )
  
# fit best model with all data
best_mod <- glmer(surv_t1 ~ log_size_t0 + log_size_t02 + snw_jm1 + (log_size_t0 | year_t1),
                  data = surv_df, family='binomial')

write.csv(coef(best_mod)$year_t1, 
          'results/ml_mod_sel/surv/surv_best_mod_cv.csv',
          row.names=F)
write.csv(score_df, 'results/ml_mod_sel/surv/surv_clim_sel_cv.csv', row.names = F)


# plot -----------------------------------------------------------

coefs1 <- best_mod %>% fixef
coefs2 <- glmer(surv_t1 ~ log_size_t0 + log_size_t02 + ppt_t0 + (log_size_t0 | year_t1),
                data = surv_df, family='binomial') %>% 
            fixef
coefs3 <- glmer(surv_t1 ~ log_size_t0 + log_size_t02 + snw_j0 + (log_size_t0 | year_t1),
                data = surv_df, family='binomial') %>% 
            fixef

# quantiles of climate predictor
clim_quant1 <- surv_df$snw_jm1 %>% unique %>% quantile
clim_quant2 <- surv_df$ppt_t0  %>% unique %>% quantile
clim_quant3 <- surv_df$snw_j0  %>% unique %>% quantile

x_seq <- seq( min(surv_df$log_size_t0, na.rm=T),
              max(surv_df$log_size_t0, na.rm=T),
              length.out = 100 )

y_low1  <- coefs1[1] +
          coefs1[2] * x_seq +
          coefs1[3] * x_seq^2 +
          coefs1[4] * clim_quant1['25%']
y_high1 <- coefs1[1] +
          coefs1[2] * x_seq +
          coefs1[3] * x_seq^2 +
          coefs1[4] * clim_quant1['75%']

y_low2  <- coefs2[1] +
          coefs2[2] * x_seq +
          coefs2[3] * x_seq^2 +
          coefs2[4] * clim_quant2['25%']
y_high2 <- coefs2[1] +
          coefs2[2] * x_seq +
          coefs2[3] * x_seq^2 +
          coefs2[4] * clim_quant2['75%']

y_low3  <- coefs3[1] +
           coefs3[2] * x_seq +
           coefs3[3] * x_seq^2 +
           coefs3[4] * clim_quant3['25%']
y_high3 <- coefs3[1] +
           coefs3[2] * x_seq +
           coefs3[3] * x_seq^2 +
           coefs3[4] * clim_quant3['75%']


tiff('results/ml_mod_sel/surv_cv.tiff',
     unit="in", width=6.3, height=6.3, res=400, compression="lzw")

par( mfrow=c(2,2), 
     mar = c(3.5,3.5,0.2,0.2), mgp = c(2.1,0.7,0), 
     cex.lab = 1.2 )

plot(jitter(surv_t1) ~ log_size_t0, data=surv_df,
     ylab = 'survival rate')
lines(x_seq, boot::inv.logit(y_low1), lwd=2, col = 'red')
lines(x_seq, boot::inv.logit(y_high1), lwd=2, col = 'blue')

  legend(2,0.75,
       c('low snw_jm1','high snw_jm1'), lwd = 2,
       col = c('red','blue'), bty = 'n', cex = 1.2)


plot(jitter(surv_t1) ~ log_size_t0, data=surv_df,
     ylab = 'survival rate')
lines(x_seq, boot::inv.logit(y_low2), lwd=2, col = 'red')
lines(x_seq, boot::inv.logit(y_high2), lwd=2, col = 'blue')

legend(2,0.75,
       c('low ppt_t0','high ppt_t0'), lwd = 2,
       col = c('red','blue'), bty = 'n', cex = 1.2)


plot(jitter(surv_t1) ~ log_size_t0, data=surv_df,
     ylab = 'survival rate')
lines(x_seq, boot::inv.logit(y_low3), lwd=2, col = 'red')
lines(x_seq, boot::inv.logit(y_high3), lwd=2, col = 'blue')

legend(2,0.75,
       c('low snw_j0','high snw_j0'), lwd = 2,
       col = c('red','blue'), bty = 'n', cex = 1.2)

dev.off()

