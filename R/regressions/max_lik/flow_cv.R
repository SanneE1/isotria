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
                mutate( log_size_t1 = log(size_t1) )  %>% 
                mutate( year_t1 = year_t1 %>% as.character %>% as.numeric )
 
years     <- flow_df$year_t1 %>% unique %>% sort

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

# brier score - appropriate score for logistic regression
brier_score <- function(x, resp){

  sum2 <- test_df %>% 
            mutate( pred = boot::inv.logit(x) ) %>% 
            mutate( sum2 = (resp - pred)^2 ) %>% 
            subset( !is.na(sum2) )
  
  sum2$sum2
  
}

flow_scor <- list()

# fit all models
for(ii in 1:length(years)){
  
  train_df <- subset(flow_df, !(year_t1 %in% years[ii]) )
  test_df  <- subset(flow_df,   year_t1 == years[ii] ) %>% 
                # IMPORTANT: set correct response variable!!!
                mutate( resp = flower_t1 )
                          
  # fit all models
  flow_clim_m <- lapply( candidate_mods, 
                         function(x) glmer(x, data=train_df, family='binomial') ) %>% 
                  setNames( c('null',
                              'ppt_t0', 'ppt_tm1', 'ppt_tm2',
                              'tmp_t0', 'tmp_tm1', 'tmp_tm2',
                              'snw_t0', 'snw_tm1',
                              'snw_j0', 'snw_jm1' ) )
  
  flow_pred       <- lapply(flow_clim_m, predict, newdata=test_df, re.form=NA)
  flow_scor[[ii]] <- lapply(flow_pred, brier_score) %>% as.data.frame
  
}

# get scores
score_df <- Reduce(function(...) rbind(...), flow_scor) %>% 
              colMeans %>% 
              as.data.frame %>% 
              tibble::add_column(model = row.names(.), .before=1) %>% 
              rename( score = "." ) %>% 
              arrange( score )
  
score_df$score %>% plot

# fit best model with all data
best_mod <- glmer(flower_t1 ~ log_size_t1 + log_size_t12 + tmp_t0 + (log_size_t1 | year_t1),
                  data = flow_df, family='binomial')
      
write.csv(coef(best_mod)$year_t1, 
          'results/ml_mod_sel/flow/flow_best_mod_cv.csv',
          row.names=F)
write.csv(score_df, 'results/ml_mod_sel/flow/flow_clim_sel_cv.csv', row.names = F)


# plot -----------------------------------------------------------

coefs <- best_mod %>% fixef

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


tiff('results/ml_mod_sel/flow_cv.tiff',
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

