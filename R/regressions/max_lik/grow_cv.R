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

# mean squared error score
rmse_score <- function(x){

  sum2 <- test_df %>% 
            mutate( pred = x ) %>% 
            mutate( sum2 = (resp - pred)^2 ) %>% 
            subset( !is.na(sum2) )
  
  sum2$sum2
  
}

grow_scor <- list()

# fit all models
for(ii in 1:length(years)){
  
  train_df <- subset(grow_df, !(year_t1 %in% years[ii]) )
  test_df  <- subset(grow_df,   year_t1 == years[ii]) %>% 
                mutate( resp = log_size_t1 )
  
  # fit all models
  grow_clim_m <- lapply( candidate_mods, 
                         function(x) lmer(x, data=train_df) ) %>% 
                  setNames( c('null',
                              'ppt_t0', 'ppt_tm1', 'ppt_tm2',
                              'tmp_t0', 'tmp_tm1', 'tmp_tm2',
                              'snw_t0', 'snw_tm1',
                              'snw_j0', 'snw_jm1' ) )
  
  grow_pred       <- lapply(grow_clim_m, predict, newdata=test_df, re.form=NA)
  grow_scor[[ii]] <- lapply(grow_pred, rmse_score) %>% as.data.frame
  
}

# get scores
score_df <- Reduce(function(...) rbind(...), grow_scor) %>% 
              colMeans %>% 
              as.data.frame %>% 
              tibble::add_column(model = row.names(.), .before=1) %>% 
              rename( score = "." ) %>% 
              arrange( score )
  
score_df$score %>% plot

# fit best model with all data
best_mod <- lmer(log_size_t1 ~ log_size_t0 + snw_tm1 + (log_size_t0 | year_t1),
                 data = grow_df)

write.csv(coef(best_mod)$year_t1, 
          'results/ml_mod_sel/grow/grow_best_mod_cv.csv',
          row.names=F)
write.csv(score_df, 'results/ml_mod_sel/grow/grow_clim_sel_cv.csv', row.names = F)


# plot -----------------------------------------------------------------

coefs <- best_mod %>% fixef

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


tiff('results/ml_mod_sel/grow_cv.tiff',
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
