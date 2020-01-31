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
surv_   <- isotria_clim %>% 
              subset( !(is.na(log_size_t0) & is.na(surv_t1)) ) %>%
              subset( !(size_t0 %in% 0) ) %>% 
              mutate( year_t1 = year_t1 %>% as.character %>% as.numeric ) %>% 
              subset( year_t1 < 1998 )

fru_t0  <- surv_ %>%
              select(Site, New_Tag, Habitat_Man, year_t1, n_fruit_t1, flower_t1) %>% 
              rename( n_fruit_t0 = n_fruit_t1,
                      flower_t0  = flower_t1 ) %>% 
              mutate( year_t1 = year_t1 + 1 )

# test importance of fruiting last year ------------------------------------------

# data frame for analyses
surv_df <- left_join( surv_, fru_t0 ) %>% 
              mutate( n_fruit_t0   = as.factor(n_fruit_t0) ) %>% 
              mutate( col = 'black' ) %>% 
              mutate( col = replace(col, n_fruit_t0  == '1', 'red') )

# glm regression 
mod  <- glm(surv_t1 ~ log_size_t0 + n_fruit_t0,
            data = subset(surv_df, log_size_t0 > 2), 
            family='binomial')
mod1 <- glm(surv_t1 ~ log_size_t0 * n_fruit_t0,
            data = surv_df, 
            family='binomial')

AIC(mod1,mod)
coefs   <- coef(mod)
coefs   <- coef(mod1)

x_seq   <- seq(min(surv_df$log_size_t0,na.rm=T),
               max(surv_df$log_size_t0,na.rm=T), length.out = 100 )

y_fru0  <- coefs[1] + x_seq * coefs[2]
y_fru1  <- coefs[1] + x_seq * coefs[2] + coefs[3] + x_seq * coefs[4] 


tiff('results/ml_mod_sel/surv_vs_fruiting_t0.tiff',
     unit="in", width=6.3, height=6.3, res=400, compression="lzw")

par( mfrow=c(1,1), 
     mar = c(3.5,3.5,0.2,0.2), mgp = c(2.1,0.7,0), 
     cex.lab = 1.2 )

plot(jitter(surv_t1) ~ log_size_t0, data=surv_df,
     ylab = 'survival rate', col = col )
lines(x_seq, boot::inv.logit(y_fru0), lwd=2, col = 'black')
lines(x_seq, boot::inv.logit(y_fru1), lwd=2, col = 'red')

legend(1.8,.6,
     c('did not flower','flowered'), lwd = 2,
     col = c('black','red'), bty = 'n', cex = 1.2)

dev.off()


  
# test importance of fruiting TWO YEARS IN A ROW ----------------------------------

# create data frame with fruiting two years in advance (tm1)
fru_tm1 <- surv_ %>% 
              select(Site, New_Tag, Habitat_Man, year_t1, n_fruit_t1, flower_t1) %>% 
              rename( n_fruit_tm1 = n_fruit_t1,
                      flower_tm1  = flower_t1 ) %>% 
              mutate( year_t1 = year_t1 + 2 )

# data frame for analyses
surv_df <- left_join( surv_, fru_t0 ) %>% 
              left_join( fru_tm1 ) %>% 
              mutate( fruit_x2   = n_fruit_t0 + n_fruit_tm1 ) %>% 
              mutate( fruit_x2   = replace(fruit_x2, fruit_x2 == 1, 0) ) %>% 
              mutate( fruit_x2   = as.factor(fruit_x2) ) %>% 
              mutate( col = 'black' ) %>% 
              mutate( col = replace(col, fruit_x2  == '2', 'red') )

# glm regression 
mod  <- glm(surv_t1 ~ log_size_t0 + fruit_x2,
            data = surv_df, 
            family='binomial')
mod1 <- glm(surv_t1 ~ log_size_t0 * fruit_x2,
            data = surv_df, 
            family='binomial')

AIC(mod1,mod)
coefs   <- coef(mod1)

x_seq   <- seq(min(surv_df$log_size_t0,na.rm=T),
               max(surv_df$log_size_t0,na.rm=T), length.out = 100 )

y_fru0  <- coefs[1] + x_seq * coefs[2]
y_fru1  <- coefs[1] + x_seq * coefs[2] + coefs[3] + x_seq * coefs[4] 


tiff('results/ml_mod_sel/surv_vs_fruiting_tm1.tiff',
     unit="in", width=6.3, height=6.3, res=400, compression="lzw")

par( mfrow=c(1,1), 
     mar = c(3.5,3.5,0.2,0.2), mgp = c(2.1,0.7,0), 
     cex.lab = 1.2 )

plot(jitter(surv_t1) ~ log_size_t0, data=surv_df,
     ylab = 'survival rate', col = col )
lines(x_seq, boot::inv.logit(y_fru0), lwd=2, col = 'black')
lines(x_seq, boot::inv.logit(y_fru1), lwd=2, col = 'red')

legend(1.8,.6,
     c('did not flower','flowered'), lwd = 2,
     col = c('black','red'), bty = 'n', cex = 1.2)

dev.off()
