rm(list=ls())
setwd("C:/cloud/Dropbox/isotria_idiv")
library(dplyr)
library(tidyr)
library(testthat)
library(rstan)
library(loo)
library(lme4)
options(stringsAsFactors = F)
source('C:/cloud/Dropbox/isotria_idiv/analysis/climate/make_anomalies.R')

# set rstan options to parallel cores
rstan_options( auto_write = TRUE )
options( mc.cores = parallel::detectCores() )

# read data
istoria   <- read.csv('data/istoria_long.csv')
snow_an   <- read.csv('data/snow_depth_m_anomalies.csv') %>%
                subset( year != 1986) %>%
                subset( year < 1998 ) %>% 
                .[,-c(1,7:11)] %>% 
                as.matrix
prism_df  <- read.csv('C:/cloud/Dropbox/isotria_idiv/data/climate/prism_isotria.csv') %>% 
                group_by(variable,year,month) %>% 
                summarise( value = mean(value) ) %>% 
                ungroup

# survival analysis
surv_df   <- istoria %>% 
                subset( !is.na(size_t0) & !is.na(surv_t1) ) %>%
                subset( size_t0 != 0 ) %>% 
                subset( year_t1 < 1998 ) %>% 
                mutate( log_size_t0 = log(size_t0) )

# format climate
years     <- surv_df$year_t1 %>% unique
m_obs     <- 6
m_back    <- 12 
ppt_anom  <- prism_df %>% 
                subset(variable == 'ppt' ) %>% 
                prism_clim_form(years, m_back, m_obs) %>% 
                dplyr::select(-year) %>% 
                as.matrix
tmp_anom  <- prism_df %>% 
                subset(variable == 'tmean' ) %>% 
                prism_clim_form(years, m_back, m_obs) %>% 
                select(-year) %>% 
                as.matrix

  
# organize data into list to pass to stan
dat_stan <- list(
  n       = nrow(surv_df),
  n_time  = nrow(snow_an),
  n_year  = as.factor(surv_df$year_t1) %>% unique %>% length,
  n_lag   = ncol(snow_an),
  y       = surv_df$surv,
  size_t0 = log(surv_df$size_t0),
  clim    = t(snow_an),
  mean_c  = snow_an[,'V3'],
  year_i  = as.factor(surv_df$year_t1) %>% as.numeric
)

# simulation parameters
sim_pars <- list(
  warmup = 1000, 
  iter   = 4000, 
  thin   = 2, 
  chains = 4
)

# snow depth SAM model
fit_sd <- stan(
  file   = paste0("analysis/stan/bernoulli_SAM_re.stan"),
  data   = dat_stan,
  pars   = c('b0','b_s','b_c','b_yr','s_yr','theta', 'log_lik'),
  warmup = sim_pars$warmup,
  iter   = sim_pars$iter,
  thin   = sim_pars$thin,
  chains = sim_pars$chains#,
  #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
)

# january snow model
fit_s_jan <- stan(
  file   = paste0("analysis/stan/bernoulli_size_meanc_re.stan"),
  data   = dat_stan,
  pars   = c('b0','b_s','b_c','b_yr','s_yr','log_lik'),
  warmup = sim_pars$warmup,
  iter   = sim_pars$iter,
  thin   = sim_pars$thin,
  chains = sim_pars$chains#,
  #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
)


# graph out january snow depth anomaly results
tiff('results/demography/surv/surv_snow.tiff',
     unit="in", width=6.3, height=6.3, res=600,compression="lzw")


U <- max(surv_df$log_size_t0) 
L <- min(surv_df$log_size_t0)

n   <- 15
h   <- (U-L)/n                   # Bin size
lb  <- L+c(0:n)*h                # Lower boundaries of bins 
up  <- (L+h)+c(0:n)*h            # Upper boundaries of bins 
y   <- 0.5*(lb[1:n]+lb[2:(n+1)])   # Bins' midpoints

(y - h*0.5)[1]

# bin prob of survival
bin_prop <- function(midpoint,h){
  
  lower  <- midpoint - h*0.5
  upper  <- midpoint + h*0.5
  
  sub_df <- surv_df %>% 
              mutate( log_size_t0 = log(size_t0) ) %>% 
              subset( log_size_t0 >= lower & log_size_t0 < upper)  
  
  data.frame( log_size_t0 = midpoint,
              surv_t1     = sum(sub_df$surv_t1)/nrow(sub_df),
              stringsAsFactors = T)

}

# extend upper bound to include observed maximum extreme
y[length(y)] = y[length(y)] + 0.1

# create dataframe with bins
surv_bin_df <- lapply(y, bin_prop, h) %>% 
                  Reduce(function(...) bind_rows(...), .)

b0  <- summary(fit_s_jan)$summary[,'mean']['b0']
b_s <- summary(fit_s_jan)$summary[,'mean']['b_s']
b_c <- summary(fit_s_jan)$summary[,'mean']['b_c']

# xlimit
x_seq     <- seq(L,U,0.1)
low_sd    <- boot::inv.logit( b0 + b_s * x_seq + 
                              b_c * quantile(snow_an[,'V3'])[2] )
high_sd   <- boot::inv.logit( b0 + b_s * x_seq + 
                              b_c * quantile(snow_an[,'V3'])[4])

par(mfrow=c(1,1), mar = c(4,4,0.3,0.3), mgp = c(2.4,0.9,0) )
plot(surv_bin_df$log_size_t0, surv_bin_df$surv_t1,
     ylab = 'Survival', xlab = "log(size)", ylim = c(0,1),
     pch = 16, cex.lab = 2, cex.axis = 1.5)
lines(x_seq, low_sd, col = 'red', lwd = 3)
lines(x_seq, high_sd, col = 'blue', lwd = 3)
legend('right', c('low snow', 'high snow'),
       lwd=3, col=c('red','blue') , bty = 'n',
       cex = 2)     

dev.off()   


# annual snow depth anomaly
dat_stan$mean_c <- snow_an[,paste0('V',1:5)] %>% rowMeans
fit_a_sd <- stan(
  file   = paste0("analysis/stan/bernoulli_size_meanc_re.stan"),
  data   = dat_stan,
  pars   = c('b0','b_s','b_c','b_yr','s_yr','log_lik'),
  warmup = sim_pars$warmup,
  iter   = sim_pars$iter,
  thin   = sim_pars$thin,
  chains = sim_pars$chains#,
  #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
)


# precipitation 12 months ------------------------------------------------------
dat_stan <- list(
  n       = nrow(surv_df),
  n_time  = nrow(ppt_anom),
  n_year  = as.factor(surv_df$year_t1) %>% unique %>% length,
  n_lag   = ncol(ppt_anom),
  y       = surv_df$surv_t1,
  size_t0 = log(surv_df$size_t0),
  clim    = t(ppt_anom),
  mean_c  = rowMeans(ppt_anom),
  year_i  = as.factor(surv_df$year_t1) %>% as.numeric
)

# snow depth SAM model
fit_ppt <- stan(
  file   = paste0("analysis/stan/bernoulli_SAM_re.stan"),
  data   = dat_stan,
  pars   = c('b0','b_s','b_c','b_yr','s_yr','theta', 'log_lik'),
  warmup = sim_pars$warmup,
  iter   = sim_pars$iter,
  thin   = sim_pars$thin,
  chains = sim_pars$chains#,
  #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
)

# annual ppt
fit_a_ppt <- stan(
  file   = paste0("analysis/stan/bernoulli_size_meanc_re.stan"),
  data   = dat_stan,
  pars   = c('b0','b_s','b_c','b_yr','s_yr','log_lik'),
  warmup = sim_pars$warmup,
  iter   = sim_pars$iter,
  thin   = sim_pars$thin,
  chains = sim_pars$chains#,
  #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
)



# graph out january snow depth anomaly results
tiff('results/demography/surv/surv_a_ppt.tiff',
     unit="in", width=6.3, height=6.3, res=600,compression="lzw")

U <- max(surv_df$log_size_t0) 
L <- min(surv_df$log_size_t0)

n   <- 15
h   <- (U-L)/n                   # Bin size
lb  <- L+c(0:n)*h                # Lower boundaries of bins 
up  <- (L+h)+c(0:n)*h            # Upper boundaries of bins 
y   <- 0.5*(lb[1:n]+lb[2:(n+1)])   # Bins' midpoints

(y - h*0.5)[1]

# bin prob of survival
bin_prop <- function(midpoint,h){
  
  lower  <- midpoint - h*0.5
  upper  <- midpoint + h*0.5
  
  sub_df <- surv_df %>% 
              mutate( log_size_t0 = log(size_t0) ) %>% 
              subset( log_size_t0 >= lower & log_size_t0 < upper)  
  
  data.frame( log_size_t0 = midpoint,
              surv_t1     = sum(sub_df$surv_t1)/nrow(sub_df),
              stringsAsFactors = T)

}

# extend upper bound to include observed maximum extreme
y[length(y)] = y[length(y)] + 0.1

# create dataframe with bins
surv_bin_df <- lapply(y, bin_prop, h) %>% 
                  Reduce(function(...) bind_rows(...), .)

b0  <- summary(fit_a_ppt)$summary[,'mean']['b0']
b_s <- summary(fit_a_ppt)$summary[,'mean']['b_s']
b_c <- summary(fit_a_ppt)$summary[,'mean']['b_c']

# xlimit
x_seq     <- seq(L,U,0.1)
low_sd    <- boot::inv.logit( b0 + b_s * x_seq + 
                              b_c * quantile(rowMeans(ppt_anom))[2] )
high_sd   <- boot::inv.logit( b0 + b_s * x_seq + 
                              b_c * quantile(rowMeans(ppt_anom))[4])

par(mfrow=c(1,1), mar = c(4,4,0.3,0.3), mgp = c(2.4,0.9,0) )
plot(surv_bin_df$log_size_t0, surv_bin_df$surv_t1,
     ylab = 'Survival', xlab = "log(size)", ylim = c(0,1),
     pch = 16, cex.lab = 2, cex.axis = 1.5)
lines(x_seq, low_sd, col = 'red', lwd = 3)
lines(x_seq, high_sd, col = 'blue', lwd = 3)
legend('right', c('low precip', 'high precip'),
       lwd=3, col=c('red','blue') , bty = 'n',
       cex = 2)     

dev.off()   








# temperature 12 months ------------------------------------------------------
dat_stan <- list(
  n       = nrow(surv_df),
  n_time  = nrow(tmp_anom),
  n_year  = as.factor(surv_df$year_t1) %>% unique %>% length,
  n_lag   = ncol(tmp_anom),
  y       = surv_df$surv_t1,
  size_t0 = log(surv_df$size_t0),
  clim    = t(tmp_anom),
  mean_c  = rowMeans(tmp_anom),
  year_i  = as.factor(surv_df$year_t1) %>% as.numeric
)

# snow depth SAM model
fit_tmp <- stan(
  file   = paste0("analysis/stan/bernoulli_SAM_re.stan"),
  data   = dat_stan,
  pars   = c('b0','b_s','b_c','b_yr','s_yr','theta', 'log_lik'),
  warmup = sim_pars$warmup,
  iter   = sim_pars$iter,
  thin   = sim_pars$thin,
  chains = sim_pars$chains#,
  #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
)

# annual ppt
fit_a_tmp <- stan(
  file   = paste0("analysis/stan/bernoulli_size_meanc_re.stan"),
  data   = dat_stan,
  pars   = c('b0','b_s','b_c','b_yr','s_yr','log_lik'),
  warmup = sim_pars$warmup,
  iter   = sim_pars$iter,
  thin   = sim_pars$thin,
  chains = sim_pars$chains#,
  #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
)

# NULL model
fit_null <- stan(
  file   = paste0("analysis/stan/bernoulli_size_re.stan"),
  data   = dat_stan,
  pars   = c('b0','b_s','b_yr','s_yr', 'log_lik'),
  warmup = sim_pars$warmup,
  iter   = sim_pars$iter,
  thin   = sim_pars$thin,
  chains = sim_pars$chains#,
  #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
)

# write this out -----------------------------------------------------------

# out_sd    <- rstan::extract(fit_sd)   %>% as.data.frame
# out_ppt   <- rstan::extract(fit_ppt)  %>% as.data.frame
# out_null  <- rstan::extract(fit_null) %>% as.data.frame
# write.csv(out_sd,   'results/demography/sd_surv_36.csv',   row.names=F)
# write.csv(out_ppt,  'results/demography/ppt_surv_36.csv',  row.names=F)
# write.csv(out_null, 'results/demography/null_surv_36.csv', row.names=F)
# 
# list(out_sd, out_ppt, out_null) %>% ls


# plots -------------------------------------------------------------

# snow depth
post  <- fit_sd %>% as.data.frame

tiff('results/demography/snow_depth_weights.tiff',
     unit="in", width=4.5, height=4.5, res=600,compression="lzw")

par(mfrow=c(1,1),mar=c(3,3,1,0.2), mgp=c(2,0.8,0) )
boxplot(post[,paste0('theta[',1:5,"]")],
        main  = 'Contribution of month',
        names = month.abb[c(11:12,1:3)],
        outline = F,
        ylab = 'Prop. contrib. of season')

dev.off()

# precipitation weight
post  <- fit_ppt %>% as.data.frame

tiff('results/demography/ppt.tiff',
     unit="in", width=4.5, height=4.5, res=600,compression="lzw")

par(mfrow=c(1,1),mar=c(3,3,1,0.2), mgp=c(2,0.8,0) )
boxplot(post[,paste0('theta[',1:12,"]")],
        main  = 'Contribution of month',
        names = month.abb[c(6:1,12:7)],
        outline = F,
        ylab = 'Prop. contrib. of season')

dev.off()

# precipitation weight
post  <- fit_tmp %>% as.data.frame

tiff('results/demography/airt.tiff',
     unit="in", width=4.5, height=4.5, res=600,compression="lzw")

par(mfrow=c(1,1),mar=c(3,3,1,0.2), mgp=c(2,0.8,0) )
boxplot(post[,paste0('theta[',1:12,"]")],
        main  = 'Contribution of month',
        names = month.abb[c(6:1,12:7)],
        outline = F,
        ylab = 'Prop. contrib. of season')

dev.off()


# do model selection -------------------------------------------------------
mod_fit   <- list(sd    = fit_sd,     
                  ppt   = fit_ppt,
                  tmp   = fit_tmp,
                  null  = fit_null )

mod_fit   <- list(sd_j  = fit_s_jan,  
                  sd_a  = fit_a_sd,  
                  ppt_a = fit_a_ppt,
                  tmp_a = fit_a_tmp,
                  null  = fit_null,
                  sd    = fit_sd,     
                  ppt   = fit_ppt,
                  tmp   = fit_tmp )

# wAIC model selection using loo approximation 
# extract log likelihoods
waic_df   <- lapply(mod_fit, extract_log_lik) %>% 
                # calculate WAIC
                lapply( waic ) %>% 
                setNames( paste0('waic_',names(mod_fit)) ) %>%
                # compare models with WAIC
                loo::compare(x = .) %>% 
                as.data.frame %>% 
                tibble::add_column(model = names(mod_fit), .before = 1)

# save.image( 'results/models/prelim_mod_sel.Rdata') 


# # leave-one-out estimates
# loo_l      <- lapply(mod_fit, loo) %>%
#                 setNames( c("loo_sd",   "loo_ppt", 
#                             "loo_null") )
# loo_df     <- loo::compare(loo_l$loo_sd,   
#                            loo_l$loo_ppt, 
#                            loo_l$loo_null ) %>%
#                 as.data.frame %>%
#                 tibble::add_column(model = gsub("loo_","",names(loo_l) ), .before = 1)
