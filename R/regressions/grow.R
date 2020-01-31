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
                .[,-c(1,7:11)] %>% 
                as.matrix
prism_df  <- read.csv('C:/cloud/Dropbox/isotria_idiv/data/climate/prism_isotria.csv') %>% 
                group_by(variable,year,month) %>% 
                summarise( value = mean(value) ) %>% 
                ungroup

# growth analysis
grow_df   <- istoria %>% 
                subset( !is.na(size_t0) & !is.na(surv_t1) ) %>%
                mutate( log_size_t0 = log(size_t0),
                        log_size_t1 = log(size_t1) ) %>% 
                subset( !(log_size_t0 %in% -Inf ) ) %>% 
                subset( !(log_size_t1 %in% -Inf ) ) %>% 
                subset( !( is.na(log_size_t1) ) )

# format climate
years     <- grow_df$year_t1 %>% unique
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
  n       = nrow(grow_df),
  n_time  = nrow(snow_an),
  n_year  = as.factor(grow_df$year_t1) %>% unique %>% length,
  n_lag   = ncol(snow_an),
  y       = grow_df$log_size_t1,
  size_t0 = grow_df$log_size_t0,
  clim    = t(snow_an),
  mean_c  = snow_an[,'V3'],
  year_i  = as.factor(grow_df$year_t1) %>% as.numeric
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
  file   = paste0("analysis/stan/gaussian_SAM_re.stan"),
  data   = dat_stan,
  pars   = c('b0','b_s','b_c','b_yr','s_yr','theta', 's', 'log_lik'),
  warmup = sim_pars$warmup,
  iter   = sim_pars$iter,
  thin   = sim_pars$thin,
  chains = sim_pars$chains#,
  #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
)

# january snow model
fit_s_jan <- stan(
  file   = paste0("analysis/stan/gaussian_size_meanc_re.stan"),
  data   = dat_stan,
  pars   = c('b0','b_s','b_c','b_yr','s_yr','s','log_lik'),
  warmup = sim_pars$warmup,
  iter   = sim_pars$iter,
  thin   = sim_pars$thin,
  chains = sim_pars$chains#,
  #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
)

# annual snow depth anomaly
dat_stan$mean_c <- snow_an[,paste0('V',1:5)] %>% rowMeans
fit_a_sd <- stan(
  file   = paste0("analysis/stan/gaussian_size_meanc_re.stan"),
  data   = dat_stan,
  pars   = c('b0','b_s','b_c','b_yr','s_yr','s','log_lik'),
  warmup = sim_pars$warmup,
  iter   = sim_pars$iter,
  thin   = sim_pars$thin,
  chains = sim_pars$chains#,
  #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
)

# precipitation 12 months ------------------------------------------------------
dat_stan <- list(
  n       = nrow(grow_df),
  n_time  = nrow(ppt_anom),
  n_year  = as.factor(grow_df$year_t1) %>% unique %>% length,
  n_lag   = ncol(ppt_anom),
  y       = grow_df$log_size_t1,
  size_t0 = log(grow_df$size_t0),
  clim    = t(ppt_anom),
  mean_c  = rowMeans(ppt_anom),
  year_i  = as.factor(grow_df$year_t1) %>% as.numeric
)

# ppt depth SAM model
fit_ppt <- stan(
  file   = paste0("analysis/stan/gaussian_SAM_re.stan"),
  data   = dat_stan,
  pars   = c('b0','b_s','b_c','b_yr','s_yr','theta', 's', 'log_lik'),
  warmup = sim_pars$warmup,
  iter   = sim_pars$iter,
  thin   = sim_pars$thin,
  chains = sim_pars$chains#,
  #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
)

fit_a_ppt <- stan(
  file   = paste0("analysis/stan/gaussian_size_meanc_re.stan"),
  data   = dat_stan,
  pars   = c('b0','b_s','b_c','b_yr','s_yr','s','log_lik'),
  warmup = sim_pars$warmup,
  iter   = sim_pars$iter,
  thin   = sim_pars$thin,
  chains = sim_pars$chains#,
  #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
)

# temperature 12 months ------------------------------------------------------
dat_stan <- list(
  n       = nrow(grow_df),
  n_time  = nrow(tmp_anom),
  n_year  = as.factor(grow_df$year_t1) %>% unique %>% length,
  n_lag   = ncol(tmp_anom),
  y       = grow_df$log_size_t1,
  size_t0 = log(grow_df$size_t0),
  clim    = t(tmp_anom),
  mean_c  = rowMeans(tmp_anom),
  year_i  = as.factor(grow_df$year_t1) %>% as.numeric
)

# snow depth SAM model
fit_tmp <- stan(
  file   = paste0("analysis/stan/gaussian_SAM_re.stan"),
  data   = dat_stan,
  pars   = c('b0','b_s','b_c','b_yr','s_yr','theta', 's', 'log_lik'),
  warmup = sim_pars$warmup,
  iter   = sim_pars$iter,
  thin   = sim_pars$thin,
  chains = sim_pars$chains#,
  #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
)

fit_a_tmp <- stan(
  file   = paste0("analysis/stan/gaussian_size_meanc_re.stan"),
  data   = dat_stan,
  pars   = c('b0','b_s','b_c','b_yr','s_yr','s','log_lik'),
  warmup = sim_pars$warmup,
  iter   = sim_pars$iter,
  thin   = sim_pars$thin,
  chains = sim_pars$chains#,
  #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
)

# NULL model
fit_null <- stan(
  file   = paste0("analysis/stan/gaussian_size_re.stan"),
  data   = dat_stan,
  pars   = c('b0','b_s','b_yr','s_yr', 's', 'log_lik'),
  warmup = sim_pars$warmup,
  iter   = sim_pars$iter,
  thin   = sim_pars$thin,
  chains = sim_pars$chains,
  control = list(adapt_delta = 0.99, stepsize = 0.001, max_treedepth = 15)
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

tiff('results/demography/grow/snow_depth_weights.tiff',
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

tiff('results/demography/grow/ppt.tiff',
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

tiff('results/demography/grow/airt.tiff',
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
                tibble::add_column(model = rownames(.), .before = 1)

save.image( 'results/models/grow_mod_sel.Rdata')


# # leave-one-out estimates
# loo_l      <- lapply(mod_fit, loo) %>%
#                 setNames( c("loo_sd",   "loo_ppt", 
#                             "loo_null") )
# loo_df     <- loo::compare(loo_l$loo_sd,   
#                            loo_l$loo_ppt, 
#                            loo_l$loo_null ) %>%
#                 as.data.frame %>%
#                 tibble::add_column(model = gsub("loo_","",names(loo_l) ), .before = 1)
