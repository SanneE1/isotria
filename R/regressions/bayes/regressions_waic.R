rm(list=ls())
setwd("C:/cloud/MEGA/Projects/SIDE/istoria")
library(tidyverse)
library(testthat)
library(rstan)
library(loo)
options(stringsAsFactors = F)

# set rstan options to parallel cores
rstan_options( auto_write = TRUE )
options( mc.cores = parallel::detectCores() )

# read data
istoria <- read.csv('data/istoria_long.csv')
snow_an <- read.csv('data/snow_depth_m_anomalies.csv') %>% 
              subset( year != 1986) %>%
              .[,-1] %>% 
              as.matrix

# survival analysis
surv_df <- istoria %>% 
              subset( !is.na(Size_t0) & !is.na(surv) ) %>%
              subset( Size_t0 != 0 )

# organize data into list to pass to stan
dat_stan <- list(
  n       = nrow(surv_df),
  n_time  = nrow(snow_an),
  n_year  = 2,
  n_lag   = ncol(snow_an),
  y       = surv_df$surv,
  size_t0 = log(surv_df$Size_t0),
  clim    = t(snow_an),
  year_i  = as.factor(surv_df$year) %>% as.numeric
)

# simulation parameters
sim_pars <- list(
  warmup = 1000, 
  iter   = 4000, 
  thin   = 2, 
  chains = 3
)

# gaussian moving window
fit_simpl <- stan(
  file = paste0("analysis/stan/bernoulli_dirichlet.stan"),
  data = dat_stan,
  pars = c('alpha','beta_c','theta','log_lik'),
  warmup = sim_pars$warmup,
  iter = sim_pars$iter,
  thin = sim_pars$thin,
  chains = sim_pars$chains#,
  #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
)

# gaussian moving window
fit_simpl_size <- stan(
  file = paste0("analysis/stan/bernoulli_dirichlet_size.stan"),
  data = dat_stan,
  pars = c('alpha','beta_c','beta_s','theta','log_lik'),
  warmup = sim_pars$warmup,
  iter = sim_pars$iter,
  thin = sim_pars$thin,
  chains = sim_pars$chains#,
  #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
)

# gaussian moving window
fit_size <- stan(
  file = paste0("analysis/stan/bernoulli_size.stan"),
  data = dat_stan,
  pars = c('alpha','beta_s','log_lik'),
  warmup = sim_pars$warmup,
  iter = sim_pars$iter,
  thin = sim_pars$thin,
  chains = sim_pars$chains#,
  #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 20)
)


# do model selection
mod_fit   <- list(simpl      = fit_simpl,     
                  simpl_size = fit_simpl_size,
                  size       = fit_size )


# leave-one-out estimates
loo_l      <- lapply(log_liks, loo) %>%
                setNames( c("loo_simpl",   "loo_simpl_size", 
                            "loo_size") )
loo_df     <- loo::compare(loo_l$loo_simpl,   loo_l$loo_simpl_size, 
                           loo_l$loo_size ) %>%
                as.data.frame %>%
                tibble::add_column(model = gsub("loo_","",names(loo_l) ), .before = 1)


# wAIC model selection using loo approximation (from library 'loo')
log_liks   <- lapply(mod_fit, extract_log_lik)

# WAIC estimates
waic_l    <- lapply(log_liks, waic) %>%
                setNames(c("waic_simpl",   
                           "waic_simpl_size", 
                           "waic_size") )
waic_df   <- loo::compare(waic_l$waic_simpl,   
                          waic_l$waic_simpl_size, 
                          waic_l$waic_size) %>%
                as.data.frame %>%
                tibble::add_column(model = gsub("waic_","",names(waic_l) ), .before = 1)
