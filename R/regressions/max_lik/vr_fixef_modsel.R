rm(list=ls())
setwd("C:/cloud/MEGA/Projects/SIDE/istoria")
library(dplyr)
library(tidyr)
library(testthat)
library(lme4)
library(bbmle)
options(stringsAsFactors = F)

replace_0 <- function(x) replace(x, x == 0, NA)

# read data
isotria <- read.csv('data/istoria_long.csv') %>% 
              mutate( Size_t0 = replace_0(Size_t0),
                      Size_t1 = replace_0(Size_t1) ) %>% 
              mutate( log_size_t0  = log( Size_t0 ),
                      log_size_t1  = log( Size_t1 ),
                      log_size_t02 = log( Size_t0 )^2,
                      log_size_t12 = log( Size_t1 )^2 )

# survival analysis
surv_df <- isotria %>% 
              subset( !(is.na(log_size_t0) & is.na(surv)) ) %>%
              subset( !(Size_t0 %in% 0) ) %>% 
              subset( year_t1 < 1998 )

flow_df <- subset(isotria, Fruit_t1 != 2)


# survival model selection ------------------------------------

# start models
sur_size  <- glmer(surv ~ log_size_t0 + (1 | year_t1),
                   data=surv_df, family='binomial')
sur_size2 <- glmer(surv ~ log_size_t0 + log_size_t02 + (1 | year_t1),
                   data=surv_df, family='binomial')
sur_site  <- glmer(surv ~ log_size_t0 + Site + (1 | year_t1),
                   data=surv_df, family='binomial')
sur_ss    <- glmer(surv ~ log_size_t0 * Site + (1 | year_t1),
                   data=surv_df, family='binomial')
sur_ss2   <- glmer(surv ~ log_size_t0 * Site + log_size_t02 + (1 | year_t1),
                   data=surv_df, family='binomial')

# size by year interactions
sur_size_i  <- glmer(surv ~ log_size_t0 + (log_size_t0 | year_t1),
                     data=surv_df, family='binomial')
sur_size2_i <- glmer(surv ~ log_size_t0 + log_size_t02 + (log_size_t0 | year_t1),
                     data=surv_df, family='binomial')
sur_site_i  <- glmer(surv ~ log_size_t0 + Site + (log_size_t0 | year_t1),
                     data=surv_df, family='binomial')
sur_ss_i    <- glmer(surv ~ log_size_t0 * Site + (log_size_t0 | year_t1),
                     data=surv_df, family='binomial')
sur_ss2_i   <- glmer(surv ~ log_size_t0 * Site + log_size_t02 + (log_size_t0 | year_t1),
                     data=surv_df, family='binomial')

# mod sel
AICtab(sur_size,sur_size2,sur_site,sur_ss,sur_ss2,
       sur_size_i,sur_size2_i,sur_site_i,sur_ss_i,sur_ss2_i)


# growth
gr_size  <- lmer(log_size_t1 ~ log_size_t0  + (1 | year_t1),
                   data=isotria)
gr_size2 <- lmer(log_size_t1 ~ log_size_t0 + log_size_t02 + (1 | year_t1),
                   data=isotria)
gr_site  <- lmer(log_size_t1 ~ log_size_t0 + Site + (1 | year_t1),
                   data=isotria)
gr_ss    <- lmer(log_size_t1 ~ log_size_t0 * Site + (1 | year_t1),
                   data=isotria)
gr_ss2   <- lmer(log_size_t1 ~ log_size_t0 * Site + log_size_t02 + (1 | year_t1),
                   data=isotria)
  
gr_size_i  <- lmer(log_size_t1 ~ log_size_t0  + (log_size_t0 | year_t1),
                   data=isotria)
gr_size2_i <- lmer(log_size_t1 ~ log_size_t0 + log_size_t02 + (log_size_t0 | year_t1),
                   data=isotria)
gr_site_i  <- lmer(log_size_t1 ~ log_size_t0 + Site + (log_size_t0 | year_t1),
                   data=isotria)
gr_ss_i    <- lmer(log_size_t1 ~ log_size_t0 * Site + (log_size_t0 | year_t1),
                   data=isotria)
gr_ss2_i   <- lmer(log_size_t1 ~ log_size_t0 * Site + log_size_t02 + (log_size_t0 | year_t1),
                   data=isotria)


AICtab(gr_size,gr_size2,gr_site,gr_ss,gr_ss2,
       gr_size_i,gr_size2_i,gr_site_i,gr_ss_i,gr_ss2_i)


# flowering
fl_size  <- glmer(Fruit_t1 ~ log_size_t0 + (1 | year_t1),
                  data=flow_df, family='binomial')
fl_size2 <- glmer(Fruit_t1 ~ log_size_t0 + log_size_t02 + (1 | year_t1),
                   data=flow_df, family='binomial')
fl_site  <- glmer(Fruit_t1 ~ log_size_t0 + Site + (1 | year_t1),
                   data=flow_df, family='binomial')
fl_ss    <- glmer(Fruit_t1 ~ log_size_t0 * Site + (1 | year_t1),
                   data=flow_df, family='binomial')
fl_ss2   <- glmer(Fruit_t1 ~ log_size_t0 * Site + log_size_t02 + (1 | year_t1),
                   data=flow_df, family='binomial')

fl_size_i  <- glmer(Fruit_t1 ~ log_size_t0 + (log_size_t0 | year_t1),
                  data=flow_df, family='binomial')
fl_size2_i <- glmer(Fruit_t1 ~ log_size_t0 + log_size_t02 + (log_size_t0 | year_t1),
                   data=flow_df, family='binomial')
fl_site_i  <- glmer(Fruit_t1 ~ log_size_t0 + Site + (log_size_t0 | year_t1),
                   data=flow_df, family='binomial')
fl_ss_i    <- glmer(Fruit_t1 ~ log_size_t0 * Site + (log_size_t0 | year_t1),
                   data=flow_df, family='binomial')
fl_ss2_i   <- glmer(Fruit_t1 ~ log_size_t0 * Site + log_size_t02 + (log_size_t0 | year_t1),
                   data=flow_df, family='binomial')

AICtab(fl_size,fl_size2,fl_site,fl_ss,fl_ss2,
       fl_size_i,fl_size2_i,fl_site_i,fl_ss_i,fl_ss2_i)
