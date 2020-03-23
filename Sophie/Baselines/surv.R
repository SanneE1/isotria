library(dplyr)
 library(tidyr)
 library(testthat)
 library(lme4)
 library(bbmle)
 library(ggplot2)


   ## Read and format data
   d <- read.csv('data/isotria_long.csv')
surv <- d[complete.cases(d$size_t0, d$surv_t1, d$Habitat_Man),] %>%
     subset(size_t0 != 0)  %>%
     mutate(size_t0 = log(size_t0),
            size_t1 = log(size_t1))
 
   ## model options
   
   candidate_mods <- list(
       "null" = surv_t1 ~ size_t0 + (1 | year_t1),
       "site1" = surv_t1 ~ size_t0 * Site + (1 | year_t1),
       "site2" = surv_t1 ~ size_t0 + Site + (1 | year_t1),
       "yearsize" = surv_t1 ~ size_t0 + (size_t0 | year_t1),
       "yearsize2" = surv_t1 ~ size_t0 + (0 + size_t0 | year_t1)
     )

   
surv_m    <- lapply( candidate_mods, 
                                    function(x) glmer(x, data=surv, family='binomial') )

   compare <- as.data.frame(AICtab(surv_m, weights = TRUE, base = TRUE))
 
 ## Overview of best model
   summary(surv_m[[row.names(compare)[1]]])
plot(surv_m[[row.names(compare)[1]]])


## exploratory plots
#source("../../52 Scrap code/plot_binned_prop_df.R")
source("/Users/sophi/Documents/idivproject/isotria/plot_binned_prop_df.R")

df_bin <- logitbin_df(surv, resp = surv_t1, xvar = size_t0, year_t1, Site)
df_bin <- df_bin %>%
  mutate(site = as.factor(Site))

ggplot(df_bin, aes(x = size_t0, y = surv_t1)) + geom_point(aes(colour = site)) + facet_wrap(vars(year_t1))
