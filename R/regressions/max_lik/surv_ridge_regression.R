rm(list=ls())
setwd("C:/cloud/Dropbox/isotria_idiv")
library(dplyr)
library(tidyr)
library(testthat)
library(lme4)
library(bbmle)
library(glmnet)
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

# glm regression 
glm_mod <- glm(surv_t1 ~ log_size_t0 + ppt_t0 + snw_j0,
               data = surv_df, family='binomial')

# data for ridge regression
rr_df <- surv_df %>% 
            select(surv_t1,log_size_t0,ppt_t0,snw_j0) %>% 
            subset( !is.na(log_size_t0) ) %>% 
            subset( !is.na(surv_t1) )
y     <- rr_df$surv_t1
x     <- rr_df %>% 
            select(log_size_t0,ppt_t0,snw_j0) %>% 
            data.matrix()
lams  <- 10^seq(3, -2, by = -.1)

# fit the model and get optimal lambdas
rid_mod  <- glmnet(x, y, alpha = 0, 
                   lambda=lams, family = 'binomial')
cv_fit  <- cv.glmnet(x, y, alpha = 0)
opt_lam <- cv_fit$lambda.min

#find the best lambda from our list via cross-validation
predict(rid_mod, s = opt_lam, type = 'coefficients')

# best ridge coefficients
coeff_df <- data.frame( ols = coef(glm_mod),
                        ridge = predict(rid_mod, 
                                        s = bestlam, 
                                        type = 'coefficients')[1:4,] )

write.csv(coeff_df, 'results/ml_mod_sel/surv/ols_vs_ridge.csv',
          row.names=F)


# ridge example from https://www.r-bloggers.com/ridge-regression-and-the-lasso/ 
swiss   <- datasets::swiss
x       <- model.matrix(Fertility~., swiss)[,-1]
y       <- swiss$Fertility
lambda  <- 10^seq(10, -2, length = 100)

set.seed(489)
train  <- sample(1:nrow(x), nrow(x)/2)
test   <- (-train)
ytest  <- y[test]

# fit OLS normal 
swisslm <- lm(Fertility~., data = swiss)
coef(swisslm)

# fit ridge
ridge.mod <- glmnet(x, y, alpha = 0, lambda = lambda)
predict(ridge.mod, s = 0, 
        type = 'coefficients')[1:6,]

swisslm   <- lm(Fertility~., data = swiss, subset = train)
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = lambda)

#find the best lambda from our list via cross-validation
cv.out    <- cv.glmnet(x[train,], y[train], alpha = 0)
bestlam   <- cv.out$lambda.min

# best ridge coefficients
data.frame( ols = coef(swisslm),
            ridge = predict(ridge.mod, s = bestlam, type = 'coefficients')[1:6,]
)

	