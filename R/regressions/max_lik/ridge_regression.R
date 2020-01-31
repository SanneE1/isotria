best_mod <- glmer(surv_t1 ~ log_size_t0 + (1 | year_t1) + (1 | New_Tag) ,
                  data = surv_df, family='binomial')

best_mod <- glm(surv_t1 ~ log_size_t0 + snw_j0,
                data = surv_df, family='binomial')


y <- mtcars$hp
x <- mtcars %>% select(mpg, wt, drat) %>% data.matrix()
lambdas <- 10^seq(3, -2, by = -.1)

fit <- glmnet(x, y, alpha = 0, lambda = lambdas)
summary(fit)

best_mod %>% coef
best_mod 

best_mod %>% 
  coef %>% 
  .$New_Tag %>% 
  .$'(Intercept)' %>% 
  hist

