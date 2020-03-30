
library(dplyr)
library(lme4)
library(ggplot2)
library(patchwork)

# read data --------------------------------------------------------------------------------------------------

source(textConnection(readLines("Sophie/my IPM.R")[c(1:136)]))

# create logit binned function --------------------------------------------------------------------------------------------------

logitbin_df <- function (df, resp, xvar, ..., n =100, log_trans_xvar = FALSE) {
  
  resp <- enquo(resp)
  xvar <- enquo(xvar)
  
  if (log_trans_xvar == TRUE) {
    
    if (length(filter(df, !! xvar <= 0)[,1] >= 1 )) {
      warning("Transformed data contains values of =< 0")
    }
    
    df <- df %>%
      dplyr::filter(!! xvar > 0) %>%
      dplyr::mutate(!! xvar := log(!! xvar))
    
  }
  
  df <- df %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(bingroup = cut(!! xvar, breaks = n))
  
  df <- df %>%
    dplyr::group_by(..., bingroup) %>%
    dplyr::summarize(!! resp := mean(!! resp, na.rm = T), !! xvar := mean(!! xvar, na.rm = T))
  
  return(df)
}


### Create inverse logit function--------------------------------------------------------------------------------------------------

inv_logit<-function(x){
  exp(x)/(1+exp(x))}


# Set up dataframe for predictions --------------------------------------------------------------------------------------------------
x_seq <- seq(min(iso$size_t0, na.rm=T), 
             max(iso$size_t0, na.rm=T), by = 0.1)
site <- c(1:4)
pred_df <- expand.grid(size_t0 = x_seq, Site = site) %>%
  mutate(Site = as.factor(Site))


# create prediction values --------------------------------------------------------------------------------------------------
#sr
sr_y_pred<- cbind(pred_df, 
                  pred = predict(sr_mod, newdata = pred_df, type = "response", re.form = NA))

#gr
gr_y_pred<-cbind(pred_df,
                 pred = predict(gr_mod, newdata = pred_df, type = "response", re.form = NA)
)

#flowpop_mod
flp_y_pred<-cbind(pred_df,
                  pred = predict(flowpop_mod, newdata = pred_df, type = "response", re.form = NA)
)

#flower_n_mod
fln_y_pred<-cbind(pred_df,
                  pred =predict(flower_n_mod, newdata = pred_df, type = "response", re.form = NA) 
)

#dorm_mod
do_y_pred<-cbind(pred_df,
                 pred = predict(dorm_mod, newdata = pred_df, type = "response", re.form = NA)
)


### Plot predictions

Surv <- ggplot() +
  geom_point(data = logitbin_df(df = iso, resp = surv_t1, xvar = size_t0, n = 10, Site), 
             aes(x = size_t0, y = surv_t1, colour = Site)) + 
  geom_line(data = sr_y_pred, 
            aes(x = size_t0, y = pred, colour = Site))

Growth <- ggplot() +
  geom_point(data = iso, 
             aes(x = size_t0, y = size_t1)) + 
  geom_line(data = gr_y_pred, 
            aes(x = size_t0, y = pred), 
            colour = "red")

Flwr_p <- ggplot() +
  geom_point(data = logitbin_df(df = iso, resp = flower_t1, xvar = size_t0, n = 10, Site), 
             aes(x = size_t0, y = flower_t1, colour = Site)) + 
  geom_line(data = flp_y_pred %>% mutate(Site = as.factor(Site)), 
            aes(x = size_t0, y = pred, colour = Site))

Flwr_n <- ggplot() +
  geom_point(data = iso, 
             aes(x = size_t0, y = n_flower_t1, colour = Site)) + 
  geom_line(data = fln_y_pred %>% mutate(Site = as.factor(Site)), 
            aes(x = size_t0, y = pred, colour = Site))

Dorm <- ggplot() +
  geom_point(data = logitbin_df(df = iso, resp = dormancy_t1, xvar = size_t0, n = 10, Site), 
             aes(x = size_t0, y = dormancy_t1)) + 
  geom_line(data = do_y_pred, 
            aes(x = size_t0, y = pred),
            colour = 'red')

(Surv + Growth) / (Flwr_p + Flwr_n) / (Dorm + plot_spacer()) + plot_layout(guides = "collect")
