
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
    dplyr::summarize(!! resp := mean(!! resp), !! xvar := mean(!! xvar))
  
  return(df)
}

# fit models --------------------------------------------------------------------------------------------------



### Create inverse logit function--------------------------------------------------------------------------------------------------

inv_logit<-function(x){
  exp(x)/(1+exp(x))}


# Set up dataframe for predictions --------------------------------------------------------------------------------------------------
x_seq <- seq(min(iso$size_t0, na.rm=T), 
             max(iso$size_t0, na.rm=T), by = 0.1)
site <- c(1:4)
pred_df <- expand.grid(size_t0 = x_seq, Site = site)

# create prediction values --------------------------------------------------------------------------------------------------
#sr
sr_b0<-fixef(sr_mod)[1]
sr_b1<-fixef(sr_mod)[2]
sr_y_pred<- cbind(pred_df, 
                  pred = inv_logit(sr_b0 + sr_b1 * pred_df$size_t0))

#gr
gr_b0<-fixef(gr_mod)[1]
gr_b1<-fixef(gr_mod)[2]
gr_y_pred<-cbind(pred_df,
                 pred = (gr_b0 + gr_b1 * pred_df$size_t0)
)

#flowpop_mod
flp_b0<-fixef(flowpop_mod)[1]
flp_b1<-fixef(flowpop_mod)[2]
flp_b2 <- fixef(flowpop_mod)[3]
flp_b3 <- fixef(flowpop_mod)[4]
flp_y_pred<-cbind(pred_df,
                  pred = inv_logit(flp_b0+ flp_b1 * pred_df$size_t0 + flp_b2 * pred_df$Site + flp_b3 * pred_df$Site * pred_df$size_t0)   
)

#flower_n_mod
fln_b0<-fixef(flower_n_mod)[1]
fln_b1<-fixef(flower_n_mod)[2]
fln_b2 <- fixef(flower_n_mod)[3]
fln_y_pred<-cbind(pred_df,
                  pred = exp(fln_b0+fln_b1* pred_df$size_t0 + fln_b2 * pred_df$Site)  
)

#dorm_mod
do_b0<-fixef(dorm_mod)[1]
do_b1<-fixef(dorm_mod)[2]
do_b2 <- fixef(dorm_mod)[3]
do_b3 <- fixef(dorm_mod)[4]
do_y_pred<-cbind(pred_df,
                 pred = inv_logit(do_b0+do_b1* pred_df$size_t0 + do_b2 * pred_df$Site + do_b3 * pred_df$Site * pred_df$size_t0) 
)


### Plot predictions

Surv <- ggplot() +
  geom_point(data = logitbin_df(df = iso, resp = surv_t1, xvar = size_t0, Site) %>% mutate(Site = as.factor(Site)), 
             aes(x = size_t0, y = surv_t1)) + 
  geom_line(data = sr_y_pred, 
            aes(x = size_t0, y = pred), 
            colour = "red")

Growth <- ggplot() +
  geom_point(data = iso %>% mutate(Site = as.factor(Site)), 
             aes(x = size_t0, y = size_t1)) + 
  geom_line(data = gr_y_pred, 
            aes(x = size_t0, y = pred), 
            colour = "red")

Flwr_p <- ggplot() +
  geom_point(data = logitbin_df(df = iso, resp = flower_t1, xvar = size_t0, Site) %>% mutate(Site = as.factor(Site)), 
             aes(x = size_t0, y = flower_t1, colour = Site)) + 
  geom_line(data = flp_y_pred %>% mutate(Site = as.factor(Site)), 
            aes(x = size_t0, y = pred, colour = Site))

Flwr_n <- ggplot() +
  geom_point(data = iso %>% mutate(Site = as.factor(Site)), 
             aes(x = size_t0, y = n_flower_t1, colour = Site)) + 
  geom_line(data = fln_y_pred %>% mutate(Site = as.factor(Site)), 
            aes(x = size_t0, y = pred, colour = Site))

Dorm <- ggplot() +
  geom_point(data = logitbin_df(df = iso, resp = dormancy_t1, xvar = size_t0, Site) %>% mutate(Site = as.factor(Site)), 
             aes(x = size_t0, y = dormancy_t1, colour = Site)) + 
  geom_line(data = do_y_pred %>% mutate(Site = as.factor(Site)), 
            aes(x = size_t0, y = pred, colour = Site))

(Surv + Growth) / (Flwr_p + Flwr_n) / (Dorm + plot_spacer()) + plot_layout(guides = "collect")
