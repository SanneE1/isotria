
# IPM from data by aldo

library(ggplot2)
library(testthat)
library(lme4)
library(bbmle)
library(plyr); library(dplyr)
library(tidyr)
library(lme4)
options(stringsAsFactors = F)


# read data 

# creating a dataframe (df) with logarithmic size_t0 and size_t1 called iso
iso<-read.csv("data/isotria_long.csv") %>%
  subset(size_t0 != 0) %>%
  mutate(size_t0 = log(size_t0),
         size_t1 = log(size_t1))

iso$size_t1[which(iso$size_t1 < -999)] <- NA

# fit models 

sr_mod  <- glmer(surv_t1 ~ size_t0 + (size_t0 | year_t1), data = iso, family = 'binomial' )

gr_mod  <- lmer(size_t1 ~ size_t0 + (size_t0 | year_t1), data = iso)

flowpop_mod <- glmer(flower_t1 ~ size_t0 * Site + (1 | year_t1),data= iso, family = 'binomial')


flower_n_mod <-glmer(n_flower_t1 ~ size_t0 + Site + (1 | year_t1), data = iso, family = 'poisson')


dorm_mod<-glmer(dormancy_t1 ~ size_t0 * Site + (1 | year_t1),data = iso, family = 'binomial')



# propability to get out of dormancy

d <- read.csv('data/isotria_long.csv')
dorm <- d[which( d$surv_t1 == 1),] 

# quick check if there are individuals that remain in dormancy more than one year
a <- ddply(dorm[which(!is.na(dorm$remain_dorm_t1)),], .(New_Tag), transform, duration_dormancy = cumsum(remain_dorm_t1))

# get out of dormancy probability

p_stay <- mean(dorm$remain_dorm_t1, na.rm = TRUE) 
p_out <- 1 - p_stay


# Number of fruits per flower
# Read and format data
flower_n <- d[complete.cases(d$size_t0, d$surv_t1, d$Habitat_Man),] %>%
  subset(size_t0 != 0)  %>%
  mutate(size_t0 = log(size_t0))
# creating a dataframe that excludes zeros 

g<-d %>% filter(!is.na(n_flower_t1) & n_flower_t1 != 0) 


#creating a new collum that contains the number of fruits per flower
#by deviding the number of fruits by the number of flowers
g$n_fruits_per_flower<-g$n_fruit_t1/g$n_flower_t1


# getting the mean of friuts per flower
fruiting<- mean(g$n_fruits_per_flower, na.rm=TRUE)

# getting the mean and sd for the size distribution of new plants
w1 <-d %>% filter(is.na(stage_t0) & stage_t1 == 'plant') 
new_plants_mean<-mean(w1$size_t1, na.rm = T)
new_plants_sd<-sd(w1$size_t1, na.rm = T)

# getting the mean and sd for the size distribution of woke plants
k1<- d %>% filter(stage_t0=='dormant' & stage_t1 == 'plant' )

<<<<<<< HEAD:my IPM.R
l2<-l1 %>%filter(!is.na(size_t1))
woke_plants_mean<-mean(l2$size_t1)
woke_plants_sd<-sd(l2$size_t1)

=======
woke_plants_mean<-mean(k1$size_t1, na.rm = T)
woke_plants_sd<-sd(k1$size_t1, na.rm = T)
>>>>>>> 1368614a26c309404780328dc3b2e59b4e7dc59f:Sophie/my IPM.R



#2. Create the function to apply the inverse logit
inv_logit<-function(x){
  exp(x)/(1+exp(x))}


<<<<<<< HEAD:my IPM.R
#3. Plot the three isotria models

# plot models
par( mfrow=c(2,2), mar = c(3,3,1,0.3), mgp = c(2,0.7,0) )
plot(glmer(surv_t1 ~ size_t0 + size_t0 | year_t1), data = iso, family = binomial )
lines(x_seq,sr_y_pred,col="red")
plot(lm(size_t1 ~ size_t0 + (size_t0 | year_t1), data = iso_gr) )
lines(x_seq,gr_y_pred,col="red")
plot(glmer(flower_t1 ~ size_t0 * Site + (1 | year_t1),data= iso,family =binomial))
lines(x_seq,flp_y_pred,col="red")
plot(glmer(n_flower_t1 ~ size_t0 + Site + (1 | year_t1), data= iso, family = poisson()))
lines(x_seq,fln_y_pred,col="red")
plot(glmer(dormancy_t1 ~ size_t0 * Site + (1 | year_t1),data = iso, family = binomial()))
lines(x_seq,do_y_pred,col="red")


# sequence of X values
x_seq <- seq(min(iso$size_t0, na.rm=T), 
             max(iso$size_t0, na.rm=T), by = 0.1)

#sr
sr_b0<-fixef(sr_mod)[1]
sr_b1<-fixef(sr_mod)[2]
sr_y_pred<-(sr_b0 + sr_b1*(x_seq))

#gr
gr_b0<-fixef(gr_mod)[1]
gr_b1<-fixef(gr_mod)[2]
gr_y_pred<-(gr_b0 + gr_b1 *(x_seq))

#flowpop_mod
flp_b0<-fixef(flowpop_mod)[1]
flp_b1<-fixef(flowpop_mod)[2]
flp_b2<-fixef(flowpop_mod)[3]
flp_b3<-fixef(flowpop_mod)[4]
flp_y_pred<-exp(flp_b0+flp_b1flp_b2+flp_b3*(x_seq))   ### this doesn't follow the model

#flower_n_mod
fln_b0<-fixef(flower_n_mod)[1]
fln_b1<-fixef(flower_n_mod)[2]
fln_b2<-fixef(flower_n_mod)[3]
fln_y_pred<-exp(fln_b0+fln_b1+ fln_b2*(x_seq))   ### this doesn't follow the model


#dorm_mod
do_b0<-fixef(dorm_mod)[1]
do_b1<-coef(dorm_mod)[2]
do_b2<-coef(dorm_mod)[3]
do_b3<-coef(dorm_mod)[4]
do_y_pred<-exp(do_b0+do_b1+do_b2+do_b3*(x_seq)) ### this doesn't follow the model


=======
>>>>>>> 1368614a26c309404780328dc3b2e59b4e7dc59f:Sophie/my IPM.R
# IPM functions -------------------------------------------------------------

# this is a list with containing the IPM parameters. 
# To access a single values, use $ sign (e.g. pars$surv_b0)
# fixef
pars  <- list( surv_b0 = fixef(sr_mod)[1],
               surv_b1 = fixef(sr_mod)[2],
               grow_b0 = fixef(gr_mod)[1],
               grow_b1 = fixef(gr_mod)[2],
               grow_sd = summary(gr_mod)$sigma,
               flop_b0 = fixef(flowpop_mod)[1],
               flop_b1 = fixef(flowpop_mod)[2],
               flop_b2 = fixef(flowpop_mod)[3],
               flop_b3 = fixef(flowpop_mod)[4],
               flon_b0 = fixef(flower_n_mod)[1],
               flon_b1 = fixef(flower_n_mod)[2],
               flon_b2 = fixef(flower_n_mod)[3],
               dom_b0 = fixef(dorm_mod)[1],
               dom_b1 = fixef (dorm_mod)[2],
               dom_b2 = fixef(dorm_mod)[3],
               dom_b3 = fixef(dorm_mod)[4],
               p_stay = p_stay, 
               p_out = p_out,
               fruiting=fruiting,
               new_plants_mean = new_plants_mean,
               new_plants_sd = new_plants_sd,
               woke_plants_mean = woke_plants_mean,
               woke_plants_sd = woke_plants_sd,
               L       = min(iso$size_t0,na.rm=T),
               U       = max(iso$size_t0,na.rm=T),
               mat_siz = 50
)


# Transforms all values below/above limits in min/max size

x_range <- function(x,pars){
  pmin(pmax(x,pars$L),pars$U)
}

# Growth (transition) from size x to size y
gxy <- function(y, x, pars){
  xb <- x_range(x, pars)
  # returns a *probability density distribution* for each x value
  return( dnorm(y, 
                mean = pars$grow_b0 +pars$grow_b1 *xb, 
                sd   = pars$grow_sd) )
}

# Survival at size x
sx<-function(x,pars){
  xb <- x_range(x, pars)
  # survival prob. of each x size class 
  return( inv_logit(pars$surv_b0 +pars$surv_b1 * xb) )
}

# transition: Survival * growth
pxy<-function(x,y,pars, site){
  xb <- x_range(x, pars)
  return( sx(xb,pars) * (1 - dox(xb, pars, site)) * gxy(xb,y,pars) )
}


# flowering at size x
flx<-function(x,pars, site){
  xb <- x_range(x, pars)
  # flowering  prob. of each x size class 
  return( inv_logit(pars$flop_b0 + 
                    pars$flop_b1 * xb +
                    pars$flop_b2 * site +
                    pars$flop_b3 * xb * site) )
}


# flowernumber at size x
fln<-function(x, pars, site){
  xb <- x_range(x, pars)
  # flowernumber of each x size class 
  return( exp(pars$flon_b0 + pars$flon_b1 * xb + pars$flon_b2 * site)) 
}


# propability to go dormant at size x
dox<-function(x,pars, site){
  xb <- x_range(x, pars)
  return( inv_logit(pars$dom_b0 + 
                      pars$dom_b1 * xb +
                      pars$dom_b2 * site +
                      pars$dom_b3 * site * xb) )
}

# New recruits from size x to size y
nrxy <- function(y,pars, h){
  
  # returns a *probability density distribution* for each x value
  return( dnorm(y, 
                mean = pars$new_plants_mean , 
                sd   = pars$new_plants_sd) ) * h
}

# woke plants from size x to size y
woxy <- function(y,pars, h){
  
  # returns a *probability density distribution* for each x value
  return( dnorm(y, 
                mean = pars$woke_plants_mean , 
                sd   = pars$woke_plants_sd) ) * h
}

#fecundity
fec<-function(x,y, pars, h, site){
  xb <- x_range(x, pars)
 return( flx(xb,pars, site) * fln(xb,pars, site) * pars$fruiting * nrxy(y,pars, h) )
}



# IPM lambda ------------------------------------------------------------

# IPM kernel
kernel <- function(pars, site){
  
  n   <- pars$mat_siz
  L   <- pars$L 
  U   <- pars$U
  #these are the upper and lower integration limits
  h   <- (U-L)/n                   #Bin size
  b   <- L+c(0:n)*h                #Lower boundaries of bins 
  y   <- 0.5*(b[1:n]+b[2:(n+1)])   #Bins' midpoints
  #these are the boundary points (b) and mesh points (y)
  
  # Fertility matrix
  
  Fmat            <- matrix(0,(n+1),(n+1))
  
  Fmat[2:(n+1),
       2:(n+1)] <-fec(y, y, pars, h, site)
  
  
 
  
  # Growth/survival transition matrix
  Tmat       <- matrix(0,(n+1),(n+1))
  
  # Growth/survival transitions among cts sizes
  Tmat[2:(n+1),
       2:(n+1)]   <- t( outer(y,y,pxy,pars, site)* h )
  
# Dormancy
  # living  plants go dormant and go in top row
  Tmat[1,2:(n+1)] <-  dox(y,pars,site)  #get in to  dormancy
  
  # dormant plants stay dormant for an other year
  Tmat[1,1]       <- pars$p_stay #stay dormant 
  
  # Graduation from dormancy to cts size
  Tmat[2:(n+1),1] <- pars$p_out * woxy(y,pars,h) #get out of d * size distn   
  

  
  # Full Kernel is simply a summation of fertility and transition matrix
  k_yx          <- Fmat+Tmat     
  
  return(list(k_yx    = k_yx,
              Fmat    = Fmat,
              Tmat    = Tmat,
              meshpts = y))
  
}

# there are 4 sites (numbered 1-4)

# K_site1 <- kernel(pars, site = 1)

# Deterministic lambda

lambda <-function(pars, site){
  Re(eigen(kernel(pars, site)$k_yx)$value[1])
}

lambda(pars, site = 1)
lambda(pars, site = 2)
lambda(pars, site = 3)
lambda(pars, site = 4)


