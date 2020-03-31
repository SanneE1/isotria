
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
#iso<-read.csv("data/isotria_long.csv") %>%
 # mutate(size_t0 = log(size_t0),
         #size_t1 = log(size_t1),
         #Site = as.factor(Site))



#iso$size_t0[which(iso$size_t0 < -999)] <- NA
#iso$size_t1[which(iso$size_t1 < -999)] <- NA

# creating a dataframe (df) with logarithmic size_t0 and size_t1 called iso by Aldo
iso <- read.csv("data/isotria_long.csv") %>%
  subset( size_t0 != 0 ) %>%
  mutate( size_t0 = log(size_t0),
          size_t1 = log(size_t1),
          Site = as.factor(Site)) %>% 
  mutate( size_t1 = replace( size_t1,
                             size_t1 == -Inf,
                             NA) )
         

iso$size_t1[which(iso$size_t1 < -999)] <- NA
str(iso)


# fit models 

sr_mod  <- glmer(surv_t1 ~ size_t0 + Site + (1 | year_t1), data = iso, family = 'binomial' )

gr_mod  <- lmer(size_t1 ~ size_t0 + (size_t0 | year_t1), data = iso)

flowpop_mod <- glmer(flower_t1 ~ size_t1 + Site + (1 | year_t1),data= iso, family = 'binomial')

flower_n_mod <-glmer(n_flower_t1 ~ size_t1 + Site + (1 | year_t1), data = iso, family = 'poisson')

dorm_mod<-glmer(dormancy_t1 ~ size_t1 + (1 | year_t1), data = iso, family = 'binomial')

# ALDOS way-------------------------------------------

# new plants 
re_df <- read.csv('data/isotria_long.csv') %>% 
  filter( is.na(stage_t0) & stage_t1 == 'plant' ) %>% 
  mutate( size_t0 = log(size_t0),
          size_t1 = log(size_t1), 
          Site = as.factor(Site)) %>% 
  mutate( size_t1 = replace( size_t1,
                             size_t1 == -Inf,
                             NA) )

new_plants_mean <- mean( re_df$size_t1, na.rm = T)
new_plants_sd <- sd( re_df$size_t1, na.rm = T)

# abotion rate
abort_v <- re_df %>% 
  filter( !is.na(n_flower_t1) & n_flower_t1 != 0 ) %>%
  subset( !(n_fruit_t1 %in% 2) ) %>% 
  .$n_fruit_t1 %>% table
abort_r <- abort_v[1] / abort_v[2]
abort_r
# recruit number
rec_n_df <- re_df %>% 
  count(Site, year_t1) %>% 
  rename( rec_n_t1 = n )

# Recruits by number of flowers
rec_by_flow <- iso %>%
  # remove the ONE n_fruit_t1 == 2
  subset( !(n_fruit_t1 == 2) ) %>% 
  subset( n_flower_t1 != 0 ) %>% 
  # mutate( effective_flows_n = n_flower_t1 * n_fruit_t1 ) %>% 
  group_by( Site, year_t1 ) %>% 
  summarise( flows_t0 = sum(flower_t1) ) %>% 
  ungroup %>% 
  # +1 to make this year t0
  mutate( year_t1 = year_t1 + 1 ) %>% 
  # link to number of new recruits
  full_join( rec_n_df )

# n. of recruits 
recr_by_flow <- (rec_by_flow$rec_n_t1/rec_by_flow$flows_t0) %>% mean(na.rm=T)




#our way--------------------------------------------------------------------
# Get n_recruits/n_fruits
#iso_nfruit <- iso %>% 
 # select(Site, year_t1, n_fruit_t1) %>%
  #group_by(Site, year_t1) %>%
  #summarise(n_fruit_t0 = sum(n_fruit_t1, na.rm = T)) %>%
  #mutate(year_t1 = year_t1 + 1)

#iso_nrec <- iso[which(is.na(iso$stage_t0) & iso$stage_t1 == 'plant'),] %>%
  #group_by(Site, year_t1) %>%
  #summarise(n_rec_t1 = n())

#iso_rec_fruit <- left_join(iso_nfruit, iso_nrec, by = c("Site", "year_t1")) %>%
#mutate(n_rec_t1 = replace_na(n_rec_t1, 0))
#iso_rec_fruit$proportion <- iso_rec_fruit$n_rec_t1 / iso_rec_fruit$n_fruit_t0

#germ_mod<-lmer(proportion ~ Site + (1 | year_t1), data= iso_rec_fruit)

# dormancy probabilities
#dorm <- iso[which( iso$surv_t1 == 1),] 

#p_stay <- mean(dorm$remain_dorm_t1, na.rm = T)
#p_out <- 1 - p_stay


# Number of fruits per flower
# Read and format data
#n_fruits_per_flower <- iso %>%
  #mutate(fuits_per_flower = ifelse(n_flower_t1 > 0, n_fruit_t1 / n_flower_t1, NA))

#fruiting<- mean(n_fruits_per_flower$fuits_per_flower, na.rm = T)

# getting the mean and sd for the size distribution of new plants
#w1 <-iso %>% filter(is.na(stage_t0) & stage_t1 == 'plant') 
#new_plants_mean<-mean(w1$size_t1, na.rm = T)
#new_plants_sd<-sd(w1$size_t1, na.rm = T)

# getting the mean and sd for the size distribution of woke plants
#k1<- iso %>% filter(stage_t0=='dormant' & stage_t1 == 'plant' )

#woke_plants_mean<-mean(k1$size_t1, na.rm = T)
#woke_plants_sd<-sd(k1$size_t1, na.rm = T)

#----------------------------------------------------------

#2. Create the function to apply the inverse logit
inv_logit<-function(x){
  exp(x)/(1+exp(x))}


# IPM functions -------------------------------------------------------------

# this is a list with containing the IPM parameters. 
# To access a single values, use $ sign (e.g. pars$surv_b0)
# fixef
pars  <- list( surv_b0 = fixef(sr_mod)[1],
               surv_b1 = fixef(sr_mod)[2],
               surv_site = list(0,
                                fixef(sr_mod)[3],
                                fixef(sr_mod)[4],
                                fixef(sr_mod)[5]),
               grow_b0 = fixef(gr_mod)[1],
               grow_b1 = fixef(gr_mod)[2],
               grow_sd = summary(gr_mod)$sigma,
               flop_b0 = fixef(flowpop_mod)[1],
               flop_b1 = fixef(flowpop_mod)[2],
               flop_site = list(0,
                                fixef(flowpop_mod)[3],
                                fixef(flowpop_mod)[4],
                                fixef(flowpop_mod)[5]),
               flon_b0 = fixef(flower_n_mod)[1],
               flon_b1 = fixef(flower_n_mod)[2],
               flon_site = list(0,
                                fixef(flower_n_mod)[3],
                                fixef(flower_n_mod)[4],
                                fixef(flower_n_mod)[5]),
               #dom_b0 = fixef(dorm_mod)[1],
               #dom_b1 = fixef (dorm_mod)[2],
               #ge_b0 = fixef(germ_mod)[1],
               #ge_site = list(0,
                              #fixef(germ_mod)[2],
                              #fixef(germ_mod)[3],
                              #fixef(germ_mod)[4]),
               #p_stay = p_stay, 
               #p_out = p_out,
               #fruiting=fruiting,
               recr_by_flow = recr_by_flow, #aldo
               new_plants_mean = new_plants_mean,
               new_plants_sd = new_plants_sd,
               abort_r = abort_r,#aldo
               #woke_plants_mean = woke_plants_mean,
               #woke_plants_sd = woke_plants_sd,
               L       = min(iso$size_t0,na.rm=T),
               U       = max(iso$size_t0,na.rm=T),
               mat_siz = 50
)
pars$surv_site
pars$flop_site
pars$ flon_site 
# Transforms all values below/above limits in min/max size

x_range <- function(x,pars){
  pmin(pmax(x,pars$L),pars$U)
}

#x_seq <- function( x ){
  
  #min_x <- min(x, na.rm=T)
  #max_x <- max(x, na.rm=T)
  
  #seq(min_x, max_x, length.out=100)
  
#}

# Growth (transition) from size x to size y
gxy <- function(y, x, pars){
  xb <- x_range(x, pars)
  # returns a *probability density distribution* for each x value
  return( dnorm(y, 
                mean = pars$grow_b0 +pars$grow_b1 *xb, 
                sd   = pars$grow_sd) )
}

# Survival at size x
sx<-function(x,pars, site){
  xb <- x_range(x, pars)
  # survival prob. of each x size class 
  return( inv_logit(pars$surv_b0 + pars$surv_b1 * xb + pars$surv_site[[site]]) )
}


# transition: Survival * growth
#pxy<-function(x,y,pars, site){
  #xb <- x_range(x, pars)
  #return( sx(xb,pars, site) * (1 - dox(xb, pars)) * gxy(xb,y,pars) )
#}

# Aldos way transition: Survival * growth
pxy <-function(y,x,pars,site){
  xb <- x_range(x, pars)
  return( sx(xb,pars,site) * gxy(y,x,pars) )
}

# flowering at size x
flx<-function(x,pars, site){
  xb <- x_range(x, pars)
  # flowering  prob. of each x size class 
  return( inv_logit(pars$flop_b0 + 
                    pars$flop_b1 * xb +
                    pars$flop_size[[site]] )*0.84615385  ) #0.84.. maximum flowerpropability by aldo 
}


# flowernumber at size x
fln<-function(x, pars, site){
  xb <- x_range(x, pars)
  # flowernumber of each x size class 
  return( exp(pars$flon_b0 + pars$flon_b1 * xb + pars$flon_site[[site]])) 
}

# propability to go dormant at size x
dox<-function(x,pars){
  xb <- x_range(x, pars)
  return( inv_logit(pars$dom_b0 + 
                      pars$dom_b1 * xb) 
          )
}

# New recruits from size x to size y
nrxy <- function(y,pars ){# maybe h is missing
  
  # returns a *probability density distribution* for each x value
  return( dnorm(y, 
                mean = pars$new_plants_mean , 
                sd   = pars$new_plants_sd) ) #* h #aldo has no h in here, but in the Fmat
}




# woke plants from size x to size y
#woxy <- function(y,pars, h){
  
  # returns a *probability density distribution* for each x value
  #return( dnorm(y, 
               # mean = pars$woke_plants_mean , 
               # sd   = pars$woke_plants_sd) ) * h
#}

#fecundity
#fec<-function(x,y, pars, h, site){
 # xb <- x_range(x, pars)
 #return( flx(xb,pars, site) * fln(xb,pars, site) * pars$fruiting * (pars$ge_b0 + pars$ge_site[[site]]) * nrxy(y,pars, h) )
#}

#fecundity by aldo but with Site, h is multiplied with in the Fmat
fec<-function(y, x, pars, site){
  xb<- x_range(x, pars)
  flx(xb, pars,site)*fln(xb,pars,site)  *  pars$abort_r *  pars$recr_by_flow *nrxy(y,pars)
}

# IPM lambda ------------------------------------------------------------

# IPM kernel
kernel <- function( pars, site){
  
  n   <- pars$mat_siz
  L   <- pars$L 
  U   <- pars$U
  #these are the upper and lower integration limits
  h   <- (U-L)/n                   #Bin size
  b   <- L+c(0:n)*h                #Lower boundaries of bins 
  y   <- 0.5*(b[1:n]+b[2:(n+1)])   #Bins' midpoints
  #these are the boundary points (b) and mesh points (y)
  #  AldoIPM kernel
 
  # Fertility matrix
  
  #Fmat            <- matrix(0,(n+1),(n+1))
  
  #Fmat[2:(n+1),
      # 2:(n+1)] <- fec(y, y, pars, h, site)
  
  
  # aldo Fertility matrix
  Fmat <- matrix(0, n, n)
  Fmat <- outer(y, y, fec, pars,site) * h 
  
  # Growth/survival transition matrix
  #Tmat       <- matrix(0,(n+1),(n+1))
  
  # Growth/survival transitions among cts sizes
  #Tmat[2:(n+1),
       #2:(n+1)]   <- t( outer(y,y,pxy,pars, site)* h )
  
  # Aldo Growth/survival transition matrix
  Tmat <- matrix(0,n,n)
  
  # Growth/survival transitions among cts sizes
  Tmat <- outer(y,y,pxy,pars,site) * h
  
# Dormancy
  # living  plants go dormant and go in top row
  #Tmat[1,2:(n+1)] <-  sx(y,pars, site) * dox(y,pars)  #get in to  dormancy
  
  # dormant plants stay dormant for an other year
  #Tmat[1,1]       <- pars$p_stay #stay dormant 
  
  # Graduation from dormancy to cts size
  #Tmat[2:(n+1),1] <- pars$p_out * woxy(y,pars,h) #get out of d * size distn   
  

  
  # Full Kernel is simply a summation of fertility and transition matrix
  k_yx          <- Fmat+Tmat     
  
  return(list(k_yx    = k_yx,
              Fmat    = Fmat,
              Tmat    = Tmat,
              meshpts = y))
  
}
a<-kernel(pars,3)
a$k_yx
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

