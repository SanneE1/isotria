
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


# fit models 

sr_mod  <- glmer(surv_t1 ~ size_t0 + (size_t0 | year_t1), data = iso %>% 
                             filter(!is.na(surv_t1) & (!is.na(size_t0))), family = 'binomial' )


# creating a dataframe that excludes size_t0 = 0  and size_t1 = 0 and allso makes size_t0 and size_t0 logarythmic
iso_gr<-iso %>%
  subset(size_t1 > -999)

gr_mod  <- lmer(size_t1 ~ size_t0 + (size_t0 | year_t1), data = iso_gr %>% 
              filter(!is.na(size_t1) & (!is.na(size_t0))))

flowpop_mod <- glmer(flower_t1 ~ size_t0 * Site + (1 | year_t1),data= iso%>%
                       filter(!is.na(flower_t1) & (!is.na(size_t0))), family = 'binomial')


flower_n_mod <-glmer(n_flower_t1 ~ size_t0 + Site + (1 | year_t1), data = iso %>%
                       filter(!is.na(n_flower_t1) & (!is.na(size_t0))) , family = 'poisson')


dorm_mod<-glmer(dormancy_t1 ~ size_t0 * Site + (1 | year_t1),data = iso%>%
                  filter(!is.na(dormancy_t1) & (!is.na(size_t0))), family = 'binomial')



# propability to get out of dormancy

d <- read.csv('data/isotria_long.csv')
dorm <- d[which( d$surv_t1 == 1),] 

# quick check if there are individuals that remain in dormancy more than one year
a <- ddply(dorm[which(!is.na(dorm$remain_dorm_t1)),], .(New_Tag), transform, duration_dormancy = cumsum(remain_dorm_t1))
max(a$duration_dorm)

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
w1 <-d %>% filter(is.na(stage_t0) & stage_t1 == 'plant' & !is.na(size_t1)) 
new_plants_mean<-mean(w1$size_t1)
new_plants_sd<-sd(w1$size_t1)

# getting the mean and sd for the size distribution of woke plants
k1<- d %>% filter(stage_t0=='dormant')
l1<-k1 %>% filter(stage_t1=='plant')

l2<-l1 %>%filter(!is.na(size_t1))
woke_plants_mean<-mean(l2$size_t1)
woke_plants_sd<-sd(l2$size_t1)




#2. Create the function to apply the inverse logit
inv_logit<-function(x){
  exp(x)/(1+exp(x))}


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

# functions 
x<- seq(min(iso$size_t0),
        max(iso$size_t0), length.out = n)

# Transforms all values below/above limits in min/max size

x_range <- function(x,pars){
  pmin(pmax(x,pars$L),pars$U)
}

# Growth (transition) from size x to size y
gxy <- function(x,y,pars){
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
pxy<-function(x,y,pars){
  xb <- x_range(x, pars)
  return( sx(xb,pars) * gxy(xb,y,pars) )
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
       2:(n+1)]   <- t( outer(y,y,pxy,pars)* h )
  
# Dormancy
  # living  plants go dormant and go in top row
  Tmat[1,2:(n+1)] <-dox(y,pars,site)  #get in to  dormancy
  
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
kernel(pars, site = 1)

# Deterministic lambda

lambda <-function(pars, site){
  Re(eigen(kernel(pars, site)$k_yx)$value[1])
}

lambda(pars, site = 1)
lambda(pars, site = 2)
lambda(pars, site = 3)
lambda(pars, site = 4)






















#7. Plot the effect of matrix (kernel) size on lambda 


# Loop set up (this is currently missing key elements)
sizes<-seq(44,100,by=3)
lam_v<-numeric(length(sizes))
lam_v2<-rep(0,length(sizes))

for(i in 1:length(sizes) ){
  
  pars$mat_siz  <- sizes[i]
  lam_v[i]<-lamda1(pars)
  lam_v2[i]<-(pars$mat_siz)
  
}
plot(lam_v~lam_v2)

#7. EXTRA: Lambda with mat_siz = 3: 
# how does it differ with lambda of the matrix model?




# IPM Elasticity --------------------------------------------------------

# elasticity analysis
pars$mat_siz = 50
k_yx  <- kernel(pars)$k_yx
lam   <- Re(eigen(k_yx)$values[1])
ev    <- eigen(k_yx) 

W <- ev$vectors 
w <- abs(Re(W[, 1])) #w is the right eigenvector, this also describes the reproductive value of each stage class.  Reproductive value depends on reproduction, survival and timing (i.e., an individual has to live to reach a reproductive stage in order to have reproductive value.  Low values could indicate a small proportion of individuals in a stage class survive to reproductive age).  
V <- try(Conj(solve(W)), silent=TRUE)
v <- abs(Re(V[1, ])) #v is the left eigenvector, this also describes he proportion of individuals in each stage class at stable stage distribution
s <- v %o% w #sensitivity of lambda to changes in each matrix element (aij) is proportional to the product of the ith element of the left eigenvector and the jth element of the right eigenvector 
rotate <- function(x) t(apply(x, 2, rev))
e <- s * k_yx/lam #elasticity is the proportional sensitivity of lambda to changes in each matrix element 
e <- rotate(e)



# 8. Plot the elasticity matrix using function IMAGE and  countour
?image
image(e,xlab = "Size(t)", ylab = "Size(t+1)")
contour(e,add=T) 

# 9. Given the result above, what are the sizes with the highest elasticity? 
# Is fecundity or survival/growth important for this species' life cycle, and why?
# (respond question on lab sheet, no need to code)



# 10. Looking at the graphs above AND the elasticity matrix, 
# can you speculate on why modeling unequal variance increases lambda?
# (respond question on lab sheet, no need to code)





# Transient dynamics --------------------------------------------------
pars$mat_siz <- 50
k_yx    <- kernel(pars)$k_yx
k_dim   <- pars$mat_siz + 2

# set up two vectors 
n0_big  <- rep(0,k_dim)
n0_seed <- rep(0,k_dim)

# introduce ONE individual in, respectively, the biggest size class, 
# and the smalles (one year old seeds)
n0_big[52] <- 1
n0_seed[1] <- 1

# Run projections of the population containing the two (big") the two 
proj_big  <- project(k_yx, vector=n0_big, time=50, 
                     standard.A=T, standard.vec=T)
proj_seed <- project(k_yx, vector=n0_seed, time=50, 
                     standard.A=T, standard.vec=T)


# Plot functions sx, fx, and recruit.
tiff("transient_analysis.tiff", unit="cm", 
     width=16, height=16, res=400, compression="lzw")

# plot pro
par( mfrow=c(1,1), mar=c(4,4,0.5,0.5), mgp=c(2.5,0.5,0))
plot(0:50,pr2.1,ylim =c(-1,35), type='l',
     ylab = 'Standardized population density', xlab='Time step',
     cex.lab = 3)
lines(0:50,pr2.2)
abline(h = 1, lty=2)

dev.off()


# reactivity and inertia when all individuals are in biggest size bin
inertia(k_yx, vector = n0_big)

# reactivity and inertia when all individuals are 1-year old seeds
inertia(k_yx, vector = n0_seed)

