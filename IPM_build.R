# IPM from data by aldo
setwd("C:/Users/sophi/Documents/Spacial ecology/Spacial ecology 3/day2/data")

# Check for and install required packages
for (package in c('dplyr', 'tidyr')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
library(dplyr);library(tidyr)
options(stringsAsFactors = F)

# read data 
op_dem  <- read.csv("opuntia_all_yrs.csv")
germ_d  <- read.csv("opuntia_germ.csv")
r_size  <- subset(op_dem, Recruit == 1)$log_vol_t1 


# Visualize data ---------------------------------------------------------------------
tiff("opuntia_viable_buds.tiff", unit="cm", 
     width=16, height=16, res=400, compression="lzw")

par( mfrow=c(1,1), mar = c(3.5,3.5,1,0.3), mgp = c(2,0.7,0) )
hist(op_dem$Goodbuds_t, main="", 
     cex.lab=2, cex.axis=1.5, xlab = "viable_buds_t")
dev.off()
head(op_dem)


#1. Plot survival, growth, fecundity (viable buds), and histogram of recruitment size at time t+1
par( mfrow=c(2,2), mar = c(3.5,3.5,1,0.3), mgp = c(2,0.7,0) )
plot(jitter(Survival_t1) ~ log_vol_t, data = op_dem )
plot(log_vol_t1   ~ log_vol_t, data = op_dem )
plot(Goodbuds_t  ~ log_vol_t, data = op_dem)
hist(r_size)
# Plot GLM ---------------------------------------------------------------

# fit models 
sr_mod  <- glm(Survival_t1 ~ log_vol_t, data = op_dem, family="binomial")
gr_mod  <- lm(log_vol_t1   ~ log_vol_t, data = op_dem )
fec_mod <- glm(Goodbuds_t  ~ log_vol_t, data = op_dem, family="poisson")
rec_mod <- lm(r_size ~ 1)


#2. Create the function to apply the inverse logit
inv_logit<-function(x){
  exp(x)/(1+exp(x))}


#3. Plot the three opuntia models, and the histogram of recruitment size

# plot models
par( mfrow=c(2,2), mar = c(3,3,1,0.3), mgp = c(2,0.7,0) )
plot(jitter(Survival_t1) ~ log_vol_t, data = op_dem )
lines(x_seq,inv_logit(sr_y_pred),col="red")
plot(log_vol_t1   ~ log_vol_t, data = op_dem )
lines(x_seq,gr_y_pred,col="red")
plot(Goodbuds_t  ~ log_vol_t, data = op_dem)
lines(x_seq,fec_y_pred,col="red")
# sequence of X values
x_seq <- seq(min(op_dem$log_vol_t, na.rm=T), 
             max(op_dem$log_vol_t, na.rm=T), by = 0.1)

#sr
sr_b0<-coef(sr_mod)[1]
sr_b1<-coef(sr_mod)[2]
sr_y_pred<-(sr_b0+sr_b1*(x_seq))

#gr
gr_b0<-coef(gr_mod)[1]
gr_b1<-coef(gr_mod)[2]
gr_y_pred<-(gr_b0+gr_b1*(x_seq))

#fec_mod
fec_b0<-coef(fec_mod)[1]
fec_b1<-coef(fec_mod)[2]
fec_y_pred<-exp(fec_b0+fec_b1*(x_seq))

# IPM functions -------------------------------------------------------------

# this is a list with containing the IPM parameters. 
# To access a single values, use $ sign (e.g. pars$surv_b0)
pars  <- list( surv_b0 = coef(sr_mod)[1],
               surv_b1 = coef(sr_mod)[2],
               grow_b0 = coef(gr_mod)[1],
               grow_b1 = coef(gr_mod)[2],
               grow_sd = summary(gr_mod)$sigma,
               fecu_b0 = coef(fec_mod)[1],
               fecu_b1 = coef(fec_mod)[2],
               recr_sz = coef(rec_mod),
               recr_sd = summary(rec_mod)$sigma,
               germ1   = germ_d$germ_1,
               germ2   = germ_d$germ_2,
               seed_n  = germ_d$seed_x_bud,
               L       = min(op_dem$log_vol_t,na.rm=T),
               U       = max(op_dem$log_vol_t,na.rm=T),
               mat_siz = 50
)
pars$surv_b0

# functions 

# Transforms all values below/above limits in min/max size
x_range <- function(x,pars){
  pmin(pmax(x,pars$L),pars$U)
}

# Growth (transition) from size x to size y
gxy <- function(x,y,pars){
  xb <- x_range(x, pars)
  # returns a *probability density distribution* for each x value
  return( dnorm(y, 
                mean = pars$grow_b0 + pars$grow_b1*xb, 
                sd   = pars$grow_sd) )
}

# Survival at size x
sx<-function(x,pars){
  xb <- x_range(x, pars)
  # survival prob. of each x size class 
  return( inv_logit(pars$surv_b0 + pars$surv_b1 * xb) )
}

# transition: Survival * growth
pxy<-function(x,y,pars){
  xb <- x_range(x, pars)
  return( sx(xb,pars) * gxy(xb,y,pars) )
}

# Production of 1-YO seeds in seed bank from x-sized moms
fx<-function(x,pars){
  xb      <- x_range(x, pars)
  # n. of buds maturing
  nfruits <- exp(pars$fecu_b0 + pars$fecu_b1*xb) 
  # n. from buds to viable seed
  return( nfruits*pars$seed_n )
}

# Size distribution of recruits
recruits<-function(y,pars){
  dnorm( x    = y,
         mean = pars$recr_sz,
         sd   = pars$recr_sd )
}


# discretize the size distribution
n   <- pars$mat_siz
L   <- pars$L 
U   <- pars$U
#these are the upper and lower integration limits
h   <- (U-L)/n                   #Bin size
b   <- L+c(0:n)*h                #Lower boundaries of bins 
y   <- 0.5*(b[1:n]+b[2:(n+1)])   #Bins' midpoints


# Plot functions sx, fx, and recruit.
tiff("function_plots.tiff", unit="cm", 
     width=16, height=16, res=400, compression="lzw")

# plot models
par( mfrow=c(2,2), mar = c(3,3,2,0.3), mgp = c(2,0.7,0) )

plot(y, fx(y,pars), 
     xlab = expression('y (that is: log size'[t]*')'),
     main = 'fecundity function' )
plot(y, sx(y,pars), main = 'survival function',
     xlab = expression('y (that is: log size'[t]*')') )
plot(y, recruits(y,pars)*h, 
     xlab = expression('y (that is: log size'[t]*')'),
     main = 'prob. of recruit size' )
dev.off()



# set up a population that contains 1 individual for each y size bin
n_vec <- rep(1, length(y) )


# 4.How many of these 50 individuals survive from time t to time t+1? Provide the result and paste the code with which you obtained this result.
sx(y,pars)%*%n_vec

#38.5
# 5. How many seed are produced by the population of these 50 individuals? Provide the result and paste the code with which you obtained this result.

fx(y,pars)%*%n_vec
#66.8

# IPM lambda ------------------------------------------------------------

# IPM kernel
kernel <- function(pars){
  
  n   <- pars$mat_siz
  L   <- pars$L 
  U   <- pars$U
  #these are the upper and lower integration limits
  h   <- (U-L)/n                   #Bin size
  b   <- L+c(0:n)*h                #Lower boundaries of bins 
  y   <- 0.5*(b[1:n]+b[2:(n+1)])   #Bins' midpoints
  #these are the boundary points (b) and mesh points (y)
  
  # Fertility matrix
  Fmat            <- matrix(0,(n+2),(n+2))
  
  # Banked seeds go in top row
  Fmat[1,3:(n+2)] <- fx(y,pars)
  
  # Growth/survival transition matrix
  Tmat            <- matrix(0,(n+2),(n+2))
  
  # Graduation to 2-yo seed bank = pr(not germinating as 1-yo)
  Tmat[2,1]       <- 1 - pars$germ1
  
  # Graduation from 1-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),1] <- pars$germ1 * recruits(y,pars)*h   
  
  # Graduation from 2-yo bank to cts size = germination * size distn * pre-census survival
  Tmat[3:(n+2),2] <- pars$germ2 * recruits(y,pars)*h   
  
  # Growth/survival transitions among cts sizes
  Tmat[3:(n+2),
       3:(n+2)]   <- t( outer(y,y,pxy,pars) )*h
  
  # Full Kernel is simply a summation of fertility and transition matrix
  k_yx          <- Fmat+Tmat     
  
  return(list(k_yx    = k_yx,
              Fmat    = Fmat,
              Tmat    = Tmat,
              meshpts = y))
  
}

# Deterministic lambda
lambda <- Re(eigen(kernel(pars)$k_yx)$value[1])


#6. Create a function that calculates deterministic lambda

lamda1<-function(pars)
{
  Re(eigen(kernel(pars)$k_yx)$value[1])
}
pars$mat_siz  <- sizes[i]
lamda1(pars)


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

