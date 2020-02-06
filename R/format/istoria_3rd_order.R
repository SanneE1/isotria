# ISSUE INDIVIDUALS (gaps in Stage information), SOLVE: 454  497 5079 5080  170  240
library(tidyverse)
library(readxl)

isotria <- read_excel("data/demoallnew.xlsx") %>%
              mutate( key = paste( Site, Habitat_Man,sep="_") ) %>% 
              mutate( key = paste( key, New_Tag, sep="_") )
        
# vital rates in long form
long_form <- function(var, d){

  # extract variable names you need  
  var_names <-  grep(var, names(d), value = T)
  
  # format in long form
  dplyr::select(d, Site, New_Tag, Habitat_Man, var_names) %>% 
    gather_("year", paste0("value_",var), var_names) %>%
    mutate( year = gsub('[a-zA-Z]','',year ) ) %>%
    mutate( year = as.numeric(year) ) %>%
    setNames( gsub('value_', "", names(.)) )
  
}

# separate stage information
stg     <- long_form('Stage', isotria)
siz     <- long_form('Size', isotria)

# format size information ----------------------------------------------------

# to size t1
siz_t1    <- siz %>% rename( Size_t1 = Size )
siz_t0_t1 <- siz %>% rename( Size_t0 = Size ) %>%
                mutate( year = year + 1 )
siz_t1    <- inner_join(siz_t0_t1, siz_t1) %>% 
                mutate( year = year - 1)

# to size t2
siz_t2    <- siz %>% rename( Size_t2 = Size )
siz_t0_t2 <- siz %>% rename( Size_t0 = Size ) %>%
                mutate( year = year + 2 )
siz_t2    <- inner_join(siz_t0_t2, siz_t2) %>% 
                mutate( year = year - 2)

# to size t3
siz_t3    <- siz %>% rename( Size_t3 = Size )
siz_t0_t3 <- siz %>% rename( Size_t0 = Size ) %>%
                mutate( year = year + 3 )
siz_t3    <- inner_join(siz_t0_t3, siz_t3) %>% 
                mutate( year = year - 3)

# to size t4
siz_t4    <- siz %>% rename( Size_t4 = Size )
siz_t0_t4 <- siz %>% rename( Size_t0 = Size ) %>%
                mutate( year = year + 4 )
siz_t4    <- inner_join(siz_t0_t4, siz_t4) %>% 
                mutate( year = year - 4)


siz_3rd   <- Reduce( function(...)
                     full_join(...),
                     list(siz_t1, siz_t2, siz_t3, siz_t4) )

# data check
plot(log(Size_t1) ~ log(Size_t0), data=siz_3rd )
plot(log(Size_t2) ~ log(Size_t0), data=siz_3rd )
plot(log(Size_t3) ~ log(Size_t0), data=siz_3rd )


# format dormancy information ------------------------------------------------

# to stge t1
stg_t1    <- stg %>% rename( stage_t1 = Stage )
stg_t0_t1 <- stg %>% rename( stage_t0 = Stage ) %>%
                mutate( year = year + 1 )
stg_t1    <- inner_join(stg_t0_t1, stg_t1) %>% 
                mutate( year = year - 1)

# to stge t2
stg_t2    <- stg %>% rename( stage_t2 = Stage )
stg_t0_t2 <- stg %>% 
                rename( stage_t0 = Stage ) %>%
                mutate( year = year + 2 )
stg_t2    <- inner_join(stg_t0_t2, stg_t2) %>% 
                mutate( year = year - 2)

# to stge t3
stg_t3    <- stg %>% rename( stage_t3 = Stage )
stg_t0_t3 <- stg %>% rename( stage_t0 = Stage ) %>%
                mutate( year = year + 3 )
stg_t3    <- inner_join(stg_t0_t3, stg_t3) %>% 
                mutate( year = year - 3)

# to stge t3
stg_t4    <- stg %>% rename( stage_t4 = Stage )
stg_t0_t4 <- stg %>% rename( stage_t0 = Stage ) %>%
                mutate( year = year + 4 )
stg_t4    <- inner_join(stg_t0_t4, stg_t4) %>% 
                mutate( year = year - 4)


stg_3rd   <- Reduce( function(...)
                     full_join(...),
                     list(stg_t1, stg_t2, stg_t3, stg_t4) )

# Stage and size data

dorm_df <- full_join(siz_3rd, stg_3rd)

step1 <- subset(dorm_df, stage_t0 == 'plant' & 
                stage_t1 == 'dormant' & 
                stage_t2 == 'plant' )

step2 <- subset(dorm_df, stage_t0 == 'plant' & 
                stage_t1 == 'dormant' & 
                stage_t2 == 'dormant' &
                stage_t3 == 'plant')
 
step3 <- subset(dorm_df, stage_t0 == 'plant' & 
                stage_t1 == 'dormant' & 
                stage_t2 == 'dormant' &
                stage_t3 == 'dormant' & 
                stage_t4 == 'plant')
 

# models ---------------------------------------------
mod1 <- lm(log(Size_t1) ~ log(Size_t0), 
           data= mutate(dorm_df,  
                        Size_t1 = replace(Size_t1, 
                                          Size_t1 == 0,
                                          NA),
                        Size_t0 = replace(Size_t0, 
                                          Size_t0 == 0,
                                          NA) )
          )


mod2 <- lm(log(Size_t2) ~ log(Size_t0), 
           data= mutate(step1,  
                        Size_t2 = replace(Size_t2, 
                                          Size_t2 == 0,
                                          NA),
                        Size_t0 = replace(Size_t0, 
                                          Size_t0 == 0,
                                          NA) )
          )

mod3 <- lm(log(Size_t3) ~ log(Size_t0), data=step2)
mod4 <- lm(log(Size_t4) ~ log(Size_t0), data=step3)

coef(mod1)
coef(mod2)
coef(mod3)
coef(mod4)


# plots ----------------------------------------------------

tiff("C:/cloud/Dropbox/isotria_idiv/results/dormancy_size.tiff",
     unit="in", width=6.3, height=9, res=600,compression="lzw")

par(mfrow=c(3,2), mar = c(2.5,3,1.5,0.2),
    mgp = c(1.5,0.5,0) )
plot(log(Size_t1) ~ log(Size_t0), data=dorm_df,
     main = 'All data')
abline(0,1)
abline(mod1, col = 'red')

plot(log(Size_t2) ~ log(Size_t0), data=step1,
     ylim=c(0.5,4), xlim=c(0.5,4),
     main = 'one year dormancy')
abline(0,1)
abline(mod2, col = 'red')
legend( 'topleft',
        c('1:1 line', 'Regression slope'),
        col = c('black', 'red'), lty = 1,
        bty = 'n')

plot(log(Size_t3) ~ log(Size_t0), data=step2,
      ylim=c(0.5,4), xlim=c(0.5,4),
     main = 'two year dormancy')
abline(0,1)
abline(mod3, col = 'red')

plot(log(Size_t4) ~ log(Size_t0), data=step3,
     ylim=c(0.5,4), xlim=c(0.5,4),
     main = 'three year dormancy')
abline(0,1)
abline(mod4, col = 'red')

plot(log(Size_t1) ~ log(Size_t0), data=dorm_df,
     type='n', main = 'regression slopes')
abline(mod1, col = 'red', lwd=4)
abline(mod2, col = 'red', lwd=3)
abline(mod3, col = 'red', lwd=2)
abline(mod4, col = 'red', lwd=1)
legend( 'topleft',
        c( 'all data', 
           'one year dormancy',
           'two year dormancy',
           'three year dormancy'),
        lwd = c(4,3,2,1),
        col = rep('red',4),
        bty = 'n')

dev.off()


# check statistical significance --------------------------

# mod1 data
mod_d <- dorm_df %>% 
            mutate(Size_t1 = replace(Size_t1, 
                                     Size_t1 == 0,
                                     NA),
                   Size_t0 = replace(Size_t0, 
                                     Size_t0 == 0,
                                     NA),
                   model   = '0' ) %>% 
            select(Site,New_Tag,Habitat_Man,model,
                   Size_t0,Size_t1)

mod1_d    <- step1 %>% 
            mutate(Size_t2 = replace(Size_t2, 
                                     Size_t2 == 0,
                                     NA),
                   Size_t0 = replace(Size_t0, 
                                     Size_t0 == 0,
                                     NA),
                   model   = '1' ) %>% 
              select(Site,New_Tag,Habitat_Man,model,
                     Size_t0,Size_t2) %>% 
                rename( Size_t1 = Size_t2)

mod2_d      <- step2 %>% 
                  mutate( model = '2') %>% 
                  select(Site,New_Tag,Habitat_Man,model,
                         Size_t0,Size_t3) %>% 
                  rename( Size_t1 = Size_t3)

mod3_d      <- step3 %>% 
                  mutate( model = '3') %>% 
                  select(Site,New_Tag,Habitat_Man,model,
                         Size_t0,Size_t4) %>% 
                  rename( Size_t1 = Size_t4)


all_data_df <- Reduce(function(...) rbind(...),
                      list(mod_d,mod1_d,mod2_d,mod3_d) )
mod         <- lm( log(Size_t1) ~ log(Size_t0) + 
                   log(Size_t0):model, 
                   data=all_data_df )

write.csv(summary(mod)$coefficients,
          'C:/cloud/Dropbox/isotria_idiv/results/size_decrease.csv')

