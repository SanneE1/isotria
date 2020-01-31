setwd("C:/cloud/MEGA/Teaching/Demography_iDiv_course/data")
library(tidyverse)
options(stringsAsFactors = F)

# name list
names_raw <- sample( c( rep("John",7),
                        rep("Meg",5), 
                        rep("Diana",4) 
                       ) 
                   )

replace(names_raw, names_raw == "Meg", NA)

# without pipes
un_n   <- sort( unique( names_raw ) )
un_n

# with pipes
un_n   <- names_raw %>%
            unique %>%
            sort
un_n

# Snow data
snow_d <- read.csv("ghcn_climate_portland.csv", skip = 15) %>%
            mutate( julian = as.POSIXlt(Day, format = "%Y-%m-%d")$yday ) %>%
            mutate( julian = replace(julian, julian > 200, julian[julian > 200]-366) ) %>%
            separate(Day, into = c("year", "month","day") ) %>%
            subset( year != "1940" & year != "2017") %>%
            mutate( Snow.Depth = replace(Snow.Depth, Snow.Depth=="M", NA )) %>%
            mutate( Snow.Depth = replace(Snow.Depth, Snow.Depth=="T", 1 )) %>%
            mutate( Snow.Depth = as.numeric(Snow.Depth) ) 


# disassemble and replace opuntia
o_dat <- read.csv("opuntia_20042015.csv")
o_yr  <- Filter(function(x) !is.na(x), opunt$Year_t %>% unique)

# disassemble data and introduce errors
d_04 <- subset(o_dat, Year_t == 2004) 
d_05 <- subset(o_dat, Year_t == 2005) 

tmp_04 <- d_04 %>%
    mutate( Height_t1 = as.character(Height_t1) ) %>%
    mutate( Height_t1 = replace(Height_t1, is.na(Height_t1), "na value") ) #%>%
    #write.csv( paste0("opuntia_",2004,".csv"), row.names = F )
tmp_05 <- d_05 %>%
  mutate( Height_t1 = as.character(Height_t1) ) %>%
  mutate( Height_t1 = replace(Height_t1, is.na(Height_t1), "na value") ) 

write.csv( paste0("opuntia_",2005,".csv"), row.names = F )

vol<-function(h,w,p){(1/3)*pi*h*(w/2)*(p/2)}

d_05 <- read.csv("opuntia_2005.csv") %>%
          mutate( Height_t1 = replace(Height_t1, Height_t1 == "na value", NA ) ) %>%
          mutate( Height_t1 = as.character(Height_t1) ) %>%
          mutate( Height_t1 = as.numeric(Height_t1) ) %>%
          mutate( Size_t  = vol(Height_t,Width_t,Perp_t),
                  Size_t1 = vol(Height_t1,Width_t1,Perp_t1) ) %>%
          select(-Height_t, -Width_t, -Perp_t, -Height_t1, -Width_t1, -Perp_t1, )


