setwd("C:/cloud/Dropbox/isotria_idiv/data")
library(tidyverse)

# snow data
d <- read.csv("UCC_ghcn_USW00014764_2017_10_25_1508924854.csv", skip = 15,
        stringsAsFactors = F) %>%
        mutate( julian = as.POSIXlt(Day, format = "%Y-%m-%d")$yday ) %>%
        mutate( julian = replace(julian, julian > 200, julian[julian > 200]-366) ) %>%
        separate(Day, into = c("year", "month","day") ) %>%
        subset( year != "1940" & year != "2017") %>%
        mutate( Snow.Depth = replace(Snow.Depth, Snow.Depth=="M", NA )) %>%
        mutate( Snow.Depth = replace(Snow.Depth, Snow.Depth=="T", 1 )) %>%
        mutate( Snow.Depth = as.numeric(Snow.Depth) ) 

# snow climatology
snow_d    <- d %>% group_by(julian) %>% summarise( mean_depth = mean(Snow.Depth,na.rm=T),
                                                   max_depth = max(Snow.Depth,na.rm=T),
                                                   min_depth = min(Snow.Depth,na.rm=T),
                                                   sd_depth = sd(Snow.Depth,na.rm=T) )
snow_pa   <- d %>% group_by(julian) %>% summarise( snow_pres = sum(Snow.Depth > 0,na.rm=T),
                                                   snow_30   = sum(Snow.Depth > 30,na.rm=T) )
snow_st   <- full_join( snow_d, snow_pa ) %>%
                mutate( snow_pres = snow_pres / 76,
                        snow_30   = snow_pres / 76)

plot(mean_depth ~ julian , data = snow_st, pch = 16)
plot(sd_depth ~ julian , data = snow_st, pch = 16)
plot(snow_pres ~ julian , data = snow_st, pch = 16,
     ylab = "Probability of snow present")
plot(snow_30 ~ julian , data = snow_st, pch = 16,
     ylab = "Probability of snow above 30cm")


# beginning and end of snow season
subset(d, Snow.Depth > 0)$julian %>% min # October9th
subset(d, Snow.Depth > 0)$julian %>% max # May 9th (!!!)

# I'd say Nov-April?


