# ISSUE INDIVIDUALS (gaps in Stage information), SOLVE: 454  497 5079 5080  170  240
library(dplyr)
library(tidyr)
library(readxl)
library(testthat)

istoria <- read_excel("data/demoallnew.xlsx") %>%
              mutate( key = paste( Site, Habitat_Man,sep="_") ) %>% 
              mutate( key = paste( key, New_Tag, sep="_") )
        
# vital rates in long form
long_form <- function(var, d){

  # extract variable names you need  
  var_names <-  grep(var, names(istoria), value = T)
  
  # format in long form
  dplyr::select(d, Site, New_Tag, Habitat_Man, var_names) %>% 
    gather_("year", paste0("value_",var), var_names) %>%
    mutate( year = gsub('[a-zA-Z]','',year ) ) %>%
    mutate( year = as.numeric(year) ) %>%
    setNames( gsub('value_', "", names(.)) )
  
}

# separate stage information
vr_l      <- lapply( c('Stage','Flower','Fruit','Size'), 
                     long_form, istoria )

# format size information
vr_l[[5]] <- vr_l[[4]] %>% rename( Size_t1 = Size )
vr_l[[4]] <- vr_l[[4]] %>% rename( Size_t0 = Size ) %>%
              mutate( year = year + 1 )
vr_l[[4]] <- inner_join(vr_l[[4]], vr_l[[5]])

# format stage information
vr_l[[5]] <- vr_l[[1]] %>% rename( Stage_t1 = Stage )
vr_l[[1]] <- vr_l[[1]] %>% rename( Stage_t0 = Stage ) %>%
                mutate( year = year + 1 )
vr_l[[1]] <- inner_join(vr_l[[1]], vr_l[[5]]) %>% 
                mutate( dormancy_t1 = NA ) %>% 
                # dormant plants
                mutate( dormancy_t1 = replace(dormancy_t1,
                                              Stage_t0 == 'plant' &
                                              Stage_t1 == 'dormant',
                                              1) ) %>% 
                # not dormant
                mutate( dormancy_t1 = replace(dormancy_t1,
                                              Stage_t0 == 'plant' &
                                              Stage_t1 == 'plant',
                                              0) ) %>% 
                # not dormant 2
                mutate( dormancy_t1 = replace(dormancy_t1,
                                              Stage_t0 == 'dormant' &
                                              Stage_t1 == 'plant',
                                              0) )


# non survival data
non_surv   <- Reduce(function(...) merge(...), vr_l[1:4]) %>%
                # remove lines comprised only of NAs
                subset( !(is.na(Stage_t0) & 
                          is.na(Flower) & 
                          is.na(Fruit) & 
                          is.na(Size_t0) & 
                          is.na(Size_t1) & 
                          is.na(dormancy_t1) ) ) %>%
                arrange( year ) %>%
                rename( year_t1 = year ) 


# create survival info -----------------------------------------------------------------

# 'one timers': they only have ONE data point 
one_timers <- non_surv %>% 
                group_by(New_Tag) %>% 
                summarise( rep = n() ) %>% 
                subset( rep == 1) %>%
                as.data.frame %>%
                .[,'New_Tag']

# isolate survival-relevant data
surv       <- non_surv %>% 
                dplyr::select(New_Tag, year_t1, Stage_t1 ) %>%
                unique %>%
                arrange(New_Tag, year_t1) %>%
                subset( !(New_Tag %in% one_timers) )

# find survival relevant data
new_tags   <- surv$New_Tag %>% unique 

# calculate survival
survival_find <- function(ii){
  
  out   <- subset(surv, New_Tag == ii) %>%
              subset( !is.na(Stage_t1) ) %>%
              arrange( year_t1 ) %>%
              # dplyr::select(New_Tag, year_t1, Stage_t1) %>%
              mutate( surv = NA )
  
  if( nrow(out) == 1 ) return( NULL )
  
  # identify dormant individuals
  vec1  <- (out[,'Stage_t1'] == 'dormant') %>% as.numeric
  vec2  <- vec1
  
  # count how many consecutive dormanies we have in a row 
  for( vi in 2:length(vec1) ){
    if( vec1[vi] == 0){
      vec2[vi] <- 0
    }else{
      vec2[vi] <- vec2[vi-1] + vec1[vi]
    }
  }
  
  # update survival column
  if( any(vec2 == 4) ){
    # index position for death
    death_i <- (which( vec2 == 4 ) - 3) %>% first
    out     <- out[1:death_i,] %>% 
                  mutate( surv = 1) %>%
                  mutate( surv = replace(surv, death_i, 0) )
  }else{
    out <- mutate(out, surv = 1)
  }
  
  # check for potential double-deaths
  #if( sum(vec2 == 4) == 2 ) stop( 'Error: Double death')
  
  return( out )
  
}

# check double deaths
double_deaths <- function(ii){
  
  out   <- subset(surv, New_Tag == ii) %>%
              subset( !is.na(Stage_t1) ) %>%
              arrange( year_t1 ) %>%
              # dplyr::select(New_Tag, year_t1, Stage_t1) %>%
              mutate( surv = NA )
  
  if( nrow(out) == 1 ) return( NULL )
  
  # identify dormant individuals
  vec1  <- (out[,'Stage_t1'] == 'dormant') %>% as.numeric
  vec2  <- vec1
  
  # count how many consecutive dormanies we have in a row 
  for( vi in 2:length(vec1) ){
    if( vec1[vi] == 0){
      vec2[vi] <- 0
    }else{
      vec2[vi] <- vec2[vi-1] + vec1[vi]
    }
  }
  
  if(sum(vec2 == 4) > 1) return(ii)
  
}

# survival data
surv_df <- lapply(new_tags, survival_find ) %>%
              Reduce(function(...) rbind(...), .) %>%
              arrange(New_Tag, year_t1, Stage_t1)

# double-deaths (what happened here?)
doble_death_ids <- lapply( new_tags, double_deaths ) %>% unlist

# vital rates ---------------------------------------------------------------
vr      <- left_join( non_surv, surv_df ) %>%
              setNames( c('Site', 'New_Tag', 'Habitat_Man', 'year_t1', 
                          'stage_t0', 'stage_t1','dormancy_t1',
                          'n_flower_t1', 'n_fruit_t1', 
                          'size_t0', 'size_t1', 'surv_t1') ) %>% 
              # introduce flowering information
              mutate( flower_t1 = n_flower_t1 ) %>% 
              mutate( flower_t1 = replace(flower_t1, n_flower_t1 > 0,  1),
                      flower_t1 = replace(flower_t1, n_flower_t1 == 0, 0) ) %>% 
              # remove dormant individuals which actually died
              # (they entered dormancy but stayed dormant for 3 years)
              mutate( dormancy_t1 = replace(dormancy_t1, surv_t1 == 0, NA) )

# tests -----------------------------------------------------------------------------

# only one test at this point
expect_equal(vr$n_flower_t1 %>% is.na %>% sum,
             vr$flower_t1 %>% is.na %>% sum)

# PROBLEM: why can dormancy correspond to dead and  can dormant plants be associated with site_t0 NA?
subset(vr, dormancy_t1 == 1 & surv_t1 == 0) %>% .$New_Tag


# how can dormant plants be associated with site_t0 NA?
subset(vr, dormancy_t1 == 1 & !is.na(size_t1))

# How can n_fruit_t1 be == 2 but n_flower_t1 NA?!?
subset(vr, n_fruit_t1 == 2) %>% head

# 633: In 1989 individual goes dormant, but we have size information. Is this a mistake?
# 170: In 1998 individual has 2 fruits, but no flower.
# 292: In 1999 individual has 2 fruits, but no flower.


# Visual checks ----------------------------------------------------------------------------
par(mfrow = c(3,2) )
plot( jitter(surv_t1) ~ log(size_t0), data=subset(vr, !is.na(surv_t1)) )
plot( log(size_t1) ~ log(size_t0), data = vr )
plot( jitter(flower_t1) ~ log(size_t1), data = vr )
plot( n_flower_t1 ~ log(size_t1), data = vr )
plot( n_fruit_t1 ~ n_flower_t1, data = vr )
plot( jitter(dormancy_t1) ~ log(size_t0), data = vr )

# visual checks to find mistakes
par(mfrow = c(2,1) )
plot( jitter(surv_t1) ~ log(size_t1), data=subset(vr, !is.na(surv_t1)) )
plot( jitter(dormancy_t1) ~ log(size_t1), data = vr )


write.csv(vr, 'istoria_long.csv', row.names = F)



# trobleshoot --------------------------------------------------------------------------

# individuals which only appear in one stage
one_stage <- non_surv %>% 
                dplyr::select(New_Tag, Stage) %>% 
                unique %>%
                arrange(New_Tag) %>%
                group_by(New_Tag) %>% 
                summarise( rep = n()) %>%
                subset( rep == 1) %>% 
                .[,'New_Tag']

# only appears as dormant (in non_surv)
only_dorm <- non_surv %>% 
                dplyr::select(New_Tag, Stage) %>% 
                subset( New_Tag %in% one_stage$New_Tag) %>%
                unique %>% 
                subset( Stage == 'dormant') %>%
                as.data.frame %>%
                .[,'New_Tag']

# only appears as plant (in non_surv)
only_plant<- non_surv %>% 
                dplyr::select(New_Tag, Stage) %>% 
                subset( New_Tag %in% one_stage$New_Tag) %>%
                unique %>% 
                subset( Stage == 'plant' ) %>%
                as.data.frame %>%
                .[,'New_Tag']




# istoria_stage_long
ist_st_long <- dplyr::select(istoria, Site, New_Tag, Habitat_Man, var_names) %>% 
                  gather(year, stage, var_names) %>%
                  mutate( year = gsub('[a-zA-Z]','',year ) ) %>%
                  mutate( year = as.numeric(year) ) %>%
                  arrange(New_Tag)

# New_Tags with 15 dormant stages 
only_dorm_15 <- ist_st_long %>% 
                  dplyr::select(New_Tag, year, stage) %>%
                  subset( stage == 'dormant' ) %>%
                  group_by(New_Tag) %>%
                  summarise( rep = n() ) %>%
                  subset( rep == 15) %>%
                  .[,'New_Tag']

# Tags for which indiv. is dormant for 15 years
# These do not appear in my vr object
only_dorm_15 <- ist_st_long %>% 
                  dplyr::select(New_Tag, year, stage) %>%
                  subset( stage == 'dormant' ) %>%
                  group_by(New_Tag) %>%
                  summarise( rep = n() ) %>%
                  subset( rep == 15) %>%
                  .[,'New_Tag']

# Only one stage
id_by_stage  <- ist_st_long %>% 
                  dplyr::select(New_Tag, year, stage) %>%
                  group_by(New_Tag, stage) %>%
                  summarise( rep = n() ) %>%
                  ungroup 

# only one stage
only_one_st    <- subset(id_by_stage, !is.na(stage) ) %>%
                    dplyr::select( -rep ) %>%
                    group_by(New_Tag) %>% 
                    summarise( rep = n() ) %>%
                    subset( rep == 1 ) %>% 
                    as.data.frame %>% 
                    .[,'New_Tag']
                    
# only dormants 
only_dorm_id   <- subset(id_by_stage, New_Tag %in% only_one_st) %>% 
                    subset( stage == 'dormant') %>% 
                    as.data.frame %>% 
                    .[,'New_Tag']

# only plants 
only_plant_id  <- subset(id_by_stage, New_Tag %in% only_one_st) %>% 
                    subset( stage == 'plant') %>% 
                    as.data.frame %>% 
                    .[,'New_Tag']

only_plant_id
subset(vr, New_Tag == 5080)
subset(istoria, New_Tag == 5080) %>% as.data.frame


subset(vr, Stage == 'plant')$year_t1 %>% unique

miss_only_plant <- only_plant_id[which( !(only_plant_id %in% unique(vr$New_Tag)) )]
subset(vr, New_Tag == 194)



missing_ids    <- c(only_dorm_15, only_dorm_id)






# only plants
only_dorm     <- subset(id_by_stage, New_Tag %in% only_one_st & stage == 'plant')


                  subset( !is.na(stage) ) %>%
                  dplyr::select( -rep ) %>%
                  group_by( New_Tag ) %>%
                  summarise( rep = n() ) %>%
                  subset( rep == 1 ) %>%
                  as.data.frame %>%
                  .[,'New_Tag']

# 
subset(ist_st_long, New_Tag %in% only_one)


subset(vr, New_Tag == 335)
subset(istoria, New_Tag == 335) %>% as.data.frame



# get stage in long form
long_stage <- function(ii){
  
  # extract variable names you need  
  var_names <-  grep("Stage", names(istoria), value = T)
  
  # format in long form
  dplyr::select(istoria, Site, New_Tag, Habitat_Man, var_names) %>% 
    subset( New_Tag == ii ) %>%
    gather(year, stage, var_names) %>%
    mutate( year = gsub('[a-zA-Z]','',year ) ) %>%
    mutate( year = as.numeric(year) ) %>%
    setNames( gsub('value_', "", names(.)) )
  
}

# long form plant/dormant only
dorm_only_l   <- lapply( only_dorm,  long_stage )
sapply(dorm_only_l,  function(x) return(sum(x$stage=='dormant',na.rm=T) != 15))

subset(istoria, New_Tag == only_dorm[3]) %>%
  as.data.frame %>% 
  unique 


subset(vr, New_Tag == 315 )

plant_only_l  <- lapply( only_plant, long_stage )

# 
dorm_only_df <- data.frame( New_Tag = only_dorm[sapply(dorm_only_l,  function(x) return(sum(x$stage=='dormant',na.rm=T) != 15))] ) %>%
                  arrange( New_Tag )
dorm_only_df
 

only_dorm[14]



subset(vr, New_Tag == only_dorm[2])

plant_only_l[[6]]
  
subset(vr, New_Tag == 196)



subset(vr, New_Tag == only_plant[5])




# checks -------------------------------------------------------------------------------

# get one timers with ONLY DORMANT STAGE
vr %>% subset( (New_Tag %in% one_timers) & Stage == 'dormant' )
# individuals with Fruit == 2
vr %>% subset( Fruit == 2 )
# individuals without Stage information
vr %>% subset( is.na(Stage) ) 



istoria %>% subset( New_Tag == 170) %>% as.data.frame

# test what do one timers  
istoria %>% 
  subset( New_Tag %in% one_timers ) %>%
  subset( )
  as.data.frame

# plots
non_surv %>%
  dplyr::select(Site, New_Tag, Habitat_Man) %>% 
  unique %>%
  group_by(New_Tag, Habitat_Man) %>% 
  summarise( rep = n() ) %>% 
  as.data.frame %>% .[,3] %>% unique


# plots
non_surv %>%
  dplyr::select(year_t1, Site) %>% 
  unique %>%
  group_by(year_t1) %>% 
  summarise( rep= n() )

# Why several individuals have size 0?
# Fruit and Flower are at time 0: correct?
non_surv %>%
  dplyr::select(Site, Habitat_Man) %>% 
  unique 
