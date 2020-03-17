# ISSUE INDIVIDUALS (gaps in Stage information), SOLVE: 454  497 5079 5080  170  240
library(plyr); library(dplyr)
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
  
  # dplyr::select(d, Site, New_Tag, Habitat_Man, starts_with(var)) %>% 
  #      pivot_longer(cols = starts_with(var), names_to = "year", values_to = var, values_drop_na = TRUE) %>% 
  #      mutate( year = gsub('[a-zA-Z]','',year ) ) %>% 
  #      mutate( year = as.numeric(year) )
  
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
vr_l[[1]] <- left_join(vr_l[[1]], vr_l[[5]]) %>% 
  mutate( dormancy_t1 = NA,
          remain_dorm_t1 = NA) %>% 
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
  mutate( dormancy_t1 = replace(remain_dorm_t1,
                                Stage_t0 == 'dormant' &
                                  Stage_t1 == 'plant',
                                0) ) %>%
  mutate( remain_dorm_t1 = replace(remain_dorm_t1,
                                   Stage_t0 == 'dormant' &
                                     Stage_t1 == 'dormant',
                                   1))


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
survival_find <- function(ii, find_long_dorm = FALSE) {
  
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
    } else {
      vec2[vi] <- vec2[vi-1] + vec1[vi]
    }
  }
  
  if(find_long_dorm == TRUE) {
    m <- cbind(out, vec2, next_vec = c(vec2[2:length(vec2)], NA))
    come_back <- m[which(m$vec2 >3 & m$next_vec == 0),] %>%
      select("New_Tag", "year_t1", "vec2") %>%
      rename("dorm_duration" = "vec2")
    
    return(come_back)
    
  } else {
    
    # update survival column
    if( any(vec2 == 4) ){
      # index position for death
      death_i <- (which( vec2 == 4 ) - 3) %>% first
      out     <- out[1:death_i,] %>% 
        mutate( surv = 1) %>%
        mutate( surv = replace(surv, death_i, 0) )
    } else {
      out <- mutate(out, surv = 1)
    }
    
  }
  
  return(out)
  
} 


# survival data ---- Right now 2nd life is not included ----------------
surv_df <- lapply(new_tags, survival_find ) %>%
  Reduce(function(...) rbind(...), .) %>%
  arrange(New_Tag, year_t1, Stage_t1)


# vital rates ---------------------------------------------------------------
vr      <- left_join( non_surv, surv_df ) %>%
  setNames( c('Site', 'New_Tag', 'Habitat_Man', 'year_t1', 
              'stage_t0', 'stage_t1','dormancy_t1', 'remain_dorm_t1',
              'n_flower_t1', 'n_fruit_t1', 
              'size_t0', 'size_t1', 'surv_t1') ) %>% 
  # introduce flowering information
  mutate( flower_t1 = n_flower_t1 ) %>% 
  mutate( flower_t1 = replace(flower_t1, n_flower_t1 > 0,  1),
          flower_t1 = replace(flower_t1, n_flower_t1 == 0, 0) ) %>% 
  # remove dormant individuals which actually died
  # (they entered dormancy but stayed dormant for 3 years)
  mutate( dormancy_t1 = replace(dormancy_t1, surv_t1 == 0, NA) )


# Visual checks ----------------------------------------------------------------------------
par(mfrow = c(3,2) )
plot( surv_t1 ~ size_t0, data= logitbin_df(subset(vr, !is.na(surv_t1) && !is.na(size_t0)), surv_t1, size_t0, n= 500, log_trans_xvar = TRUE))
plot( log(size_t1) ~ log(size_t0), data = vr )
plot( flower_t1 ~ size_t1, data = logitbin_df(vr, flower_t1, size_t1, log_trans_xvar = TRUE))
plot( n_flower_t1 ~ log(size_t1), data = vr )
plot( n_fruit_t1 ~ n_flower_t1, data = vr )
plot( dormancy_t1 ~ size_t0, data = logitbin_df(vr, dormancy_t1, size_t0, log_trans_xvar = TRUE ))

# visual checks to find mistakes
par(mfrow = c(2,1) )
plot( jitter(surv_t1) ~ log(size_t1), data=subset(vr, !is.na(surv_t1)) )
plot( jitter(dormancy_t1) ~ log(size_t1), data = vr )



write.csv(vr, 'data/istoria_long.csv', row.names = F)



# trobleshoot --------------------------------------------------------------------------

#individuals come back after long dormancy (+3 years) -----------
come_back <- lapply(new_tags, function(x) survival_find(x, find_long_dorm = TRUE)) %>%
  Reduce(function(...) rbind(...), .) %>%
  arrange(New_Tag, year_t1)

# missing size information even though the plant is not dormant and does survive (and sometimes flower)
missing_size <- subset(vr, surv_t1 == 1 & is.na(size_t1) & stage_t1 == "plant") %>%
  select(Site, New_Tag, year_t1, stage_t1, size_t1) %>%
  arrange(New_Tag, year_t1)

# survival= 1 and not dormant but size_t0 was 0
size_0 <- subset(vr, stage_t1 == "plant" & size_t1 == 0) %>%
  select(Site, New_Tag, year_t1, stage_t1, size_t1) %>%
  arrange(New_Tag, year_t1)

# how can dormant plants be associated with size_t0 NA?
dormant_size <- subset(vr, dormancy_t1 == 1 & !is.na(size_t1)) %>%
  select(Site, New_Tag, year_t1, stage_t1, size_t1) %>%
  arrange(New_Tag, year_t1)

# How can n_fruit_t1 be == 2 but n_flower_t1 NA?!?
fruit_noflower <- subset(vr, n_fruit_t1 == 2) %>%
  select(Site, New_Tag, year_t1, stage_t1, size_t1, flower_t1, n_flower_t1, n_fruit_t1) %>%
  arrange(New_Tag, year_t1)

# 633: In 1989 individual goes dormant, but we have size information. Is this a mistake?
# 170: In 1998 individual has 2 fruits, but no flower.
# 292: In 1999 individual has 2 fruits, but no flower.

# Export issues

xlsx::write.xlsx(come_back, file = "Formatting_issues.xlsx", sheetName = "ComeBack", row.names = FALSE)
xlsx::write.xlsx(missing_size, file = "Formatting_issues.xlsx", sheetName = "MissingSize", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(size_0, file = "Formatting_issues.xlsx", sheetName = "ZeroSize", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(dormant_size, file = "Formatting_issues.xlsx", sheetName = "DormSize", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(fruit_noflower, file = "Formatting_issues.xlsx", sheetName = "FruitNoFlwr", row.names = FALSE, append = TRUE)

