library(BIEN)
library(rgbif)
library(dplyr)
library(leaflet)
library(bbmle)
library(ggplot2)
options( stringsAsFactors = F )
source('R/sdm/plot_binned_prop.R')

# 8 data points in bien
# isotria <- BIEN_occurrence_species('Isotria medeoloides')

# out <- name_lookup(query='Isotria medeoloides')
# head(isotria)
# isotria %>% dim

# read county data
clim_df   <- read.csv('data/sdm/climate_means_counties.csv')
count_usa <- read.csv('data/sdm/coord_usa_counties.csv')
count_iso <- read.csv('data/sdm/isotria_counties.csv') %>% 
               mutate( county = gsub(' \\([0-9]{5}\\)','',county) ) %>% 
               mutate( county = trimws(county) ) %>% 
               mutate( state  = trimws(state) )
count_crd <- inner_join( count_iso, count_usa )


leaflet(data = count_crd ) %>% 
  addTiles() %>% 
  addCircleMarkers(~longitude, ~latitude)

# counties coords
# https://data.healthcare.gov/dataset/Geocodes-USA-with-Counties/52wv-g36k

# Isotria occurrence by county
# Found here: http://explorer.natureserve.org/servlet/NatureServe?searchName=Isotria+medeoloides

# Species Distribution Model --------------------------------------------

# model selection
mod_l <- list( 
  presence ~ map,
  presence ~ mat,
  presence ~ p_pet,
  presence ~ map + mat,
  presence ~ p_pet + mat,
  presence ~ map + I(map^2),
  presence ~ mat + I(mat^2),
  presence ~ p_pet + I(p_pet^2),
  presence ~ map * mat,
  presence ~ p_pet * mat,
  presence ~ p_pet * mat + I(p_pet^2),
  presence ~ p_pet + mat + I(p_pet^2)
)
  
mods <- lapply( mod_l, function(x) glm(x, data = clim_df, family = 'binomial') )
AICtab(mods, weights=T)

# best model! 
ppet2 <- mods[[8]]

bin_df <- plot_binned_prop(clim_df, 10, p_pet, presence)
x_seq <- seq( min(clim_df$p_pet,na.rm=T),
              max(clim_df$p_pet,na.rm=T),
              length.out = 100 )
lin_df <- data.frame( x = x_seq,
                      y = boot::inv.logit( coef(ppet2)[1] + 
                                             coef(ppet2)[2] * x_seq + 
                                             coef(ppet2)[3] * x_seq^2 ) 
                      )

ggplot( lin_df ) +
  geom_line( data = lin_df,
             aes( x, y ),
             lwd = 2,
             alpha = 0.5 ) +
  geom_point( data = bin_df, 
              aes( x, y ),
              size = 3 ) +
  theme_minimal() + 
  labs( x = 'Precipitation (mm) - PET ',
        y = 'Prop. of counties present'  ) +
  geom_vline( data = subset(clim_df, 
                            state == 'ME' & presence == 1 ),
              aes( xintercept = p_pet,
                   group      = county ) ) + 
  theme( axis.title = element_text( size = 20) ) +
  ggsave( 'results/sdm/presence_ppet.png',
           width = 6.3, height = 6.3 )

iso_ppet <- subset(clim_df, state == 'ME' & presence == 1 )$p_pet

subset(clim_df, presence == 1 )$p_pet %>% hist

subset(clim_df, p_pet < -400 ) %>% nrow

ggplot( subset(clim_df, presence == 1) ) +
  geom_histogram( aes(p_pet) ) +
  geom_vline( data = subset(clim_df, 
                          state == 'ME' & presence == 1 ),
            aes( xintercept = p_pet,
                 group      = county ) ) + 
  labs( x = 'Precipitation (mm) - PET ',
        y = 'Presence counts'  ) +
  theme_minimal() +
  theme( axis.title = element_text(size = 20)) +
  ggsave( 'results/sdm/presence_ppet_hist.png',
          width = 6.3, height = 6.3 )
