library(dplyr)
library(ggplot2)

isotria_long <- read_csv("data/isotria_long.csv")

View(isotria_long)
 
library("ggplot2", lib.loc="~/R/win-library/3.5")

 
 # log(size_t0) vs log(size_t1)
 #general function
 ggplot(isotria_long,aes(log(size_t0),log(size_t1)))+
   geom_point()
 
 
 # devided by year
 t<-ggplot(isotria_long,aes(log(size_t0),log(size_t1)))+
   geom_point()
 plot(t)
 # facet_wrap
 t+facet_wrap(isotria_long$year_t1~.)+
   ggtitle("growth (log)")+
 ggsave("growth(log).png")
 
 
 # function by sanne

logitbin_df <- function (df, resp, xvar, ..., n =100, log_trans_xvar = FALSE) {
  
  resp <- enquo(resp)
  xvar <- enquo(xvar)
  
  if (log_trans_xvar == TRUE) {
    
    if (length(filter(df, !! xvar <= 0)[,1] >= 1 )) {
      warning("Transformed data contains values of =< 0")
    }
    
    df <- df %>%
      filter(!! xvar > 0) %>%
      mutate(!! xvar := log(!! xvar))
    
  }
  
  df <- df %>%
    group_by(...) %>%
    mutate(bingroup = cut(!! xvar, breaks = n))
  
  df <- df %>%
    group_by(..., bingroup) %>%
    summarize(!! resp := mean(!! resp), !! xvar := mean(!! xvar))
  
  return(df)
}

logitbin_df(df = isotria_long, resp = surv_t1, xvar = size_t0, log_trans_xvar = T)
logitbin_df(df = isotria_long, resp = surv_t1, xvar = size_t0, year_t1)

# eliminate NA
a <-isotria_long %>% filter(!is.na(size_t0))
#put in funktion

# survival log

logitbin_df(df = a, resp = surv_t1, xvar = size_t0, year_t1, log_trans_xvar = T)

survl <- logitbin_df(df = a, resp = surv_t1, xvar = size_t0, year_t1, n = 25)
survl
#general function
ggplot(survl,aes(size_t0,surv_t1))+
  geom_point()

t<-ggplot(survl,aes(size_t0,surv_t1))+
  geom_point()
plot(t)
t+facet_wrap(survl$year_t1~.)+
  ggtitle("survival(log)")+
ggsave("survival(log).png")


# n_flower log
logitbin_df(df = a, resp = n_flower_t1, xvar = size_t0, year_t1, log_trans_xvar = T)
logitbin_df(df = a, resp = n_flower_t1, xvar = size_t0, year_t1)
# eliminate the ones that did not flower
b<-a%>%filter(flower_t1==1)
n_fll <- logitbin_df(df = b, resp = n_flower_t1, xvar = size_t0, year_t1, n = 25)
n_fll
#general function
ggplot(n_fll,aes(size_t0,n_flower_t1))+
  geom_point()

t<-ggplot(n_fll,aes(size_t0,n_flower_t1))+
  geom_point()
plot(t)
t+facet_wrap(n_fll$year_t1~.)+
  ggtitle("flowernumber(log)")+
ggsave("flowernumber(log).png")

#n_fruit log
logitbin_df(df = a, resp = n_fruit_t1, xvar = size_t0, year_t1, log_trans_xvar = T)
logitbin_df(df = b, resp = n_fruit_t1, xvar = size_t0, year_t1)

n_frl<-logitbin_df(df = b, resp = n_fruit_t1, xvar = size_t0, year_t1, n = 25)
n_frl

ggplot(n_frl,aes(size_t0,n_fruit_t1))+
  geom_point()

t<-ggplot(n_frl,aes(size_t0,n_fruit_t1))+
  geom_point()
plot(t)
t+facet_wrap(n_frl$year_t1~.)+
  ggtitle("fruitnumber(log)")
ggsave("fruitnumber(log).png")


#dormancy log
logitbin_df(df = a, resp = dormancy_t1, xvar = size_t0, year_t1, log_trans_xvar = T)
logitbin_df(df = a, resp = dormancy_t1, xvar = size_t0, year_t1)

dorml<-logitbin_df(df =a , resp = dormancy_t1, xvar = size_t0, year_t1, n = 25)
dorml

ggplot(dorml,aes(size_t0,dormancy_t1))+
  geom_point()

t<-ggplot(dorml,aes(size_t0,dormancy_t1))+
  geom_point()
plot(t)
t+facet_wrap(dorml$year_t1~.)+
  ggtitle("dormancy (log)")
ggsave("dormancy(log).png")


#flower prop
logitbin_df(df = a, resp = flower_t1, xvar = size_t0, year_t1)

flow<-logitbin_df(df =a , resp = flower_t1, xvar = size_t0, year_t1, n = 25)
flow

ggplot(flow,aes(size_t0,flower_t1))+
  geom_point()

t<-ggplot(flow,aes(size_t0,flower_t1))+
  geom_point()
plot(t)
t+facet_wrap(flow$year_t1~.)+
  ggtitle("")

# flowerlog
logitbin_df(df = a, resp = flower_t1, xvar = size_t0, year_t1, log_trans_xvar = T)
logitbin_df(df = a, resp = flower_t1, xvar = size_t0, year_t1)

flowl<-logitbin_df(df =a , resp = flower_t1, xvar = size_t0, year_t1, n = 25)
flowl

ggplot(flowl,aes(size_t0,flower_t1))+
  geom_point()

t<-ggplot(flowl,aes(size_t0,flower_t1))+
  geom_point()
plot(t)
t+facet_wrap(flowl$year_t1~.)+
  ggtitle("log")

# histogramm for new plants (plants taht were NA in t0 an are plants in t1 )
w <-isotria_long %>% filter(is.na(stage_t0) & stage_t1 == 'plant') 

head(w)
ggplot(w,aes(log(size_t1)))+
geom_histogram()+
ggtitle('New Plants')
ggsave("Newplants.png")


# histogramm for plants that go from dormancy to plant
k<- isotria_long %>% filter(stage_t0=='dormant')
l<-k %>% filter(stage_t1=='plant')
head(l)
ggplot(l,aes(log(size_t1)))+
  geom_histogram()+
  ggtitle('Woke Plants')
ggsave("Wokeplants.png")


#fruits and new recrits
# dataframe for the new situation
#for fruits
e <-isotria_long %>% 
  select(Site,year_t1,n_fruit_t1) %>%
  group_by(Site, year_t1) %>%
  summarise(n_fruit_t0 = sum(n_fruit_t1, na.rm = T))

e$year_t1 <- e$year_t1 + 1


v <- w %>%
  group_by(Site, year_t1) %>%
  summarise(n_rec = n())


#frame
fru<-full_join(e,v)
fru

ggplot(fru,aes((n_fruit_t0),(n_rec)))+
  geom_point()+
  ggtitle('reproduktion')
ggsave('reproduktion.png')  








