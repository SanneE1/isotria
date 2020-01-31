binned_prop <- function(y,x,df){
    
  #graph 
  U <- max(df[,x]) 
  L <- min(df[,x])
  
  n   <- 15
  h   <- (U-L)/n                   # Bin size
  lb  <- L+c(0:n)*h                # Lower boundaries of bins 
  up  <- (L+h)+c(0:n)*h            # Upper boundaries of bins 
  y   <- 0.5*(lb[1:n]+lb[2:(n+1)])   # Bins' midpoints
  
  (y - h*0.5)[1]
  
  # bin prob of survival
  bin_prop <- function(midpoint,h){
    
    lower  <- midpoint - h*0.5
    upper  <- midpoint + h*0.5
    
    sub_df <- df %>% 
                subset( df$[,x] >= lower & df$[,x] < upper)  
    
    data.frame( x = midpoint,
                y = sum(sub_df$surv_t1)/nrow(sub_df),
                stringsAsFactors = T)
  
  }
  
  # extend upper bound to include observed maximum extreme
  y[length(y)] = y[length(y)] + 0.1
  
  # create dataframe with bins
  lapply(y, bin_prop, h) %>% 
    Reduce(function(...) bind_rows(...), .)
  
}