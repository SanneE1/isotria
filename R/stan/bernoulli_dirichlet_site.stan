
data {
  int n;
  int n_time;
  int n_lag;
  int n_site;
  int<lower=0> year_i[n];
  int<lower=0> site_i[n];
  int<lower=0,upper=1> y[n];
  matrix[n_lag, n_time] clim;
}

parameters {
  simplex[n_lag] theta;
  real beta;
  real beta_site[n_site];
}

transformed parameters {
  vector[n] x;
  
  for(i in 1:n)
    x[i] = sum(theta .* clim[,year_i[i]]);
}

model {
  
  real m[n];
  int s_i;   // placeholder for site identity 
  
  for(ny in 1:n){
    s_i    = site_i[ny];
    m[ny]  = beta_site[s_i] + beta * x[ny];
  }
  y ~ bernoulli_logit(m);
}
