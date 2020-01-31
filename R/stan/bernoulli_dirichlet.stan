
data {
  int n;
  int n_time;
  int n_lag;
  int<lower=0> year_i[n];
  int<lower=0,upper=1> y[n];
  matrix[n_lag, n_time] clim;
}

parameters {
  simplex[n_lag] theta;
  real beta_c;
  real alpha;
}

transformed parameters {
  vector[n] x;
  
  for(i in 1:n)
    x[i] = sum(theta .* clim[,year_i[i]]);
}

model {
  
  real m[n];
  
  for(ny in 1:n){
    m[ny]  = alpha + beta_c * x[ny];
  }
  y ~ bernoulli_logit(m);
}

generated quantities {
  vector[n] log_lik;

  for(ny in 1:n)
    log_lik[ny] = bernoulli_logit_lpmf(y[ny] |alpha + beta_c * 
                                              sum(theta .* clim[,year_i[ny]]) );
}
