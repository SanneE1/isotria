
data {
  int n;
  int n_year;
  int<lower=0> year_i[n];
  real y[n];
  real size_t0[n];
}

parameters {
  real b0;
  real<lower=0> s_yr;

  real b_yr[n_year];
  real b_s;
  real<lower=0> s;
}

model {
  
  real m[n];
  int y_i;   // placeholder for year random effect
  
  // Hyperpriors
  b0    ~ normal(0,100);
  s_yr  ~ inv_gamma(0.001, 0.001);
  
  for(ii in 1:n_year){
    b_yr[ii] ~ normal(b0, s_yr);
  }

  for(ny in 1:n){
    y_i    = year_i[ny];
    m[ny]  = b_yr[y_i] + b_s * size_t0[ny];
  }
  y ~ normal(m,s);
}

generated quantities {
  vector[n] log_lik;

  for(ny in 1:n)
    log_lik[ny] = normal_lpdf(y[ny] |  b0 + b_s * size_t0[ny],s);
}
