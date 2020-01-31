
data {
  int n;
  int<lower=0,upper=1> y[n];
  real size_t0[n];
}

parameters {
  real beta_s;
  real alpha;
}

model {
  real m[n];
  
  for(ny in 1:n){
    m[ny]  = alpha + beta_s * size_t0[ny];
  }
  y ~ bernoulli_logit(m);
}

generated quantities {
  vector[n] log_lik;

  for(ny in 1:n)
    log_lik[ny] = bernoulli_logit_lpmf(y[ny] |  alpha + beta_s * size_t0[ny]);
}
