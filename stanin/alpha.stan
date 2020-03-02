/* 
Cronbach's alpha
Author: Jens Roeser
Date: 02/03/20
*/
functions {
    real c_alpha(int K, real sigmaX, real[] sigmaY){
          real sigma2[K];
          real sigma_total;
          real sigma2_x = square(sigmaX);
          real alpha;

          for( i in 1:K){
            sigma2[i] = square(sigmaY[i]);
          }
          sigma_total = sum(sigma2);
  
          alpha = K/(K - 1.0) * (1.0 - (sigma_total / sigma2_x));  
        return alpha;
    }
}

data {
  int<lower = 1> N;
  int<lower = 1> K;
  vector[K] y[N];
}

transformed data{
  vector[N] x;
  for(n in 1:N){
    x[n] = sum(y[n,]);
  }
}

parameters {
  real mu[K];
  real<lower = 0> sigma[K];
  real mu_x;
  real<lower = 0> sigma_x;
}

transformed parameters{
  real alpha = c_alpha(K, sigma_x, sigma);
}


model {
  // priors
  mu ~ cauchy(0, 2.5);
  sigma ~ cauchy(0, 2.5);
  
  mu_x ~ cauchy(0, 2.5);
  sigma_x ~ cauchy(0, 2.5);
  
  for(k in 1:K){
    target += normal_lpdf(y[,k] | mu[k], sigma[k]);
  }
  target += normal_lpdf(x | mu_x, sigma_x);
}


