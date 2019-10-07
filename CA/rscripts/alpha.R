library(tidyverse)
library(rstan)
library(psy)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(101)
n_chain = n_core = 3 # number of cores/chains
iterations = 6000

K = 5
N = 1000
y <- matrix( nrow=N, ncol=K, byrow=T) 
for(i in 1:N){
  y[i,] <- sample(1:K, K, prob = rep(.1,K), replace=T)
}
range(y)
cronbach(y)$alpha

(itemvars = apply(X = y,MARGIN = 2,FUN = var))
(tscores = rowSums(y))
(nitems = K)
(var_x = var(tscores))
(mean(tscores))
(Calpha = nitems/(nitems-1) * (1 - sum(itemvars) / var_x))

data <- list(y = y,
             K = K,
             likert_min = 1,
             likert_max = 5,
             N = N) ; str(data)

model <- stan(file = "stanin/alpha.stan", data=data, chains=0)

m = stan(fit = model, 
         data = data,
         iter = iterations,
         warmup= iterations/2,
         chains = n_chain, 
         cores = n_core, 
         refresh = 100,
         seed = 365,
         control = list(adapt_delta = 0.99,
                        max_treedepth = 16))

pars <- c("mu", "sigma",  "mu_x", "sigma_x", "alpha")
print(m, pars=pars, probs=c(0.025,0.975))
traceplot(m, pars=pars)

rstan::extract(m, pars = "alpha") %>% as.tibble() %>% ggplot(aes(x = alpha)) + 
  geom_density() +
  scale_x_continuous(limits = c(-1,1)) +
  ylab("Posterior density") +
  xlab( expression(paste("Cronbach's ",alpha))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal()

