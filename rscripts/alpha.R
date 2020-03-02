library(tidyverse)
library(rstan)
library(psych)
library(lavaan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(101)
n_chain = n_core = 3 # number of cores/chains
iterations = 10000

# Simulate data with known alpha
K = 5
N = 1000
alpha <- .7

(mod <- paste("f1=~", paste(paste0(by_item_alpha, "*", "x",1:K), collapse = " + ")))

y <- simulateData(mod, sample.nobs=N, standardized = F)

# Alpha
summary(alpha(y))

# Turn data into list
data <- list(y = y,
             K = K,
             N = N) ; str(data)

# Compile model
model <- stan(file = "stanin/alpha.stan", data=data, chains=0)

# Sample
m = stan(fit = model, 
         data = data,
         iter = iterations,
         warmup= iterations/2,
         chains = n_chain, 
         cores = n_core, 
         refresh = 5000,
         seed = 365)

# Get posterior and traceplots
pars <- c("mu", "sigma",  "mu_x", "sigma_x", "alpha")
print(m, pars=pars, probs=c(0.025,0.975))
traceplot(m, pars=pars)

# Check posterior of alpha value
rstan::extract(m, pars = "alpha") %>% 
  as.tibble() %>% 
  ggplot(aes(x = alpha)) + 
  geom_density() +
#  scale_x_continuous(limits = c(-1,1)) +
  ylab("Posterior density") +
  xlab( expression(paste("Cronbach's ", alpha))) +
#  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = alpha, linetype = "dashed") +
  annotate("text", y = 10, x = alpha, label = "True parameter\nvalue") +
  theme_minimal()

