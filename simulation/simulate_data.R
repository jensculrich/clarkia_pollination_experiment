## Simulate data for mixed-effects, logistic regression
#  started Dec. 14, 2022, J Ulrich

# Simulate binary data where: 
# the outcome is: 
# 0 - a flower is not pollen limited, OR
# 1 - a flower is pollen limited
# A random intercept for planter pot, nested in site will account for heterogeneity
# in nested sample groups.

###-----------------------------------------------------------------------------
## Global simulation options

n_sites = 40 # number of sites
N = 800 # number of paired flowers
alpha0 = -0.5 # global intercept for outcome 
sigma_alpha_site = 0.5 # variation across sites
beta = 1 # effect of site type on outcome

###-----------------------------------------------------------------------------
## Simulation function

simulate_data <- function(
  
  N = N,
  n_sites = n_sites,
  alpha0 = alpha0,
  sigma_alpha_site = sigma_alpha_site,
  beta = beta
  
){
  
  # Inverse logit functio
  inv_logit <- function(x) exp(x)/(1+exp(x))
  
  # Generate covariate data (site type)
  x = rep(c(0, 1), each = 0.5*N)
  
  # Generate site names
  n_data_per_site = N/n_sites
  
  sites <- rep(c(1:n_sites), each = n_data_per_site)
  
  ## site-specific random intercepts
  site_intercepts <- rnorm(n=n_sites, mean=0, sd=sigma_alpha_site)
  # site baseline success is drawn from a normal distribution with mean 0 and 
  # site specific variation defined by sigma_alpha_site
  
  alpha_site <- rep(site_intercepts[1:n_sites], 
                             each = n_data_per_site)
  
  # Generate probability that outcome is 1
  p = vector(length = n_sites)
  
  for(i in 1:N){
    
    p[i] = 
      alpha0 + # a global intercept
      alpha_site[i] + # a site-specific intercept adjustment
      beta * x[i] # plus an effect of site type
    
  }
  
  
  # Generate outcome data
  y = vector(length = N)
  
  for(i in 1:N){
    
    y[i] = rbinom(1, 1, prob=inv_logit(p[i])) 
    
  }
  
  ###-----------------------------------------------------------------------------
  ## Return stuff
  
  return(list(
    
    N = N, # number of pairs
    n_sites = n_sites, # number of sites
    sites = sites, # vector of site names
    x = x, # site type covariate data
    y = y # outcome data
    
  ))
  
} # end simulate_data() function


## --------------------------------------------------
### Simulate data
set.seed(3)
my_simulated_data <- simulate_data(N,
                                   n_sites,
                                   alpha0,
                                   sigma_alpha_site,
                                   beta)

## --------------------------------------------------
### Prepare data for model

# data to feed to the model
N <- my_simulated_data$N # number of pairs
n_sites <- my_simulated_data$n_sites # number of sites
sites <- as.numeric(my_simulated_data$sites) # vector of sites
x <- my_simulated_data$x # site type covariate
y <- my_simulated_data$y # outcome

stan_data <- c("N", "n_sites", "sites",
               "x", "y")

# Parameters monitored
params <- c("alpha0",
            "beta",
            "sigma_alpha_site"
)

parameter_value <- c(alpha0,
                     beta,
                     sigma_alpha_site
)

# MCMC settings
n_iterations <- 1600
n_thin <- 2
n_burnin <- 0.5*n_iterations
n_chains <- 3
n_cores <- n_chains

## Initial values
# given the number of parameters, the chains need some decent initial values
# otherwise sometimes they have a hard time starting to sample
inits <- lapply(1:n_chains, function(i)
  
  list(alpha0 = runif(1, -1, 1),
       sigma_alpha_site = runif(1, 0, 1),
       beta = runif(1, -1, 1)
       
  )
)

targets <- as.data.frame(cbind(params, parameter_value))

## --------------------------------------------------
### Run model
library(rstan)
stan_model <- "./models/logistic_model.stan"

## Call Stan from R
stan_out_sim <- stan(stan_model,
                     data = stan_data, 
                     init = inits, 
                     pars = params,
                     chains = n_chains, iter = n_iterations, 
                     warmup = n_burnin, thin = n_thin,
                     seed = 1,
                     open_progress = FALSE,
                     cores = n_cores,
                     control = list(adapt_delta = 0.9))

print(stan_out_sim, digits = 3)
View(targets)

## --------------------------------------------------
### Simple diagnostic plots

# traceplot
traceplot(stan_out_sim, pars = c(
  "alpha0",
  "beta",
  "sigma_alpha_site"
))

# pairs plot
pairs(stan_out_sim, pars = c(
  "alpha0",
  "beta",
  "sigma_alpha_site"
))

