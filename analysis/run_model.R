## Analyze real data using a mixed-effects, logistic regression
#  started Dec. 14, 2022, J Ulrich

# Format binary outcome data where.. 
# the outcome is: 
# 0 - a flower is not pollen limited, OR
# 1 - a flower is pollen limited
# A random intercept for planter pot, nested in site will account for heterogeneity
# in nested sample groups.

###-----------------------------------------------------------------------------
## Global options
max_PL_accepted = 2
PL_response_threshold = 0.5

source("./analysis/prep_data.R")
my_data <- prep_data(max_PL_accepted,
                     PL_response_threshold)


# data to feed to the model
N <- my_data$N # number of pairs
n_pots <- my_data$n_pots # number of pots
pot_ID <- as.numeric(as.factor(my_data$pots)) # pot ID names
n_sites <- my_data$n_sites # number of sites
site_chr <- my_data$sites
site_ID <- my_data$sites_numeric  # vector of sites
siteLookup <- as.numeric(as.factor(my_data$siteLookup)) 
x <- my_data$x # site type covariate
y <- my_data$y # outcome

stan_data <- c("N", 
               "n_pots", "pot_ID",
               "n_sites", "site_ID",
               "siteLookup",
               "x", "y")

# Parameters monitored
params <- c("alpha0",
            "beta",
            "sigma_alpha_site",
            "sigma_alpha_pot"
)

# MCMC settings
n_iterations <- 2000
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
       sigma_alpha_pot = runif(1, 0, 1),
       beta = runif(1, -1, 1)
       
  )
)

## --------------------------------------------------
### Run model
library(rstan)
stan_model <- "./models/logistic_model.stan"

## Call Stan from R
stan_out <- stan(stan_model,
                     data = stan_data, 
                     init = inits, 
                     pars = params,
                     chains = n_chains, iter = n_iterations, 
                     warmup = n_burnin, thin = n_thin,
                     seed = 1,
                     open_progress = FALSE,
                     cores = n_cores,
                     control = list(adapt_delta = 0.9))

print(stan_out, digits = 3)

saveRDS(stan_out, "./model_outputs/stan_out_logistic_regression.RDS")

## --------------------------------------------------
### Simple diagnostic plots

# traceplot
traceplot(stan_out, pars = c(
  "alpha0",
  "beta",
  "sigma_alpha_site",
  "sigma_alpha_pot"
))

# pairs plot
pairs(stan_out, pars = c(
  "alpha0",
  "beta",
  "sigma_alpha_site",
  "sigma_alpha_pot"
))

