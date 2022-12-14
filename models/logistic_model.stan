// STAN MODEL FOR LOGISTIC MIXED-EFFECTS REGRESSION
// started Dec. 14, 2022, J Ulrich

// Model is intended to estimate the effect of categorical site type
// on the outcome of: 
// 0 - a flower is not pollen limited, OR
// 1 - a flower is pollen limited
// A random intercept for plants grouped in planter pots, 
// nested in sites, will account for heterogeneity among sample groups.

data {
  
  int<lower=0> N; // number of paired flowers
  
  vector[N] x; // covariate of site type for each flower pair
  
  int<lower=0,upper=1> y[N]; // Pollen limitation (PL) outcome for each flower pair
  
  int<lower=1> n_pots;  // number of pots 
  int<lower=1, upper=n_pots> pots[N];  // vector of pot names
  
  int<lower=1> n_sites;  // number of sites 
  int<lower=1, upper=n_sites> sites[N];  // vector of site names
  
}

parameters {
  
  real alpha0; // global intercept 
  
  // pot specific intercept allows some sites to have lower success than others, 
  // but with overall estimates for success partially informed by the data pooled across all pots.
  vector[n_pots] alpha_pot; // pot specific intercept for PL outcome
  real<lower=0> sigma_alpha_pot; // variance in pot intercepts
  
  // site specific intercept allows some sites to have lower success than others, 
  // but with overall estimates for success partially informed by the data pooled across all sites.
  vector[n_sites] alpha_site; // site specific intercept for PL outcome
  real<lower=0> sigma_alpha_site; // variance in site intercepts
  
  real beta; // effect of site type on the PL outcome

}

transformed parameters{
  
  // varying intercepts
  real alpha0_pot[n_pots];
  real alpha0_site[n_sites];

  // the linear predictor for the observations
   real p[N];

  // compute the varying intercept at the site level
  for(i in 1:N){
    alpha0_site[i] = alpha_site[i];
  }

  // compute varying intercept at the pot within site level
  for(i in 1:n_pots){
     alpha0_pot[i] = alpha0_site[pots[i]] + alpha_pot[i];
  }
  
  for(i in 1:N){
    
      p[i] = alpha0 + // a global intercept
             alpha0_pot[pots[i]] + // a pot|site specific intercept
             beta * x[i] // an effect of site type
            ; // end p
              
  }
  
}

model {
  
  // PRIORS
  
  alpha0 ~ cauchy(0, 2.5); // weakly informative prior for global intercept
  
  
  // level-3 grouping
  alpha_site ~ normal(0, sigma_alpha_site); 
  // prob of success intercept for each site drawn from the community
  // distribution (variance defined by sigma), centered at 0. 
  sigma_alpha_site ~ cauchy(0, 1); // weakly informative prior
  
  // level-2 grouping
  alpha_pot ~ normal(0, sigma_alpha_pot); 
  // prob of success intercept for each site drawn from the community
  // distribution (variance defined by sigma), centered at 0. 
  sigma_alpha_pot ~ cauchy(0, 1); // weakly informative prior
  
  beta ~ cauchy(0, 2.5); // weakly informative prior for effect of site type on outcome
  
  // LIKELIHOOD
  
  y ~ bernoulli_logit(p);
  
}
