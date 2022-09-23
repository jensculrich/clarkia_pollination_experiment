library(tidyverse)
library(rstanarm)
library(ggplot2)

### Pollen limitation analysis
## Started 9.22.22 by J Ulrich

## SITE_TREATMENT
# Plants were placed in arrays at parks with ("treatment") vs without ("control")
# flower enhancements when in flower. 
# n=5 unique control parks and n=6 unique treatment parks (SITE)

## FLOWER_TREATMENT
# On each plant - to account for variation in resource limitation - one flower was
# designated as ("treatment") and received a pollen supplementation, while another flower
# designated as ("control") and experienced and ambient pollination environment.
# generally we used 4 plants per pot, unless there were not 4 or more plants in the
# pot that produced 2 or more healthy flowers during the one week period where plant arrays
# were installed at sites. 

## SEEDS_PRODUCED
# we let the fruits develop for ~3 weeks after the experiment and then clipped and
# bagged them. Fruits were dissected and number of seeds (the response variable) were 
# counted in the lab (counts currently still ongoing as of 9.22.22 and so this is a partial dataset)

## expected outcomes
# We expect a positive effect of PLANT_TREATMENT on seeds produced. If no effect, plants
# are not pollen limited in this system. 
# A positive impact of SITE_TREATMENT on SEEDS_PRODUCED would indicate that flower
# enhancements have a positive effect across the board on seeds produced, but 
# we are really looking to test the interaction between PLANT_TREATMENT*SITE_TREATMENT
# a positive value would indicate that plants are less pollen limited in treatment sites.

# Analysis with simulated data

### Analysis with real data
df <- read.csv("clarkia_pollination_data_2022.csv")

# remove flowers with uncounted seed pods
df <- df %>%
  drop_na(SEEDS_PRODUCED) 

# recode treatments as binary integer values (0 = control, 1 = treatment)
df$SITE_TREATMENT <- recode_factor(df$SITE_TREATMENT, 
                control = "0", treatment = "1")
df$FLOWER_TREATMENT <- recode_factor(df$FLOWER_TREATMENT, 
                                   "control" = "0", "treatment" = "1",
                                   " treatment" = "1")
df <- df %>%
  mutate(SITE_TREATMENT = as.integer(SITE_TREATMENT) - 1,
         FLOWER_TREATMENT = as.integer(FLOWER_TREATMENT) - 1,
         SITE =  as.factor(SITE))
 
str(df)

stan_glm_fit1 <- stan_glmer(SEEDS_PRODUCED ~ 
                  SITE_TREATMENT + 
                  FLOWER_TREATMENT + 
                  SITE_TREATMENT*FLOWER_TREATMENT + (1|SITE), 
                family = poisson(link = "log"),
                data = df) 

print(stan_glm_fit1)  

(test1 <- pp_check(stan_glm_fit1, plotfun = "stat", binwidth = 0.01))
pp_check(stan_glm_fit1)

stan_glm_fit2 <- update(stan_glm_fit1, family = neg_binomial_2)
print(stan_glm_fit2) 

(test2 <- pp_check(stan_glm_fit2, plotfun = "stat", binwidth = 0.01))
pp_check(stan_glm_fit2)
# visual check greatly favours the neg bin model, but that being said
# it still doesn't look great.

loo1 <- loo(stan_glm_fit1, cores = 2)
loo2 <- loo(stan_glm_fit2, cores = 2)
loo_compare(loo1, loo2)
 # clear preference for the negative binomial model

# library(shinystan)
# launch_shinystan(fit)

### Alternative Approach
# use pollen limitation index for each plant 
# (divide see)
### Analysis with real data
df2 <- read.csv("clarkia_pollination_data_2022.csv")

# remove flowers with uncounted seed pods
df2 <- df2 %>%
  filter(PL_INDEX != "0") %>%
  filter(PL_INDEX != "#DIV/0!") %>%
  filter(PL_INDEX != "") %>%
  mutate(PL_INDEX = as.numeric(PL_INDEX)) %>%
  filter(PL_INDEX < 2) # there is one big outlier that I am just going to remove for this exploratory analysis
  
# recode treatments as binary integer values (0 = control, 1 = treatment)
df2$SITE_TREATMENT <- recode_factor(df2$SITE_TREATMENT, 
                                   control = "0", treatment = "1")
df2 <- df2 %>%
  mutate(SITE_TREATMENT = as.integer(SITE_TREATMENT) - 1,
         SITE =  as.factor(SITE))

str(df2)

# run lmer
stan_glm_fit3 <- stan_lmer(PL_INDEX ~ 
                              SITE_TREATMENT + 
                            (1|SITE),
                            data = df2) 

print(stan_glm_fit3)  

(test3 <- pp_check(stan_glm_fit3, plotfun = "stat", binwidth = 0.01))
pp_check(stan_glm_fit3)

# run glmer with gamma distribution
stan_glm_fit4 <- stan_glmer(PL_INDEX ~ 
                             SITE_TREATMENT + 
                             (1|SITE),
                            family = Gamma(link = "inverse"),
                           data = df2) 

print(stan_glm_fit4)  

(test4 <- pp_check(stan_glm_fit4, plotfun = "stat", binwidth = 0.01))
pp_check(stan_glm_fit4)

# run glmer with gamma distribution
stan_glm_fit5 <- stan_glmer(PL_INDEX ~ 
                              SITE_TREATMENT + 
                              (1|SITE),
                            family = Gamma(link = "log"),
                            data = df2) 

print(stan_glm_fit5)  

(test5 <- pp_check(stan_glm_fit5, plotfun = "stat", binwidth = 0.01))
pp_check(stan_glm_fit5)

loo3 <- loo(stan_glm_fit3, cores = 2)
loo4 <- loo(stan_glm_fit4, cores = 2)
loo5 <- loo(stan_glm_fit5, cores = 2)
loo_compare(loo3, loo4, loo5)

# based on the visual pp checks and on the loo scores,
# model 4 and 5 currently seem to be the best perfomative models,
# perhaps with doubling the data by counting the rest of the seeds the 
# overall model output fit will slightly improve, giving best estimates
# for parameters