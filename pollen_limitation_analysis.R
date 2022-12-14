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

###-----------------------------------------------------------------------------
## Analysis with real data

df <- read.csv("clarkia_pollination_data_2022.csv")

###-----------------------------------------------------------------------------
## Basic data summary

# How many pairs did we tag and place in the field
# (Including those we would have tagged but didn't have a flower for in a pot) 
pairs <- 0.5 * ( 
  # multiply by .5, because nrow counts both flowers from a plant
  df %>%
    nrow())

# How many pairs flowered in the window and were successfully treated
# with supplemental pollen during the window
successfully_treated_pairs <- 0.5 * ( 
  # multiply by .5, because nrow counts both flowers from a plant
  df %>%
  filter(BOTH_FLOWERS_OPEN_AND_POLLEN_APPLIED == "Y") %>%
  nrow())

# remove flowers with uncounted seed pods
successfully_treated_pairs_recovered <- 0.5 * ( 
  # multiply by .5, because nrow counts both flowers from a plant
  df %>%
    filter(BOTH_FLOWERS_OPEN_AND_POLLEN_APPLIED == "Y",
           BOTH_CAPSULES_RECOVERED == "Y") %>%
    nrow())

# number pairs where either a) didn't both flower during the experiment or 
# b) both flowered but the stigma of treatment flower was not receptive when we visited the site
(pairs - successfully_treated_pairs)


# number pairs where we didn't recover both capsules either due to
# a) capsule split during the ripening phase (maybe from the heat?)
# b) capsule was removed, presumably by herbivory or weather (capsule fully missing)
# c) labelling on the seed packet was unclear (two packets with the same label)
# d) seed packet was not found in the seed packet bin (assistants misplaced?)
(successfully_treated_pairs - successfully_treated_pairs_recovered)

###-----------------------------------------------------------------------------
## Data prep

max_PL_accepted <- 2

# prepare data set for analysis
df_prepped <- df %>%
  
  # remove flowers with from pairs not treated or not recovered
  filter(BOTH_FLOWERS_OPEN_AND_POLLEN_APPLIED == "Y",
         BOTH_CAPSULES_RECOVERED == "Y") %>%
  
  # I added excel formulas to view PL and PL INDEX, but let's drop and recalculate here
  # to make sure that the formula is clear and reproducible.
  # We will only use the PL_INDEX, which I use to represent the number of seeds
  # produced by a flower under ambient pollination conditions, relative to 
  # to the number of seeds produced by a flower on the same plant that receives a complete pollen load
  # PL_INDEX = 1 - ((Treatment - Control) / Control)
  # Should always be > 0
  # In theory, PL_INDEXshould be less than 1, but may be greater than 1 due to 
  # differences in ovules per capsule or treatment failure.
  select(-PL, -PL_INDEX) %>% # drop the values calculated in excel
  select(SITE_TREATMENT, SITE, POT_NUMBER, FLOWER_TREATMENT, SEEDS_PRODUCED) %>% # drop unneeded columns
  # add a plant ID variable
  mutate(PLANT_ID = rep(1:(.5*nrow(.)), each = 2)) %>%
  # now spread by values for seeds produced within Plant ID
  pivot_wider(names_from = FLOWER_TREATMENT, values_from = SEEDS_PRODUCED) %>%
  # and calculate PL_INDEX
  mutate(PL_INDEX = 1 - ((treatment - control) / treatment)) %>%

  # We will remove plants where the treatment produced 0 seeds,
  # presumably this is due to either a failure of the treatment or 
  # or some growth deformity/stress of the plant that completely prevented capsule from developing
  # should be clear here about how removing these plants will effect the results
  # but as is, cannot produce a PL_INDEX from these plants because cannot divide by 0 to get the ratio
  filter(treatment != 0) %>%

  # We will also remove plants from a maximum acceptabe PL_INDEX
  # Plants with an index above this value made significantly more seeds for untreated (ambient)
  # flowers than on supplemented flowers. This could be due to overapplication of pollen
  # failure to apply the pollen when the stigma was actually receptive,
  # or some growth deformity/stress of the plant that prevented capsule from developing
  # Again, remember to be transparent about this value if we choose to retain it,
  # and how altering the threshold might affect the results.
  filter(PL_INDEX < max_PL_accepted) %>%
  
  # Now we also likely won't be able to accept true 0 values if we log link the data
  # For now, these will be subbed with infitisemely small - but postive - PL_INDEX values
  mutate(PL_INDEX = ifelse(PL_INDEX == 0, 0.001, PL_INDEX))

###-----------------------------------------------------------------------------
## Visualize prepped data (And transform into a binary response)

# Plot density of response
(ggplot(df_prepped) +
   
   geom_density(aes(x=PL_INDEX, fill=SITE_TREATMENT), alpha=0.5) +
   scale_x_continuous(name = "Pollen limitation index") +
   scale_y_continuous(name = "Density", limits = c(0,1),
                      breaks = c(0, .5, 1),
                      label = c("0", "0.5", "1")) +
   scale_fill_discrete(name = "Site type", 
                       breaks = c("control", "treatment"),
                       labels = c("mowed", "unmowed")) + 
   theme_bw() +
   theme(legend.text=element_text(size=10),
         axis.text.x = element_text(size = 12),
         axis.text.y =element_text(size = 12),
         axis.title.x = element_text(size = 14),
         axis.title.y = element_text(size = 14),
         plot.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()) +
   geom_vline(xintercept=PL_response_threshold, lty="dashed")
 
)

# What is the mean PL_INDEX (given the filters we've already imposed on failed flowers)
mean(df_prepped$PL_INDEX)

# We can see that we have a bimodal response
# Consequently we will likely have to recode the variable into..
# PL_INDEX < threshold (pollen supplementation made a difference) 
# or
# PL_INDEX > threshold (pollen supplementation has no effect)
PL_response_threshold <- 0.5

# Here 1 indicates pollen limited
# and 0 indicates no pollen limitation 
df_binary <- df_prepped %>%
  mutate(PL_INDEX_BINARY = ifelse(PL_INDEX < PL_response_threshold, 1, 0))

# Plot density of response
(ggplot(df_binary) +
    
    geom_density(aes(x=PL_INDEX_BINARY, fill=SITE_TREATMENT), alpha=0.5)
  
)

df_binary_plot_data <- df_binary %>%
  group_by(SITE_TREATMENT) %>%
  add_tally(PL_INDEX_BINARY == 1) %>%
  add_tally(PL_INDEX_BINARY == 0) %>%
  slice(1) %>%
  mutate(PROPORTION_POLLEN_LIMITED = n / (n+nn)) %>%
  mutate(PROPORTION_NOT_POLLEN_LIMITED = nn / (n+nn)) %>%
  select(SITE_TREATMENT, 
         PROPORTION_POLLEN_LIMITED, PROPORTION_NOT_POLLEN_LIMITED) %>%
  pivot_longer(!SITE_TREATMENT, names_to = "type", values_to = "proportion")

# Plot density of response
(ggplot(df_binary_plot_data, aes(x=type, y=proportion, fill=SITE_TREATMENT)) +
    
    geom_bar(stat="identity", width=.5, position = "dodge") +
    scale_x_discrete(name="", breaks = c("PROPORTION_NOT_POLLEN_LIMITED", "PROPORTION_POLLEN_LIMITED"),
                       labels=c("Not pollen limited", "Pollen limited")) +
    scale_y_continuous(name="Proportion of flowers", 
                       limits = (c(0,1)), breaks = c(0, .25, .5, .75, 1),
                       label = c("0%", "25%", "50%", "75%", "100%")) +
    scale_fill_discrete(name = "Site type", 
                        breaks = c("control", "treatment"),
                        labels = c("mowed", "unmowed")) +
    theme_bw() +
    theme(legend.text=element_text(size=10),
        axis.text.x = element_text(size = 12),
        axis.text.y =element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
  
)  

###-----------------------------------------------------------------------------
## Fit model

# run glmer with gamma distribution
stan_fit <- stan_glmer(PL_INDEX_BINARY ~ 
                              SITE_TREATMENT + 
                              (1|SITE),
                            family = binomial(link = "logit"),
                            data = df_binary) 

# confirm sufficient mixing of the chains
(trace <- plot(stan_fit, "trace", pars = c("(Intercept)",
                                           "SITE_TREATMENTtreatment")))
# confirm stable exploration of the possible parameter space
(pairs(stan_fit, pars = c("(Intercept)",
                                         "SITE_TREATMENTtreatment")))

saveRDS(stan_fit, "./stan_fit.RDS")
 
###-----------------------------------------------------------------------------
## View model outputs

# values less than zero decrease the odds that a plant is pollen limited
print(stan_fit)  
round(posterior_interval(stan_fit, prob = 0.95), 2)

# posterior distribution for the parameters describing the uncertainty 
# related to unknown parameter values:
plot(stan_fit)
pplot <- plot(stan_fit, pars = c("(Intercept)",
                                 "SITE_TREATMENTtreatment",
                                 "Sigma[SITE:(Intercept),(Intercept)]")
              )
pplot + 
  geom_vline(xintercept = 0) +
  scale_y_discrete(breaks=c("(Intercept)",
                            "SITE_TREATMENTtreatment",
                            "Sigma[SITE:(Intercept),(Intercept)]"
                            ),
                   labels=c("Intercept",
                            "Effect of no mow",
                            "Sigma (site)"
                   )) +
  scale_x_continuous(
    name="Posterior Parameter Estimate \n (Values > 0 increase odds of pollen limitation; \n Values < 0 decrease odds of pollen limitation)") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y =element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

plot_title <- ggplot2::ggtitle("Posterior Distributions")
plot(stan_fit, "dens_overlay", pars = c("(Intercept)",
                                        "SITE_TREATMENTtreatment")) + 
  plot_title

###-----------------------------------------------------------------------------
## LOO comparison

# leave one out cross validation
(loo1 <- loo(stan_fit, save_psis = TRUE))

stan_fit0 <- update(stan_fit, formula = PL_INDEX_BINARY ~ (1|SITE), QR = FALSE, refresh=0)
(loo0 <- loo(stan_fit0))
loo_compare(loo0, loo1)
# the baseline model is (marginally) better.
# covariate (site treatment) DOES NOT contain clearly useful information for predictions.

###------------------------------------------------------------------------------
## Posterior Predictive Check

# can the model can reproduce the mean observed in the real data?
# (where the mean is the proportion of plants that are 
# pollen limited accourding to our pollen limitation threshold)
(test <- pp_check(stan_fit, plotfun = "stat", binwidth = 0.01))

# The histogram shows the distributions of the fraction of simulated data 
# that resulted in pollen limitation in each of the posterior draws.

# can the model can reproduce the patterns observed in the real data
# where we have most plants being pollinated relatively sufficiently
# and about 1/3 being pollen limited?
pp_check(stan_fit, nreps=100)














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