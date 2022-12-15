library(tidyverse)

## Prepare real data for mixed-effects, logistic regression
#  started Dec. 14, 2022, J Ulrich

# Format binary outcome data where.. 
# the outcome is: 
# 0 - a flower is not pollen limited, OR
# 1 - a flower is pollen limited
# A random intercept for planter pot, nested in site will account for heterogeneity
# in nested sample groups.

prep_data <- function(max_PL_accepted,
                      PL_response_threshold){
  
  ###-----------------------------------------------------------------------------
  ## Read real data
  
  df <- read.csv("./data/clarkia_pollination_data_2022.csv")
  
  ###-----------------------------------------------------------------------------
  ## Data prep
  
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
    filter(PL_INDEX < max_PL_accepted)
  
  ###-----------------------------------------------------------------------------
  ## Transform from a continuous to binary outcome
  
  # Here 1 indicates pollen limited
  # and 0 indicates no pollen limitation 
  df_binary <- df_prepped %>%
    mutate(PL_INDEX_BINARY = ifelse(PL_INDEX < PL_response_threshold, 1, 0))
  
  ###-----------------------------------------------------------------------------
  ## Extract additional info from the df
  
  # siteLookup (which level 3 cluster is level 2 cluster grouped in?)
  siteLookup <- df_binary %>%
    group_by(as.factor(POT_NUMBER)) %>%
    slice(1) %>%
    pull(SITE)
  
  # site treatment covariate 
  x <- df_binary %>%
    # treatment as 1, control as 0
    mutate(SITE_TREATMENT = (as.numeric(as.factor(SITE_TREATMENT)) - 1)) %>%
    pull(SITE_TREATMENT) 
  
  # site treatment covariate 
  y <- df_binary %>%
    pull(PL_INDEX_BINARY) 
  
  ###-----------------------------------------------------------------------------
  ## Return stuff
  
  return(list(
    
    N = nrow(df_binary), # number of pairs
    n_pots = length(unique(df_binary$POT_NUMBER)),
    pots = df_binary$POT_NUMBER, # vector of pot names
    n_sites = length(unique(df_binary$SITE)), # number of sites
    
    # should add a numeric site name and return both
    sites = df_binary$SITE, # vector of site names
    sites_numeric = as.numeric(as.factor(df_binary$SITE)), # numeric site names
    # need a site name for each pot (length = n_pots)
    siteLookup = siteLookup,
    
    x = x, # site type covariate data
    y = y # outcome data (binary PL index)
    
  ))
  
}