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

source()
my_data <- prep_data(max_PL_accepted,
                     PL_response_threshold)