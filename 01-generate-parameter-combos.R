
## ---------------------------
## Objective: To use a Latin hyper cube sampler to evenly sample distributions
## for parameter combinations used in simulations
##
## Input: N/A
##
## Output: parameter_combos.csv
## ---------------------------

## load packages ---------------------------
library(lhs)
library(tidyverse)


# Set seed for reproducible code
set.seed(22920)


# Set up information for sampler -----------------------------------------------

# Number of samples in the parameter space
n_samples <- 1000 #10000

# Total number of parameters
n_params <- 5


# Initialize sampler -----------------------------------------------------------

lhs_raw <- randomLHS(n = n_samples, # Number of rows / samples
                     k = n_params)  # Number of columns or parameter variables

# head(lhs_raw)


# Initialize data frame --------------------------------------------------------
param_combos <- setNames(data.frame(matrix(ncol = n_params, nrow = n_samples)),
                         c('psi', 'theta11', 'p111', 'p101', 'p001'))

# head(param_combos)


# Use LHS for model parameters -------------------------------------------------

# Set variables that will change
param_combos <- param_combos %>%
                  mutate(psi = qunif(lhs_raw[, 1], min = 0, max = 1),
                         theta11 = qunif(lhs_raw[, 2], min = 0, max = 1),
                         p111 = qunif(lhs_raw[, 3], min = 0.5, max = 1),
                         p101 = qunif(lhs_raw[, 4], min = 0, max = 0.25), # p111 > p101
                         p001 = qunif(lhs_raw[, 5], min = 0, max = p101)) # p101 >= p001

# head(param_combos)


# Create combinations of sample sizes ------------------------------------------
nsites <- c(20, 100) # low/high
nindiv <- c(5, 50) # low/high
ntests <- c(1, 2, 3) # reality, ideal no. of samples, triplicate PCR

sample_size <- expand.grid(nsites = nsites,
                           nindiv = nindiv,
                           ntests = ntests)


# Cross-join datasets ----------------------------------------------------------
param_combos <- cross_join(sample_size, param_combos)


# Write file -------------------------------------------------------------------

write.csv(param_combos, file = "data/parameter_combos.csv", row.names = FALSE)


# End Script
