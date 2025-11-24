
## ---------------------------
## Objective: Simulate a dataset for each parameter combination
##
## Input: FXN-sim.data.R
##        parameters_combos.csv
##
## Output: sim.datasets.rds - a list where each entry is one simulated dataset
## ---------------------------


## load packages ---------------------------
library(tidyverse)
library(magrittr)


## load functions ---------------------------
source('functions/FXN-sim-data.R') 


# Load LHS parameters ---------------------------
param.combos <- read.csv('data/parameter_combos.csv')

# Fixed parameters
theta01 <- 0
p011 <- 1
delta <- 0.95 # sensitivity
r <- 0.999 # specificity ~ 1

sim.datasets <- list()

for (i in 1:nrow(param.combos)) {
  # Simulate full dataset [0,1,U]
  sim.datasets[[i]] <- sim.data(nsites = param.combos$nsites[i],
                                nindiv = param.combos$nindiv[i],
                                ntests = param.combos$ntests[i],
                                psi = param.combos$psi[i],
                                theta11 = param.combos$theta11[i],
                                theta01 = theta01,
                                p111 = param.combos$p111[i],
                                p101 = param.combos$p101[i],
                                p001 = param.combos$p001[i],
                                p011 = p011,
                                delta = delta,
                                r = r)
  print(i) # To see what dataset was generated
}

saveRDS(sim.datasets, 'data/sim.datasets.rds')

# End script