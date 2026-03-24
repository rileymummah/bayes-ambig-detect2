
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


sim.datasets <- list()

for (i in 1:nrow(param.combos)) {
  # Simulate full dataset [0,1,U]
  sim.datasets[[i]] <- sim.data(nsites = param.combos$nsites[i],
                                nindiv = param.combos$nindiv[i],
                                ntests = param.combos$ntests[i],
                                psi = param.combos$psi[i],
                                theta11 = param.combos$theta11[i],
                                theta01 = param.combos$theta01[i],
                                p111 = param.combos$p111[i],
                                b2 = param.combos$b2[i],
                                b3 = param.combos$b3[i],
                                delta = param.combos$delta[i],
                                r = param.combos$r[i])
  print(i) # To see what dataset was generated
}

saveRDS(sim.datasets, 'data/sim.datasets.rds')

# End script