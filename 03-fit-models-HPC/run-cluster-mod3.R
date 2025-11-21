# Use array number in script
args = commandArgs(trailingOnly = TRUE)

################################################################################
## Setup
# Load functions
source('functions/FXN-model3.R')

library(jagsUI)
library(dplyr)
library(magrittr)

################################################################################
# Load LHS parameters
params <- read.csv('data/parameter_combos.csv')

# Read in simulated datasets
sim.datasets <- readRDS('data/sim.datasets.rds')

#-------------------------------------------------------------------------------

# Flags determine the dataset to be used
# (Us removed) [ flag = 1 ]
# (U -> 0) [ flag = 2 ]
# (U -> 1) [ flag = 3 ]

iter = as.numeric(args[1])

for (flag in c('noU','ND','D')) {
  # Run Model3: Fixed diagnostics rates
  out <- model3(iter, input.data = sim.datasets, param.combos = params, flag = flag)
  write.csv(out, file=paste0('output/HPC/model3-',flag,'-',iter,'.csv'), row.names = F)
}


# Run again for next iteration
iter = iter + 6000

for (flag in c('noU','ND','D')) {
  # Run Model3: Fixed diagnostics rates
  out <- model3(iter, input.data = sim.datasets, param.combos = params, flag = flag)
  write.csv(out, file=paste0('output/HPC/model3-',flag,'-',iter,'.csv'), row.names = F)
}





#End script
