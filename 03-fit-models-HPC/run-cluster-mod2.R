# Use array number in script
args = commandArgs(trailingOnly = TRUE)
# 1-6000:1

################################################################################
## Setup
# Load functions
source('functions/FXN-model2.R')

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
# (Us removed) [ flag = 'noU' ]
# (U -> 0) [ flag = 'ND' ]
# (U -> 1) [ flag = 'D' ]

iter = as.numeric(args[1])

for (flag in c('noU','ND','D')) {
  # Run Model2: Estimate detection probabilities
  out <- model2(iter, input.data = sim.datasets, param.combos = params, flag = flag)
  write.csv(out, file=paste0('output/HPC/model2-',flag,'-',iter,'.csv'), row.names = F)
}


# Run again for next iteration
iter = iter + 6000

for (flag in c('noU','ND','D')) {
  # Run Model2: Estimate detection probabilities
  out <- model2(iter, input.data = sim.datasets, param.combos = params, flag = flag)
  write.csv(out, file=paste0('output/HPC/model2-',flag,'-',iter,'.csv'), row.names = F)
}

#End script
