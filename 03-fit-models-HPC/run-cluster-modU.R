# Use array number in script
args = commandArgs(trailingOnly = TRUE)
# 1-6000:1

################################################################################
## Setup
# Load functions
source('functions/FXN-modelU.R')

library(jagsUI)
library(dplyr)
library(magrittr)

################################################################################
# Load LHS parameters
params <- read.csv('data/parameter_combos.csv')

# Read in simulated datasets
sim.datasets <- readRDS('data/sim.datasets.rds')

#-------------------------------------------------------------------------------
# Run model with Us included, Us excluded, Us as NDs, and Us as Ds
iter = as.numeric(args[1])

out <- modelU(iter, input.data = sim.datasets, param.combos = params, flag = 'U')
write.csv(out, file=paste0('output/HPC/modelU-U-',iter,'.csv'), row.names = F)

# Run again for next iteration
iter = iter + 6000

out <- modelU(iter, input.data = sim.datasets, param.combos = params, flag = 'U')
write.csv(out, file=paste0('output/HPC/modelU-U-',iter,'.csv'), row.names = F)

#End script
