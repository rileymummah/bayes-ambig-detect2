# Use array number in script
args = commandArgs(trailingOnly = TRUE)

model = as.character(args[1])
iter = as.numeric(args[2])
flag = as.character(args[3])


################################################################################
## Setup
library(jagsUI)
library(dplyr)
library(magrittr)

# Load LHS parameters
params <- read.csv('data/parameter_combos.csv')

# Read in simulated datasets
sim.datasets <- readRDS('data/sim.datasets.rds')


################################################################################
# Load functions
if (model == '1') {
  
  source('functions/FXN-model1.R')
  out <- model1(iter, input.data = sim.datasets, param.combos = params, flag = flag)
  
} else if (model == '2') {
  
  source('functions/FXN-model2.R')
  out <- model2(iter, input.data = sim.datasets, param.combos = params, flag = flag)
  
} else if (model == '3') {
  
  source('functions/FXN-model3.R')
  out <- model3(iter, input.data = sim.datasets, param.combos = params, flag = flag)
  
} else if (model == 'U') {
  
  source('functions/FXN-modelU.R')
  out <- modelU(iter, input.data = sim.datasets, param.combos = params, flag = flag)
  
}

write.csv(out, file=paste0('output/HPC/model',model,'-',flag,'-',iter,'.csv'), row.names = F)

#End script
