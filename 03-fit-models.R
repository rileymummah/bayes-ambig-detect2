
## ---------------------------
## Objective: To fit models 1,2,3, and U to the simulated datasets. This code
##            should only be used if fitting the models locally. There is
##            additional code to fit the models on a high performance computer.
## 
## Input:
##    FXN-model1.R
##    FXN-model2.R
##    FXN-model3.R
##    FXN-modelU.R
##    parameter_combos.csv
##
## Output: 
##    combined-model1.rds
##    combined-model2.rds
##    combined-model3.rds
##    combined-modelU.rds
##    combined-output.rds
##
## ---------------------------

## load packages ---------------------------
library(tidyverse)
library(magrittr)
library(jagsUI)


## load functions ---------------------------
source('functions/FXN-model1.R')
source('functions/FXN-model2.R')
source('functions/FXN-model3.R')
source('functions/FXN-modelU.R')

## load data --------------------------------

# Load LHS parameters
params <- read.csv('data/parameter_combos.csv')

# Read in simulated datasets
sim.datasets <- readRDS('data/sim.datasets.rds')

#-------------------------------------------------------------------------------

# Flags determine the dataset to be used
# (Us removed) [ flag = noU ]
# (U -> 0) [ flag = ND ]
# (U -> 1) [ flag = D ]

runs <- expand.grid(flag = c('noU','ND','D'),
                    iter = 1:nrow(params))


# Run TEST section --------------------------------------------------------
# Run this section to see how the code works. I advise against running these 
# models locally. They will take weeks to run, but this code should be 
# sufficient for troubleshooting and model testing.

# Run a model with one parameter set
tmp <- model3(i = 5, flag = 'D', input.data = sim.datasets, param.combos = params)

# Run a model with multiple parameter sets
mapply(model3, i = runs$iter[1:3], flag = runs$flag[1:3], 
       MoreArgs = list(input.data = sim.datasets, 
                       param.combos = params),
       SIMPLIFY = F) -> tmp

gcplyr::merge_dfs(tmp, collapse = T) -> combined.model.output

saveRDS(combined.model.output, file = 'output/test-output.rds')


# Run Model1 --------------------------------------------------------------

mapply(model1, i = runs$iter, flag = runs$flag, 
       MoreArgs = list(input.data = sim.datasets, 
                       param.combos = params),
       SIMPLIFY = F) -> tmp

gcplyr::merge_dfs(tmp, collapse = T) -> combined.model1

# Because this takes so long to run, save the model-level output and combine once all models are run.
saveRDS(combined.model1, file = 'output/combined-model1.rds')


# Run Model2 --------------------------------------------------------------

mapply(model2, i = runs$iter, flag = runs$flag, 
       MoreArgs = list(input.data = sim.datasets, 
                       param.combos = params),
       SIMPLIFY = F) -> tmp

gcplyr::merge_dfs(tmp, collapse = T) -> combined.model2

# Because this takes so long to run, save the model-level output and combine once all models are run.
saveRDS(combined.model2, file = 'output/combined-model2.rds')



# Run Model3 --------------------------------------------------------------

# This takes something on the order of days-to-week to run (and this is the fastest/simplest of the models)
mapply(model3, i = runs$iter, flag = runs$flag, 
       MoreArgs = list(input.data = sim.datasets, 
                       param.combos = params),
       SIMPLIFY = F) -> tmp

gcplyr::merge_dfs(tmp, collapse = T) -> combined.model3

# Because this takes so long to run, save the model-level output and combine once all models are run.
saveRDS(combined.model3, file = 'output/combined-model3.rds')



# Run ModelU --------------------------------------------------------------

runs <- expand.grid(flag = 'U',
                    iter = 1:nrow(params))


mapply(modelU, i = runs$iter, flag = runs$flag,  
       MoreArgs = list(input.data = sim.datasets, 
                       param.combos = params),
       SIMPLIFY = F) -> tmp

gcplyr::merge_dfs(tmp, collapse = T) -> combined.modelU

# Because this takes so long to run, save the model-level output and combine once all models are run.
saveRDS(combined.modelU, file = 'output/combined-modelU.rds')


# Combine output files ----------------------------------------------------
modelU <- readRDS('output/combined-modelU.rds')
model1 <- readRDS('output/combined-model1.rds')
model2 <- readRDS('output/combined-model2.rds')
model3 <- readRDS('output/combined-model3.rds')

bind_rows(modelU, model1, model2, model3) %>%
  saveRDS('output/combined-output.rds')

#End script
