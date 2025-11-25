
## ---------------------------
## Objective: To combine output generated on high performance computing cluster
##
##
## Input:
##   parameter_combos.csv
##   combined-modelU.csv
##   combined-model1.csv
##   combined-model2.csv
##   combined-model3.csv
##
## Output:
##   combined-output.csv
##   combined-output.rds
##
## ---------------------------

## load packages ---------------------------
library(tidyverse)
library(magrittr)
library(stringr)

## load functions ---------------------------

## load data ---------------------------
# List of models and flags (missing from simulation output)
models <- c('model2','model3')#'model1',

param.combos <- read.csv('data/parameter_combos.csv') %>%
                rownames_to_column() # Necessary for subsequent code

## Combine datasets ---------------------------

# Read in initial dataset
data <- read.csv('output/combined-model1.csv', header=T) %>% #ModelU
        filter(dataset != 'dataset')

# Add subsequent datasets to file; adding flag for data manipulation
for (i in 1:length(models)) {
  tmp <- read.csv(paste0('output/combined-',models[i],'.csv'),
                  header=T) %>%
    filter(dataset != 'dataset')

  data <- full_join(data, tmp)
}

# Remove deviance
filter(data, parameter != 'deviance') -> data


# Check that there are equal numbers (divisible by 12000) across models/datasets
table(data$model, data$flag)

# # Check that each model has the correct number of entries
# # ModelU should have 5 parameter x adjustment combos
# # Model1 should have 15 parameter x adjustment combos
# # Model2 should have 12 parameter x adjustment combos
# # Model3 should have 6 parameter x adjustment combos
table(data$dataset, data$model) %>%
  as.data.frame() -> tmp

table(tmp$Var2, tmp$Freq)


# Combine output and save
data %>%
  rename(q2.5 = X2.5.,
         q25 = X25.,
         q50 = X50.,
         q75 = X75.,
         q97.5 = X97.5.) %>%
  mutate_at(c("true.mean","mean","sd","q25","q50","q75","Rhat","n.eff",
              "overlap0","f","runtime","n.burnin","n.iter","n.samples",
              "q2.5","q97.5"), as.numeric) %>%
  # To mark which parameter set and create a model-flag variable
  mutate(param.set = str_extract(dataset, "[0-9]+"),
         model.flag = paste0(model,'-',flag)) %>%
  # To order variables
  select(dataset, param.set, model, flag, model.flag, parameter, true.mean,
         mean, sd, q2.5, q25, q50, q75, q97.5, Rhat, n.eff, overlap0, f,
         runtime, n.burnin, n.iter, n.samples) %>%
  # Join with true parameter values
  left_join(., param.combos[,c('rowname','nsites','nindiv','ntests')],
            by=c('param.set'='rowname')) %>%
  # Calculate some metrics
  mutate(acc = mean - true.mean,
         bci.width = q97.5 - q2.5,
         nsites = factor(nsites,
                         levels = c(20, 100),
                         labels = c('l.sites','h.sites')),
         nindiv = factor(nindiv,
                         levels = c(5, 50),
                         labels = c('l.indiv','h.indiv')),
         ntests = factor(ntests,
                         levels = c(1, 2, 3),
                         labels = c('low','med','high')),
         model.flag = ifelse(model == 'modelU', 'modelU', model.flag)) -> data

# Save cleaned/combined data
write.csv(data, 'output/combined-output.csv')
saveRDS(data, 'output/combined-output.rds')
