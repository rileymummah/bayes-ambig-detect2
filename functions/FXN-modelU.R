## ---------------------------
## This code was written by: r.o. mummah
## For questions: rmummah@umass.edu
## Date Created: 2023-02-27
## ---------------------------

## ---------------------------
## Objective:
##
##
## Input:
##
##
## Output:
##
##
## ---------------------------


## ---------------------------
## load up the packages we will need:  (uncomment as required)

# library(tidyverse)
library(jagsUI)
library(dplyr)
library(magrittr)

## ---------------------------
# Write each of the models in their own script so they can be easily called and applied to the simulated datasets

modelU <- function(i, input.data, param.combos, flag) {
  start <- Sys.time()

  x <- input.data[[i]]

  ##############################################################################
  # Uncertain detections included
  # Bundle data
  if (flag == 'U'){
  data <- list(y = x$y,
               nsites = x$nsites,
               nindiv = x$nindiv,
               ntests = x$ntests,
               # delta/r usually provided by diagnostic lab
               delta = x$delta, r = x$r) 
  }

  # Initial values
  # Take max value across surveys for each site and year combo
  zinit <- apply(data$y, c(1), max, na.rm = TRUE)
  zinit[zinit > 0] <- 1

  # Take max value across surveys for each site and year combo
  winit <- apply(data$y, c(1, 2), max, na.rm = TRUE)
  winit[winit > 0] <- 1

  # Initial conditions for latent state z, latent state w, and parameters
  inits <- function() {list(z = zinit,
                            w = winit,
                            psi = runif(1,0,1),
                            theta11 = runif(1,0,1),
                            b1 = runif(1,x$delta,1),
                            b2 = runif(1,0,1),
                            b3 = runif(1,0,b2))
  }

  # Parameters to store
  params <- c("psi", "theta11", "b1", "b2", "b3")

  print(paste0('ModelU-',flag,': Run ',i))

  # Model fitting
  out <- jagsUI::autojags(data = data,
                          inits = inits,
                          parameters.to.save = params,
                          model.file = 'models/modelU.txt',
                          n.chains = 4,
                          n.burnin = 1000,
                          n.adapt = 10000,
                          iter.increment = 5000,
                          Rhat.limit = 1.1,
                          max.iter = 101000,
                          parallel = TRUE)

  time <- Sys.time() - start


  # Save output
  bind_cols(dataset = paste0('dataset',i),
            model = 'modelU',
            flag = flag,
            parameter = c('psi','theta11','p111','p101','p001','deviance'),
            true.mean = c(param.combos$psi[i],
                          param.combos$theta11[i],
                          param.combos$p111[i],
                          param.combos$p101[i],
                          param.combos$p001[i],
                          NA),
            out$summary) %>%
  mutate(runtime = time,
         n.burnin = out$mcmc.info$n.burnin,
         n.iter = out$mcmc.info$n.iter,
         n.samples = out$mcmc.info$n.samples) -> save.out
  
  print(paste0('Model U-',flag,': Run ',i,' COMPLETE'))
  
  return(save.out)
  
}
