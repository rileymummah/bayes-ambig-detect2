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
               delta = x$delta, r = x$r, # Usually provided by diagnostic lab
               theta01 = x$theta01, p011 = x$p011) # Assumed to be 0
  # } else if (flag == 'noU') {
  #   x$y[,,3] <- 0 # Remove Us
  #   
  #   data <- list(y = x$y,
  #                nsites = x$nsites,
  #                nindiv = x$nindiv,
  #                ntests = x$ntests, # Non-detections + Detections
  #                delta = x$delta, r = x$r, # Usually provided by diagnostic lab
  #                theta01 = x$theta01, p011 = x$p011) # Assumed to be 0
  } else if(flag == 'ND') {
    x$y[,,1] <- x$y[,,1] + x$y[,,3] # Add Us to NDs
    x$y[,,3] <- 0
    
    data <- list(y = x$y, 
                 nsites = x$nsites,
                 nindiv = x$nindiv,
                 ntests = x$ntests,
                 delta = x$delta, r = x$r, # Usually provided by diagnostic lab
                 theta01 = x$theta01, p011 = x$p011) # Assumed to be 0
  } else if (flag == 'D') {
    x$y[,,2] <- x$y[,,2] + x$y[,,3] # Add Us to Ds
    x$y[,,3] <- 0
    
    data <- list(y = x$y,
                 nsites = x$nsites,
                 nindiv = x$nindiv,
                 ntests = x$ntests,
                 delta = x$delta, r = x$r, # Usually provided by diagnostic lab
                 theta01 = x$theta01, p011 = x$p011) # Assumed to be 0
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
                            p111 = runif(1,0.5,1),
                            p100 = runif(1,0.5,1),
                            p000 = runif(1,0.5,1))
  }

  # Parameters to store
  params <- c("psi", "theta11", "p111", "p100", "p000")

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
            parameter = c('psi','theta11','p111','p100','p000','deviance'),
            true.mean = c(param.combos$psi[i],
                          param.combos$theta11[i],
                          param.combos$p111[i],
                          param.combos$p100[i],
                          param.combos$p000[i],
                          NA),
            out$summary) %>%
  mutate(runtime = time,
         n.burnin = out$mcmc.info$n.burnin,
         n.iter = out$mcmc.info$n.iter,
         n.samples = out$mcmc.info$n.samples) -> save.out
  
  print(paste0('Model U-',flag,': Run ',i,' COMPLETE'))
  
  return(save.out)
  
}
