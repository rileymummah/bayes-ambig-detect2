
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

library(jagsUI)
library(dplyr)
library(magrittr)

## ---------------------------

model2 <- function(i, input.data, param.combos, flag) {
  start <- Sys.time()

  x <- input.data[[i]]

  ##############################################################################
  # Model-noU2 = Uncertain detections excluded - estimate detection probabilities
  # Bundle data
  if (flag == 'noU') {
    # Remove Us
    data <- list(y = x$y[,,2], # Detections only
                 nsites = x$nsites,
                 nindiv = x$nindiv,
                 ntests = x$y[,,1] + x$y[,,2]) # Non-detections + Detections
  } else if(flag == 'ND') {
    # Make Us 0
    data <- list(y = x$y[,,2], # Detections only
                 nsites = x$nsites,
                 nindiv = x$nindiv,
                 ntests = matrix(x$ntests, nrow = x$nsites, ncol = x$nindiv))
  } else if (flag == 'D') {
    # Make Us 1
    data <- list(y = x$y[,,2] + x$y[,,3], # Detections and uncertain detections
                 nsites = x$nsites,
                 nindiv = x$nindiv,
                 ntests = matrix(x$ntests, nrow = x$nsites, ncol = x$nindiv))
  }

  # Initial values
  # Take max value across surveys for each site and year combo
  zinit <- apply(data$y, c(1), max, na.rm = TRUE)
  zinit[zinit > 0] <- 1

  # Take max value across surveys for each site and year combo
  # winit <- apply(data$y, c(1, 2), max, na.rm = TRUE)
  # winit[winit > 0] <- 1


  # Initial conditions for latent state z, latent state w, and parameters
  inits <- function() {list(z = zinit,
                            # w = winit,
                            psi = runif(1,0,1),
                            theta11 = runif(1,0,1),
                            p11 = runif(1,0.5,1),
                            p01 = runif(1,0,0.25))
  }

  # Parameters to store
  params <- c("psi", "theta11", "p11", "p01")

  print(paste0('Model 2-',flag,': Run ',i))

  # Model fitting
  out <- jagsUI::autojags(data = data,
                          inits = inits,
                          parameters.to.save = params,
                          model.file = 'models/model2.txt',
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
            model = 'model2',
            flag = flag,
            parameter = c('psi','theta11','p11','p01','deviance'),
            true.mean = c(param.combos$psi[i],
                          param.combos$theta11[i],
                          param.combos$delta[i],
                          param.combos$r[i],
                          NA),
            out$summary) %>%
    mutate(runtime = time,
           n.burnin = out$mcmc.info$n.burnin,
           n.iter = out$mcmc.info$n.iter,
           n.samples = out$mcmc.info$n.samples) -> save.out
  
  print(paste0('Model 2-',flag,': Run ',i,' COMPLETE'))
  
  return(save.out)
  
}
