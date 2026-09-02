## ---------------------------
# Objective: Function to simulate data via multi-level occupancy model with
#            imperfect (misclassification) and ambiguous detections.
#
# Input:
#   The function takes the following parameters:
#   nsites = number of sites
#   nindiv = number of individuals per site
#   ntests = number of samples processed per individual
#   psi = true site occupancy
#   theta11 = true site prevalence
#   theta01 =  contamination rate (assumed not to occur)
#   p111 = detection probability | pathogen at site & individual
#   b2 = classification probability | pathogen at site but not individual
#   b3 = classification probability | pathogen not at site & individual
#   delta = diagnostic test sensitivity
#   r = diagnostic test specificity
#
# Output:
#   The output of the function is a list which contains z (the nsites-length
#   vector of true occurrence at each site), w (the nsites x nindiv array of
#   the true infection state of each individual), y (the nsites x nindiv x 3
#   array of the observed detection state of each individual's tests), and the
#   values of the input parameters.
#
## ---------------------------



sim.data <- function(nsites, nindiv, ntests,
                     psi, theta11, theta01, 
                     p111, b2, b3,
                     r, delta) {
  # Create dataset storage
  w <- matrix(NA, nrow = nsites, ncol = nindiv)
  y <- array(NA, c(nsites, nindiv, 3)) # There are 3 detection states: 1,0,U
  p <- array(NA, c(nsites, nindiv, 3)) # Store detection prob vector
  
  # Derived parameters
  b1 <- delta/p111
  p101 <- 1-r
  p001 <- 1-r
  
  # Simulate true occurrence at each site
  # set.seed(22920)
  z <- rbinom(nsites, 1, prob = psi)
  
  # Simulate true status of individuals at each site
  # Restrict w to 0 when z=0
  for (i in 1:nsites) {
    if (z[i] == 0) {
      w[i,] <- 0
    } else {
      w[i,] <- rbinom(nindiv, 1, prob = (z[i]*theta11))
    }
    
  }
  
  # Simulate observed states of individuals at each site (detection/nondetection data)
  for (i in 1:nsites) {
    for (j in 1:nindiv) {
      # Generate probabilities for detections and non-detections # [0, 1, U]
      # P(Y=0)
      p[i,j,1] <- z[i]*w[i,j]*(1-p111) +
                  z[i]*(1-w[i,j])*(1-p101) + 
                  (1-z[i])*(1-w[i,j])*(1-p001)
      
      # P(Y=1)
      p[i,j,2] <- z[i]*w[i,j]*b1*p111 + 
                  z[i]*(1-w[i,j])*b2*p101 +
                  (1-z[i])*(1-w[i,j])*b3*p001
      
      # P(Y=U)
      p[i,j,3] <- z[i]*w[i,j]*(1-b1)*p111 +
                  z[i]*(1-w[i,j])*(1-b2)*p101 +
                  (1-z[i])*(1-w[i,j])*(1-b3)*p001
      
      # Sample detection state from multinomial dist
      y[i,j,] <- rmultinom(1, ntests, prob=p[i,j,]) # [0, 1, U]
      
    } #j
  } #i
  
  return(list(z = z, w = w, y = y, p = p,
              nsites = nsites, nindiv = nindiv, ntests = ntests,
              psi = psi, theta11 = theta11, theta01 = theta01, 
              p111 = p111, b2 = b2, b3 = b3,
              delta = delta, r = r))
}
