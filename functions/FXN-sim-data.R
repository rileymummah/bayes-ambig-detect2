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
#   theta01 = individual contamination rate (assumed not to occur)
#   p111 = true positive (sample-level) # CHANGE
#   p100 = sample-level false positive # CHANGE
#   p011 = sample- and site-level false positive # CHANGE
#   p000 = site-level false positive (assumed not to occur) # CHANGE
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
                     p111, p101, p011, p001,
                     r, delta) {
  # Create dataset storage
  w <- matrix(NA, nrow = nsites, ncol = nindiv)
  y <- array(NA, c(nsites, nindiv, 3)) # There are 3 detection states: 1,0,U
  p <- array(NA, c(nsites, nindiv, 3)) # Store detection prob vector
  
  # Simulate true occurrence at each site
  # set.seed(22920)
  z <- rbinom(nsites, 1, p = psi)
  
  # Simulate true status of individuals at each site
  for (i in 1:nsites) {
    w[i,] <- rbinom(nindiv, 1, p = (z[i]*theta11 + (1-z[i])*theta01))
  }
  
  # Simulate observed states of individuals at each site (detection/nondetection data)
  for (i in 1:nsites) {
    for (j in 1:nindiv) {
      # Generate probabilities for detections and non-detections # [0, 1, U]
      # P(Y=0)
      p[i,j,1] <- z[i]*w[i,j]*(p111*(1-delta) + (1-p111)*r) + 
                  z[i]*(1-w[i,j])*((1-p101)*(1-delta) + p101*r) + 
                  (1-z[i])*w[i,j]*(p011*(1-delta) + (1-p011)*r) + 
                  (1-z[i])*(1-w[i,j])*((1-p001)*(1-delta) + p001*r)
      # P(Y=1)
      p[i,j,2] <- z[i]*w[i,j]*(p111*delta + (1-p111)*(1-r)) + 
                  z[i]*(1-w[i,j])*((1-p101)*delta + p101*(1-r)) + 
                  (1-z[i])*w[i,j]*(p011*delta + (1-p011)*(1-r)) + 
                  (1-z[i])*(1-w[i,j])*((1-p001)*delta + p001*(1-r))
      # P(Y=U)
      p[i,j,3] <- z[i]*w[i,j]*(p111*(1-delta) + (1-p111)*(1-r)) + 
                  z[i]*(1-w[i,j])*((1-p101)*(1-delta) + p101*(1-r)) + 
                  (1-z[i])*w[i,j]*(p011*(1-delta) + (1-p011)*(1-r)) + 
                  (1-z[i])*(1-w[i,j])*((1-p001)*(1-delta) + p001*(1-r))
      
      # Sample detection state from multinomial dist
      y[i,j,] <- rmultinom(1, ntests, prob=p[i,j,]) # [0, 1, U]
      
    } #j
  } #i
  return(list(z = z, w = w, y = y, p = p,
              nsites = nsites, nindiv = nindiv, ntests = ntests,
              psi = psi, theta11 = theta11, theta01 = theta01, 
              p111 = p111, p101 = p101, p011 = p011, p001 = p001,
              delta = delta, r = r))
}
