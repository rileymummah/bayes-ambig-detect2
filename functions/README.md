## Functions

### File 1: FXN-model1.R

*Description:*

*Data inputs:* N/A

*Outputs:* N/A

### File 2: FXN-model2.R

*Description:*

*Data inputs:* N/A

*Outputs:* N/A

### File 3: FXN-model3.R

*Description:*

*Data inputs:* N/A

*Outputs:* N/A

### File 4: FXN-modelU.R

*Description:*

*Data inputs:* N/A

*Outputs:* N/A

### File 5: FXN-sim-data.R

*Description:* A function to simulate data via a multilevel occupancy model with misclassification (false positives and false negatives) and ambiguous detections.

*Data inputs:* The function takes the following parameters:

- `nsites` = number of sites
- `nindiv` = number of individuals per site
- `ntests` = number of samples processed per individual
- `psi` = true site occupancy
- `theta11` = true site prevalence
- `theta01` = individual contamination rate (assumed not to occur)
- `p111` = true positive (sample-level)
- `p101` = sample-level false positive
- `p001` = sample- and site-level false positive
- `p011` = site-level false positive (assumed not to occur)
- `delta` = diagnostic test sensitivity
- `r` = diagnostic test specificity

*Outputs:* The output of the function is a list which contains z (the `nsites`-length vector of true occurrence at each site), w (the `nsites` x `nindiv` matrix of the true infection state of each individual), y (the `nsites` x `nindiv` x 3 array of the observed detection state of each individual's tests), and the values of the input parameters.

