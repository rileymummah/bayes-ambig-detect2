# Accounting for ambiguous detections in surveillance data

## Repository Structure

- `03-fit-models-HPC/` contains all code required to run the analysis as an array job on
a SLURM-based high-performance computing cluster
- `data/` contains the generated parameter combinations and simulated
datasets
- `functions/` contains the functions required for the analysis
- `models/` contains the JAGS model text files
- `output/` contains the output for the fitted JAGS models

Each folder contains a README that outlines all files in the folder.

## Workflow

To recreate the simulation study run the following files:
  
  **01-generate-parameter-combos.R**
  
  - Sets the limits on $\psi$, $\theta_{11}$, $p_{111}$, $b_2$, and
$b_3$
  - Uses Latin hypercube sampling to create parameter sets
- Writes parameter combinations to `data/parameter_combos.csv`

**02-simulate-datasets.R**
  
  - Simulates a dataset from each parameter combination using the function
found in `functions/FXN-sim.data.R`
- Saves simulated datasets to `data/sim.datasets.rds`

**03-fit-models**
  
  To fit the models on a local computer, you can run `03-fit-models.R` and
all output will be saved as an .rds file. However, this will takes weeks
to run. To speed up computation time, we ran the models on a high
performance computing cluster. You can find all code used on our
SLURM-based HPC in the `03-fit-models-HPC/` folder.

*Notes for running on an HPC:*
  
  - `03a-submit-job-to-HPC.txt` outlines how jobs were submitted to the
HPC after cloning the repository
- The HPC code results in a series of .csv files, rather than an .rds,
so the files must be combined and saved. This can be done by running
the code found in `03b-combine-output.sh` on the HPC before export.
- Once exported, `03c-post-processing.R` will assemble the model-level
.csv files in R and format the data for plotting. The output will be
saved as `combined-output.rds` and `combined-output.csv`.

**04-plot-output.R**
  
  - Loads the simulation output `output/combined-output.rds`
- Creates and saves Figures 3-5 and S1-S19 to `output/figures/`

