#!/bin/bash

## To combine files: 
# cat *.csv > combined.csv

# Model U
cat output/HPC/modelU*.csv > output/combined-modelU.csv

# Model 1
cat output/HPC/model1*.csv > output/combined-model1.csv

# Model 2
cat output/HPC/model2*.csv > output/combined-model2.csv

# Model3
cat output/HPC/model3*.csv > output/combined-model3.csv


# Move combined-*.csv to local computer


