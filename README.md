# Simulation Code for “Causal inference targeting a concentration index for studies of health inequalities”

This repository contains the R code used to reproduce the simulation results presented in the paper Ghasempour, M., de Luna, X. and Gustafsson, P.E. (2024).[*“Causal inference targeting a concentration index for studies of health inequalities.”*](https://doi.org/10.48550/arXiv.2410.08849)  
The simulation evaluates the performance of the proposed estimators under different model specifications.

---

## Contents

The repository includes the following R scripts:

1. **Sim_par_job.R** – Main simulation script. Runs the simulations for a given sample size and saves the output files.  
2. **contrast_sim_utility.R** – Contains supporting functions required by the main simulation script.  
3. **post_processing.R** – Processes and summarizes the simulation results into separate CSV files.

---

## Instructions for use

1. Ensure that the files `Sim_par_job.R` and `contrast_sim_utility.R` are located in the **same directory**.

2. Open **R** (or **RStudio**) and set the working directory to the folder containing these files:  
   ```r
   setwd("path/to/your/folder")
   ```
3. To run the simulation, open the file `Sim_par_job.R`.
   Inside the script, specify:
   ```r
   path <- "path/to/save/results/"
   n <- 1000
   r <- 1000
   ```
   Then run:
   ```r
   source("Sim_par_job.R")
   ```
   The simulation results will be generated and stored in the specified directory.

4. When the simulation has finished, open the post-processing script and specify the same sample size `n` and the number of replications `r` used in the simulation.
   ```r
   n <- 1000
   r <- 1000
   ```
   After setting `n` and `r`, run:
   ```r
   source("post_processing.R")
   ```
   This script creates separate CSV files containing the summarized results for:
   - G(0)

   - G(1)

   - G(2)

   - and the contrasts between them.

   The resulting CSV files correspond to the tables reported in the paper.

## Additional notes

- All required R packages should be installed prior to running the code. The list of packages is provided at the beginning of each script.

- The results are automatically saved in the directory specified by the user in the `path` variable.

## Citation
If you use this code, please cite the associated paper:
> Ghasempour, M., de Luna, X., & Gustafsson, P. E. (2024).  
> *Causal inference targeting a concentration index for studies of health inequalities.*  
> [arXiv:2410.08849](https://arxiv.org/abs/2410.08849)

