# Reproducibility Materials

[![R version](https://img.shields.io/badge/R-4.4.1-blue.svg)](https://cran.r-project.org/)
[![Reproducible Environment: renv](https://img.shields.io/badge/reproducible%20environment-renv-lightgreen.svg)](https://rstudio.github.io/renv/)
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-orange.svg)](https://creativecommons.org/licenses/by/4.0/)
[![Formatted with air v0.7.1](https://img.shields.io/badge/formatted%20with-air%200.7.1-purple)](https://github.com/posit-dev/air)


This repository contains the data and R code necessary to replicate the findings
of the simulation and real data application of the study

> G-computation for causal effect estimation from observational hierarchical 
data with unmeasured cluster context


## Directories & files descriptions

```yaml
├── analysis-figures/        # Figures generated from the real-data application
├── LICENSE.md               # CCBY 4.0 License
├── mics_raw_data/           # Raw MICS survey data files (child and women modules)
|
├── R/                       # R scripts for simulations, application, and functions
│   ├── functions/           # Reusable helper functions shared across scripts
│   ├── haz-erp-application/ # Scripts for the real-data application analysis
│   ├── renv-clear.R         # Lists extra packages to retain during renv cleanup
│   ├── simulations/         # Simulation scripts organized by scenario
│   └── simulations-result/  # Processed simulation outputs for summary and figures
|
├── README.md                # Project documentation
├── renv/                    # Project-local package library managed by renv
├── renv.lock                # Exact package versions for reproducibility
|
├── simulation-figures/      # Figures generated from simulation study results
└── within-group-gcomp.Rproj # RStudio project file
```

**Note:** Raw MICS data required to replicate the findings of real data application 
is not included here in this repository to save space. Instructions to get them 
are provided in [`mics_raw_data/README.md`](mics_raw_data/README.md).
