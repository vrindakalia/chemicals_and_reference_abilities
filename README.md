# Mixture of organic pollutants is associated with cognitive function in healthy adults

This repository contains code for review, associated with the manuscript titled: "Mixture of organic pollutants is associated with cognitive function in healthy adults." 
The study aimed to investigate whether a mixture of chemicals, measured in serum, was associated with cognitive function in a cohort of healthy volunteers.

Before beginning, please run the script titled `directory_set_up.R` to create the necessary folder structure for data storage and movement. 

In the `code` folder, the scripts should be run in the following order:  

1. In the `data_preparation` folder:
* `chemical_exposure_data.R` cleans and performs pre-processing of the chemical exposure data. 
* `chemical_iccs.R` generates the intra-class correlation coefficient for the chemicals detected in at least 50% of the samples
* `outcome_and_covariate_data.R` to clean and merge the outcome and covariate data 

2. In the `data_exploration` folder:  
* `chemical_descriptors.R` produces summary statistics for the chemicals retained for analysis
* `demographics_table1.R` produces Table 1 in the manuscript

3. In the `models` folder: 
* `gee_models.R` runs multiple GEEs for each chemical and outcome of interest
* `bkmr_models.R` runs multiple BKMRs. The analysis was run with 60,000 iterations that took almost an hour per outcome. May want to consider whether should run the script

4. The `output` folder contains results from the models run as well as other files saved to disc in a previous step

5. The `functions` folder

6. The `figures` folder

7. The `packages` folder

The figures used in the manuscript can be found in the "figures"folder

