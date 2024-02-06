# FoodBrainStudy_fMRI-intake

This project contains code for the paper "Cerebellar response to visual portion size cues is associated with the portion size effect in children (Fuchs et al.)"

\*\* this document is in progress \*\*

## Folder Structure

### code/

This folder contains .R and .Rmd files used for analyses and plots

In this folder:

-   setup_data.R : imports and organizes raw data (sourced by determine_analysis_sample.R)
-   determine_analysis_sample.R : generates dataframe to determine which subjects meet criteria for analyses (sourced by feis_portionsize.R)
-   feis_portionsize.R : extracts individual-level portion size slopes using FEIS models (sourced by gen_fmri_covtable.R)
-   gen_fmri_covtable.R : generates a table with predictors and covariates for fmri analyses. exports table as csv into /BIDS/derivatives/analyses/foodcue-paper2/R/ (sourced by gen_fmri_index.R)
-   gen_fmri_index.R : generates "index files" that list subjects to include in fmri analyses. exports data as .txt files into /BIDS/derivatives/analyses/foodcue-paper2/R/ (sourced by analyze_descriptives.Rmd, analyze_beh.Rmd)
-   analyze_descriptives.Rmd: generates descriptive statistics for fmri predictors/covariates and demographics
-   analyze_beh.Rmd: plots and analyzes in-scanner behavioral data (percent wanting)
-   cerebellum-intake_plot.Rmd: plots the association between cerebellar BOLD responses to portion size and quadratic portion size slopes

### data/

#### data/raw

This folder contains data raw data used as input for code in R/

-   FILE: {description}

### BIDS/code

This folder contains code to (1) process in-scanner wanting data and (2) process and analyze fMRI data

#### BIDS/code/foodcue_proc

This folder contains code used to ....

#### BIDS/code/afni/proc_scripts

This folder contains code used to process MRI data following fmriprep

-   1_smooth-scale: smooths (blurs) and scales fmri data
-   2_createmask: creates overall foodcue mask using separate foodcue run masks
-   3_3ddeconvolve: runs first-level GLM

#### BIDS/code/afni/groupanalyses_paper2

This folder contains code to run group-level analyses.

-   3dcalc_clust_mask: {description}
-   G0_copylevel1: copies level-1 GLM files in paper-specific analysis folder
-   G1_feis: runs regressions (via 3dttest++) predicting BOLD responses to food amount from portion size slopes and covariates
-   G1_feis.PBS and G1_feis.slurm: these run G1_feis on Penn State computing clusters via PBS and SLURM job schedulers 
-   G2_extract: extract beta values for the cerebellum cluster with activation to Large-Small portions associated with quadratic portion size slopes 
-   G3_extract.PBS: runs G2_extract on Penn State computing cluster via PBS job scheduler

### BIDS/derivatives/

This folder contains derivative files used as input for code in code/

