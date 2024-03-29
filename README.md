# FoodBrainStudy_fMRI-intake

This project contains code for the paper "Cerebellar response to visual portion size cues is associated with the portion size effect in children (Fuchs et al.)"

## Folder Structure

### code/

This folder contains .R and .Rmd files used for demographic/behavioral analyses and plots

In this folder:

-   deidentify_data.R: removes PII from datasets in data/raw/ and copies de-identified data into data/raw_deidentified/
-   setup_data.R: imports and organizes raw data (sourced by determine_analysis_sample.R)
-   determine_analysis_sample.R: generates dataframe to determine which subjects meet criteria for analyses (sourced by feis_portionsize.R)
-   feis_portionsize.R: extracts individual-level portion size slopes using FEIS models (sourced by gen_fmri_covtable.R)
-   gen_fmri_covtable.R: generates a table with predictors and covariates for fMRI analyses. exports table as CSV into /BIDS/derivatives/analyses/foodcue-paper2/R/ (sourced by gen_fmri_index.R)
-   gen_fmri_index.R: generates "index files" that list subjects to include in fMRI analyses. exports data as .txt files into /BIDS/derivatives/analyses/foodcue-paper2/R/ (sourced by analyze_descriptives.Rmd, analyze_beh.Rmd)
-   analyze_descriptives.Rmd: generates descriptive statistics for FMRI predictors/covariates and demographics
-   analyze_beh.Rmd: plots and analyzes in-scanner behavioral data (percent wanting)
-   cerebellum-intake_plot.Rmd: plots (1) association between cerebellar BOLD responses to portion size and quadratic portion size slopes and (2) simulated subject-specific portion size curves

### data/

#### data/raw

This folder would contain data raw data used as input for code in R/ but it will not be shared, as some datasets contain potentially identifiable information. Datasets in this folder have been copied into data/raw_deidentified with potentially identifiable information (visit 1 date, date of birth, race, ethnicity) removed. 

#### data/raw_deidentified

This folder contains raw but de-identified datasets to use as input for code in R/. Using this data will require updating import paths in setup_data.R Files starting with dict- contain metadata for the following datasets:

-   anthro_data.csv: contains anthropometric data 
-   demographics_data.csv: contains demographic data
-   intake_data.csv: contains data from the four portion size meals
-   visit6_data.csv: contains data related to the MRI visit (e.g., pre-mri fullness and anxiety; not fMRI or food-cue task data)
-   FoodAndBrainR01DataP-Scansroar.csv: contains data that indicates whether each fMRI run was initiated 

### BIDS/code

This folder contains code to (1) process in-scanner wanting data and (2) process and analyze fMRI data

#### BIDS/code/foodcue_proc

This folder contains code used to generate derivatives needed for fmri analyses in AFNI (e.g., onset files, censor files).
Scripts that begin with p_ contain functions that can be run for 1 subject at a time.

- p0_getbehavial.py: defines function to generate behavioral (wanting) data from the food-cue task by block
- p1_getonsets.py: defines function to generate onset files that contain onsets for all runs (even those with high motion) 
- p2_create_censor_files.py: defines function to process -desc-confounds_timeseries.tsv files (output from fmriprep). and output regressor and censor files.
- p4a_gen_byrun_onsets.py: defines function to generate onset files that exclude runs with motion above a certain threshold
- p6_calc_avg_motion.py: defines function to calculate and export the average framewise displacement during food-cue task for a subject. exports data into {}
- wrapper-python_paper2.py: runs functions in p0, p1, p2, p4a, p6 for all subjects with food-cue task data in bids/raw_data

#### BIDS/code/afni/

This folder contains code used to process and analyze MRI data following fmriprep

-   gen_sub_scripts_paper2: generates a wrapper file for each subject using templates in template_scripts/. The wrapper will be used to call scripts in proc_scripts/
-   proc_scripts/1_smooth-scale: smooths (blurs) and scales fMRI data (uses derivatives from fmriprep)
-   proc_scripts/2_createmask: creates overall foodcue mask using separate foodcue run masks (uses derivatives from fmriprep)
-   proc_scripts/3_3ddeconvolve: runs first-level GLM (uses derivatives from fmriprep and output from code in BIDS/code/foodcue_proc/)
-   template_scripts/paper2_wrapper-afni: template wrapper
-   template_scripts/paper2_wrapper-afni.pbs: template to run paper2_wrapper-afni on Penn State computing cluster via PBS job scheduler

#### BIDS/code/afni/groupanalyses_paper2

This folder contains code to run group-level analyses.

-   G0_copylevel1: copies level-1 GLM files in paper-specific analysis folder
-   G1_feis: runs regressions (via 3dttest++) predicting BOLD responses to food amount from portion size slopes and covariates
-   G1_feis_sensitivity: runs same regression as G1_feis (BOLD responses ~ quadratic portion size slope) with additional covariate (BMI percentile)
-   G1_feis.PBS and G1_feis.slurm: run G1_feis(_sensitivity) on Penn State computing clusters via PBS and SLURM job schedulers 
-   G2_extract: extract beta values for the cerebellum cluster with activation to Large-Small portions associated with quadratic portion size slopes (identified via G1_feis)
-   G3_extract.PBS: runs G2_extract on Penn State computing cluster via PBS job scheduler

### BIDS/derivatives/

This folder contains derivative files used or generated by code in BIDS/code/

-   preprocessed/beh/task-foodcue_summary.tsv: contains processed behavioral (wanting) data from the food-cue task by block. Generated by BIDS/code/foodcue_proc/p0_getbehavial.py
-   preprocessed/beh/task-foodcue_summary.json: contains metadata for preprocessed/beh/task-foodcue_summary.tsv
-   preprocessed/fmriprep/foodcue-avg_fd.tsv: contains average framewise displacement for each subject. Generated by BIDS/code/foodcue_proc/p6_calc_avg_motion.py
-   analyses/foodcue-paper2/R/: folder with index files and covariate files generated by R code in /code/ and used by code in BIDS/code/afni/groupanalyses_paper2
-   analyses/foodcue-paper2/level2/feis_cerebellum/ped_fd-0.9_b20_3runs_noGSR_09-05-23/fmri_covariates.csv: Covariate file used via G1_feis for analyses
-   analyses/foodcue-paper2/level2/feis_cerebellum/ped_fd-0.9_b20_3runs_noGSR_09-05-23/cerebellum_betas.txt: Larger-Smaller contrast values extract from cerebellum. Generated by BIDS/code/afni/groupanalyses_paper2/G2_extract
-   analyses/foodcue-paper2/masks/appetitive_mask.nii: appetitive network mask generated using SPM PickAtlas toolbox
-   analyses/foodcue-paper2/masks/cerebellum_aal_mask.nii: cerebellum mask generated using SPM PickAtlas toolbox

