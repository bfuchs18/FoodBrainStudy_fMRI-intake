# This script generates a table of predictors/covariates for fMRI analyses in AFNI including:
# FEIS estimates
# CEBQ
# imaging covariates: sex, average FD, pre-mri fullness, pre-mri CAMS

#### Setup ####

# load packages
#library(data.table)
library(mice) # for imputing missing pre-mri fullness value

# source data_org.R and feis_portionsize.R
source("code/feis_portionsize.R") # this sources data_org

# import average framewise displacement
fd <- read.delim("BIDS/derivatives/preprocessed/fmriprep/task-foodcue_avg-fd.tsv")
names(fd)[names(fd) == "id"] <- "sub"

#### Add FEIS and CEBQ variables to fmri_covariates ####

fmri_covariates <- merge(intake_feis_data, r01_eatbeh[, c("sub", "cebq_fr", "cebq_sr")], by = "sub", all.x = TRUE)

#### Add sex ####

fmri_covariates <- merge(fmri_covariates, r01_eatbeh[, c("sub", "sex")], by = "sub", all.x = TRUE)

#### Add pre-MRI fullness ####

## generate pre_mri fullness values
r01_V6$pre_mri_ff <- NA
r01_V6$pre_mri_snack <- NA

for (i in 1:nrow(r01_V6)) {
  if(!is.na(r01_V6$ff_postmri_snack2[i])){
    r01_V6$pre_mri_ff[i] <- r01_V6$ff_postmri_snack2[i]
    r01_V6$pre_mri_snack[i] <- "yes"
  }
  else if(!is.na(r01_V6$ff_postmri_snack[i])) {
    r01_V6$pre_mri_ff[i] <- r01_V6$ff_postmri_snack[i]
    r01_V6$pre_mri_snack[i] <- "yes"
  }
  else if(!is.na(r01_V6$ff_premri_snack[i])) {
    r01_V6$pre_mri_ff[i] <- r01_V6$ff_premri_snack[i]
    r01_V6$pre_mri_snack[i] <- "no"
  }
}

## impute missing pre_mri fullness value

# make df of variables to impute on
impute_data <- r01_eatbeh[,c('sub','sex', 'age_yr', 'bmi_percentile')]
impute_data <- merge(impute_data, r01_V6[, c("sub", "pre_mri_ff")], by = "sub", all.y = TRUE) # merge on y to only impute for children who attended V6

# Run the mice code with 0 iterations
imp_template <- mice(impute_data, maxit=0)

# Extract predictorMatrix and methods of imputation
predM <- imp_template$predictorMatrix
meth <- imp_template$method

# Setting values of 'sub' variable to 0 in the predictor matrix to leave out of imputation
predM[, c("sub")] <- 0

# Impute the impute_data data, use predM as the predictor matrix and don't print the imputation process
## m is the number of imputations
## maxit is the max number of iterations for each imputation

impute <- mice(impute_data, maxit = 5, m = 1,
               predictorMatrix = predM,
               method = meth, print =  FALSE, seed = 1)

# Extract the imputed data
imputed_complete <- complete(impute, 1)  # Use the first imputed dataset (can be any from 1 to m)

# Extract the column names of variables that were imputed
#imputed_var_names <- colnames(impute_data)[colSums(is.na(impute_data)) > 0]

# Create a new column 'imputed_preff' in 'imputed_complete' to indicate imputation status
imputed_complete$imputed_preff <- 0

# Mark rows in imputed_complete as imputed (imputed_complete$imputed = 1) if they were missing value in impute_data$pre_mri_ff
imputed_complete[imputed_complete$sub %in% impute_data$sub & is.na(impute_data$pre_mri_ff) > 0, "imputed_preff"] <- 1

# add pre_mri_ff and imputed_preff to intake_feis_data
fmri_covariates <- merge(fmri_covariates, imputed_complete[, c("sub", "pre_mri_ff", "imputed_preff")], by = "sub", all.x = TRUE)

# add r01_V6$pre_mri_snack to intake_feis_data
fmri_covariates <- merge(fmri_covariates, r01_V6[, c("sub", "pre_mri_snack")], by = "sub", all.x = TRUE)


#### Add pre-mri CAMS value ####

fmri_covariates <- merge(fmri_covariates, r01_V6[, c("sub", "cams_pre_mri")], by = "sub", all.x = TRUE)

#### Add framewise displacement ####
fmri_covariates <- merge(fmri_covariates, fd[, c("sub", "fd_avg_allruns")], by = "sub", all.x = TRUE)

#### Replace NA with -999 ####
fmri_covariates[is.na(fmri_covariates)] = -999

#### Export database to BIDS for use in imaging analyses ####
write.csv(fmri_covariates, 'BIDS/derivatives/analyses/foodcue-paper2/R/fmri_covariates.csv', row.names = FALSE)


