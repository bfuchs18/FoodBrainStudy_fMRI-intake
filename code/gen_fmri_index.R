# This script generates index files (tab separated files) that specify who to include in each analysis

### For inclusion, subjects must have =>3 good food cue runs (determined in determine_analysis_sample.R),
### and data for predictor or covariates in a given model. For analyses with appetitive masks,
### subjects must also have reasonable cortical FOV (determined via visual inspection)

#### Setup ####

# source data_org.R and feis_portionsize.R
source("code/gen_fmri_covtable.R") # this sources setup_data.R, feis_portionsize.R, and determine_analysis_sample.R

#### Update inclusion database (1 = meets criteria, 0 = doesn't) ####

# the following steps check for values of eating behavior variables in fmri_covariates

for (i in 1:nrow(meets_inclusion_criteria)) {
  sub_value <- meets_inclusion_criteria$sub[i]

  # check for linear model estimates
  if ( sub_value %in% fmri_covariates$sub ) {
    if (fmri_covariates$l_grams_int[fmri_covariates$sub == sub_value] != -999) {
      meets_inclusion_criteria$has_lin_feis[meets_inclusion_criteria$sub == sub_value] <- 1
    } else {
      meets_inclusion_criteria$has_lin_feis[meets_inclusion_criteria$sub == sub_value] <- 0
    }
  } else {
    meets_inclusion_criteria$has_lin_feis[meets_inclusion_criteria$sub == sub_value] <- 0
  }

  # check for quad model estimates
  if ( sub_value %in% fmri_covariates$sub ) {
    if (fmri_covariates$q_grams_int[fmri_covariates$sub == sub_value] != -999) {
      meets_inclusion_criteria$has_quad_feis[meets_inclusion_criteria$sub == sub_value] <- 1
    } else {
      meets_inclusion_criteria$has_quad_feis[meets_inclusion_criteria$sub == sub_value] <- 0
    }
  } else {
    meets_inclusion_criteria$has_quad_feis[meets_inclusion_criteria$sub == sub_value] <- 0
  }

  # check for covariates
  if ( sub_value %in% fmri_covariates$sub ) {
    if (fmri_covariates$pre_mri_ff[fmri_covariates$sub == sub_value] != -999 &&
        fmri_covariates$cams_pre_mri[fmri_covariates$sub == sub_value] != -999 &&
        fmri_covariates$bmi_percentile[fmri_covariates$sub == sub_value] != -999 &&
        fmri_covariates$ree[fmri_covariates$sub == sub_value] != -999 &&
        fmri_covariates$fd_avg_allruns[fmri_covariates$sub == sub_value] != -999) {
      meets_inclusion_criteria$has_covar[meets_inclusion_criteria$sub == sub_value] <- 1
    } else {
      meets_inclusion_criteria$has_covar[meets_inclusion_criteria$sub == sub_value] <- 0
    }
  } else {
    meets_inclusion_criteria$has_covar[meets_inclusion_criteria$sub == sub_value] <- 0
  }

}

#### Add cortical_fov to meets_inclusion_criteria ####

# indicates whether cortical FOV is acceptable

# 105 and 119 have extreme FOV cut-off in fmri data
meets_inclusion_criteria$cortical_fov <- ifelse(meets_inclusion_criteria$sub %in% c("105", "119"), 0, 1)

#### Make index files ####

# _lin_ = indicates analysis with parameters from linear FEIS models
# _quad_ = indicates analysis with parameters from quadratic FEIS models
# _app = indicates analysis with appetitive mask
# _cer = indicates analysis with cerebellum mask
# _b20 = indicates runs censored if >=20% TRs censored across task (food and office) blocks



# subset subjects for each analysis -- censoring b20
subset_lin_app_b20 <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_b20 == 1  &
                                                meets_inclusion_criteria$has_covar == 1 &
                                                meets_inclusion_criteria$cortical_fov == 1 &
                                                meets_inclusion_criteria$has_lin_feis == 1, ]

subset_quad_app_b20 <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_b20 == 1  &
                                                  meets_inclusion_criteria$has_covar == 1 &
                                                  meets_inclusion_criteria$cortical_fov == 1 &
                                                  meets_inclusion_criteria$has_quad_feis == 1, ]

subset_lin_cer_b20 <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_b20 == 1  &
                                                             meets_inclusion_criteria$has_covar == 1 &
                                                             meets_inclusion_criteria$has_lin_feis == 1, ]

subset_quad_cer_b20 <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_b20 == 1  &
                                                              meets_inclusion_criteria$has_covar == 1 &
                                                              meets_inclusion_criteria$has_quad_feis == 1, ]

## subset fmri covariates to only include subjects included in quadratic analyses
subset_fmri_covariates_quad_app <- fmri_covariates[fmri_covariates$sub %in% subset_quad_app_b20$sub,]
subset_fmri_covariates_quad_cer <- fmri_covariates[fmri_covariates$sub %in% subset_quad_cer_b20$sub,]

# make tab-separated lists
index_lin_app_b20 <- paste(subset_lin_app_b20$sub, collapse="\t")
index_quad_app_b20 <- paste(subset_quad_app_b20$sub, collapse="\t")
index_lin_cer_b20 <- paste(subset_lin_cer_b20$sub, collapse="\t")
index_quad_cer_b20 <- paste(subset_quad_cer_b20$sub, collapse="\t")

# write tab-separated lists
writeLines(index_lin_app_b20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_lin_appetitive_fd-0.9_b20_3runs.txt")
writeLines(index_quad_app_b20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_quad_appetitive_fd-0.9_b20_3runs.txt")
writeLines(index_lin_cer_b20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_lin_cerebellum_fd-0.9_b20_3runs.txt")
writeLines(index_quad_cer_b20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_quad_cerebellum_fd-0.9_b20_3runs.txt")
