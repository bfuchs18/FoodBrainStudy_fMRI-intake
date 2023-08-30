# This script generates index files (tab separated files) that specify who to include in analyses

### For inclusion, subjects must have acceptable fMRI data and not have missing data
### for predictor or covariates in a given model

#### Setup ####

# source data_org.R and feis_portionsize.R
source("code/gen_fmri_covtable.R") # this sources setup_data.R, feis_portionsize.R, and determine_analysis_sample.R

#### Update inclusion database (1 = meets criteria, 0 = doesn't) ####


for (i in 1:nrow(meets_inclusion_criteria)) {
  sub_value <- meets_inclusion_criteria$sub[i]

  # check for linear model estimates
  if (fmri_covariates$l_grams_int[fmri_covariates$sub == sub_value] != -999) {
    meets_inclusion_criteria$has_lin_feis[meets_inclusion_criteria$sub == sub_value] <- 1
  } else {
    meets_inclusion_criteria$has_lin_feis[meets_inclusion_criteria$sub == sub_value] <- 0
  }

  # check for quad model estimates
  if (fmri_covariates$q_grams_int[fmri_covariates$sub == sub_value] != -999) {
    meets_inclusion_criteria$has_quad_feis[meets_inclusion_criteria$sub == sub_value] <- 1
  } else {
    meets_inclusion_criteria$has_quad_feis[meets_inclusion_criteria$sub == sub_value] <- 0
  }

  # check for CEBQ_SR values
  if (fmri_covariates$cebq_sr[fmri_covariates$sub == sub_value] != -999) {
    meets_inclusion_criteria$has_sr[meets_inclusion_criteria$sub == sub_value] <- 1
  } else {
    meets_inclusion_criteria$has_sr[meets_inclusion_criteria$sub == sub_value] <- 0
  }

  # check for FS_FR values
  if (fmri_covariates$cebq_fr[fmri_covariates$sub == sub_value] != -999) {
    meets_inclusion_criteria$has_fr[meets_inclusion_criteria$sub == sub_value] <- 1
  } else {
    meets_inclusion_criteria$has_fr[meets_inclusion_criteria$sub == sub_value] <- 0
  }

  # check for covariates
  if (fmri_covariates$pre_mri_ff[fmri_covariates$sub == sub_value] != -999 &&
      fmri_covariates$cams_pre_mri[fmri_covariates$sub == sub_value] != -999 &&
      fmri_covariates$fd_avg_allruns[fmri_covariates$sub == sub_value] != -999) {
    meets_inclusion_criteria$has_covar[meets_inclusion_criteria$sub == sub_value] <- 1
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
# _fr_ = indicates analysis with food responsiveness
# _sr_ = indicates analysis with satiety responsiveness
# _app = indicates analysis with appetitive mask
# _cer = indicates analysis with cerebellum mask
# _b20 = indicates censoring was based on all blocks
# _b20 = indicates censoring was based on food blocks


# subset subjects for each analysis -- censoring b20
subset_lin_app_b20 <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_b20 == 1  &
                                                meets_inclusion_criteria$has_covar == 1 &
                                                meets_inclusion_criteria$cortical_fov == 1 &
                                                meets_inclusion_criteria$has_lin_feis == 1, ]

subset_quad_app_b20 <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_b20 == 1  &
                                                  meets_inclusion_criteria$has_covar == 1 &
                                                  meets_inclusion_criteria$cortical_fov == 1 &
                                                  meets_inclusion_criteria$has_quad_feis == 1, ]

subset_fr_app_b20 <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_b20 == 1  &
                                                  meets_inclusion_criteria$has_covar == 1 &
                                                  meets_inclusion_criteria$cortical_fov == 1 &
                                                  meets_inclusion_criteria$has_fr == 1, ]

subset_sr_app_b20 <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_b20 == 1  &
                                                 meets_inclusion_criteria$has_covar == 1 &
                                                 meets_inclusion_criteria$cortical_fov == 1 &
                                                 meets_inclusion_criteria$has_sr == 1, ]


subset_lin_cer_b20 <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_b20 == 1  &
                                                             meets_inclusion_criteria$has_covar == 1 &
                                                             meets_inclusion_criteria$has_lin_feis == 1, ]

subset_quad_cer_b20 <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_b20 == 1  &
                                                              meets_inclusion_criteria$has_covar == 1 &
                                                              meets_inclusion_criteria$has_quad_feis == 1, ]

subset_fr_cer_b20 <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_b20 == 1  &
                                                            meets_inclusion_criteria$has_covar == 1 &
                                                            meets_inclusion_criteria$has_fr == 1, ]

subset_sr_cer_b20 <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_b20 == 1  &
                                                            meets_inclusion_criteria$has_covar == 1 &
                                                            meets_inclusion_criteria$has_sr == 1, ]

# subset subjects for each analysis -- censoring f20

subset_lin_app_f20 <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_f20 == 1  &
                                                 meets_inclusion_criteria$has_covar == 1 &
                                                 meets_inclusion_criteria$cortical_fov == 1 &
                                                 meets_inclusion_criteria$has_lin_feis == 1, ]

subset_quad_app_f20<- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_f20 == 1  &
                                                  meets_inclusion_criteria$has_covar == 1 &
                                                  meets_inclusion_criteria$cortical_fov == 1 &
                                                  meets_inclusion_criteria$has_quad_feis == 1, ]

subset_fr_app_f20<- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_f20 == 1  &
                                                meets_inclusion_criteria$has_covar == 1 &
                                                meets_inclusion_criteria$cortical_fov == 1 &
                                                meets_inclusion_criteria$has_fr == 1, ]

subset_sr_app_f20 <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_f20 == 1  &
                                                meets_inclusion_criteria$has_covar == 1 &
                                                meets_inclusion_criteria$cortical_fov == 1 &
                                                meets_inclusion_criteria$has_sr == 1, ]


subset_lin_cer_f20 <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_f20 == 1  &
                                                 meets_inclusion_criteria$has_covar == 1 &
                                                 meets_inclusion_criteria$has_lin_feis == 1, ]

subset_quad_cer_f20 <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_f20 == 1  &
                                                  meets_inclusion_criteria$has_covar == 1 &
                                                  meets_inclusion_criteria$has_quad_feis == 1, ]

subset_fr_cer_f20 <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_f20 == 1  &
                                                meets_inclusion_criteria$has_covar == 1 &
                                                meets_inclusion_criteria$has_fr == 1, ]

subset_sr_cer_f20 <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns_f20 == 1  &
                                                meets_inclusion_criteria$has_covar == 1 &
                                                meets_inclusion_criteria$has_sr == 1, ]

# make tab-separated lists
index_lin_app_b20 <- paste(subset_lin_app_b20$sub, collapse="\t")
index_quad_app_b20 <- paste(subset_quad_app_b20$sub, collapse="\t")
index_fr_app_b20 <- paste(subset_fr_app_b20$sub, collapse="\t")
index_sr_app_b20 <- paste(subset_sr_app_b20$sub, collapse="\t")
index_lin_cer_b20 <- paste(subset_lin_cer_b20$sub, collapse="\t")
index_quad_cer_b20 <- paste(subset_quad_cer_b20$sub, collapse="\t")
index_fr_cer_b20 <- paste(subset_fr_cer_b20$sub, collapse="\t")
index_sr_cer_b20 <- paste(subset_sr_cer_b20$sub, collapse="\t")

index_lin_app_f20 <- paste(subset_lin_app_f20$sub, collapse="\t")
index_quad_app_f20 <- paste(subset_quad_app_f20$sub, collapse="\t")
index_fr_app_f20 <- paste(subset_fr_app_f20$sub, collapse="\t")
index_sr_app_f20 <- paste(subset_sr_app_f20$sub, collapse="\t")
index_lin_cer_f20 <- paste(subset_lin_cer_f20$sub, collapse="\t")
index_quad_cer_f20 <- paste(subset_quad_cer_f20$sub, collapse="\t")
index_fr_cer_f20 <- paste(subset_fr_cer_f20$sub, collapse="\t")
index_sr_cer_f20 <- paste(subset_sr_cer_f20$sub, collapse="\t")

# write tab-separated lists
writeLines(index_lin_app_b20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_lin_appetitive_b20.txt")
writeLines(index_quad_app_b20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_quad_appetitive_b20.txt")
writeLines(index_fr_app_b20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_fr_appetitive_b20.txt")
writeLines(index_sr_app_b20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_sr_appetitive_b20.txt")
writeLines(index_lin_cer_b20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_lin_cerebellum_b20.txt")
writeLines(index_quad_cer_b20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_quad_cerebellum_b20.txt")
writeLines(index_fr_cer_b20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_fr_cerebellum_b20.txt")
writeLines(index_sr_cer_b20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_sr_cerebellum_b20.txt")

writeLines(index_lin_app_f20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_lin_appetitive_f20.txt")
writeLines(index_quad_app_f20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_quad_appetitive_f20.txt")
writeLines(index_fr_app_f20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_fr_appetitive_f20.txt")
writeLines(index_sr_app_f20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_sr_appetitive_f20.txt")
writeLines(index_lin_cer_f20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_lin_cerebellum_f20.txt")
writeLines(index_quad_cer_f20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_quad_cerebellum_f20.txt")
writeLines(index_fr_cer_f20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_fr_cerebellum_f20.txt")
writeLines(index_sr_cer_f20, "BIDS/derivatives/analyses/foodcue-paper2/R/index_sr_cerebellum_f20.txt")



