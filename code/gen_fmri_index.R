# This script generates index files (tab separated files) that specify who to include in analyses

### For inclusion, subjects must have acceptable fMRI data and not have missing data
### for predictor or covariates in a given model

#### Setup ####

# source data_org.R and feis_portionsize.R
source("code/gen_fmricovtable") # this sources setup_data.R and feis_portionsize.R

# import motion summary databases
mot_sum <- read.delim("BIDS/derivatives/preprocessed/fmriprep/task-foodcue_byrun-censorsummary_fd-0.9.tsv")
names(mot_sum)[names(mot_sum) == "id"] <- "sub"

#### Determine number of "good" fmri runs based on motion ####

# define maximum percent of TRs censored in food and non-food blocks (p_censor_interest) for a run to be "good"
censor_thresh = 20

good_run_n <- mot_sum %>%
  group_by(sub) %>%
  summarize(n_good_runs = sum(p_censor_interest < censor_thresh))

#### Generate inclusion database ####

# make dataframe with "sub" column
meets_inclusion_criteria <- data.frame(sub = fmri_covariates$sub)

# fill in meets_inclusion_criteria for each column (1 = meets criteria, 0 = doesn't)

for (i in 1:nrow(meets_inclusion_criteria)) {
  sub_value <- meets_inclusion_criteria$sub[i]

  # check for =>3 good runs
  if ( sub_value %in% good_run_n$sub ) {
    if (good_run_n$n_good_runs[good_run_n$sub == sub_value] > 2) {
      meets_inclusion_criteria$has_3goodruns[meets_inclusion_criteria$sub == sub_value] <- 1
    } else {
      meets_inclusion_criteria$has_3goodruns[meets_inclusion_criteria$sub == sub_value] <- 0
    }
  } else {
        meets_inclusion_criteria$has_3goodruns[meets_inclusion_criteria$sub == sub_value] <- 0
  }

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

#### Add passed_qc to meets_inclusion_criteria ####

# exclude 105 and 119 due to extreme FOV cut-off in fmri data
meets_inclusion_criteria$passed_qc <- ifelse(meets_inclusion_criteria$sub %in% c("105", "119"), 0, 1)

#### Make index files ####

# subset subjects for each analysis
subset_lin_analyses <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns == 1  &
                                                meets_inclusion_criteria$has_covar == 1 &
                                                meets_inclusion_criteria$passed_qc == 1 &
                                                meets_inclusion_criteria$has_lin_feis == 1, ]

subset_quad_analyses <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns == 1  &
                                                  meets_inclusion_criteria$has_covar == 1 &
                                                  meets_inclusion_criteria$passed_qc == 1 &
                                                  meets_inclusion_criteria$has_quad_feis == 1, ]

subset_fr_analyses <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns == 1  &
                                                  meets_inclusion_criteria$has_covar == 1 &
                                                  meets_inclusion_criteria$passed_qc == 1 &
                                                  meets_inclusion_criteria$has_fr == 1, ]

subset_sr_analyses <- meets_inclusion_criteria[meets_inclusion_criteria$has_3goodruns == 1  &
                                                 meets_inclusion_criteria$has_covar == 1 &
                                                 meets_inclusion_criteria$passed_qc == 1 &
                                                 meets_inclusion_criteria$has_sr == 1, ]


# make tab-separated lists
index_lin_analyses <- paste(subset_lin_analyses$sub, collapse="\t")
index_quad_analyses <- paste(subset_quad_analyses$sub, collapse="\t")
index_fr_analyses <- paste(subset_fr_analyses$sub, collapse="\t")
index_sr_analyses <- paste(subset_sr_analyses$sub, collapse="\t")

# write tab-separated lists
writeLines(index_lin_analyses, "BIDS/derivatives/analyses/foodcue-paper2/R/index_lin_analyses.txt")
writeLines(index_quad_analyses, "BIDS/derivatives/analyses/foodcue-paper2/R/index_quad_analyses.txt")
writeLines(index_lin_analyses, "BIDS/derivatives/analyses/foodcue-paper2/R/index_fr_analyses.txt")
writeLines(index_fr_analyses, "BIDS/derivatives/analyses/foodcue-paper2/R/index_sr_analyses.txt")


