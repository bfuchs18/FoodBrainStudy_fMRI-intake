# This script generates a table for quad_sign*vertex analyses in AFNI (3dMVM)

#### Setup ####

# load packages
#library(data.table)
library(mice) # for imputing missing pre-mri fullness value

# source data_org.R and feis_portionsize.R
source("code/gen_fmri_index.R") # this sources setup_data.R, determine_analysis_sample.R, feis_portionsize.R, gen_fmri_covtable.R

#### Generate 3dMVM table ####

# copy fmri_covariates to mvm_dataframe
mvm_dataframe <- fmri_covariates

# change sub to Subj
colnames(mvm_dataframe)[which(names(mvm_dataframe) == "sub")] <- "Subj"

# add InputFile column
mvm_dataframe <- mvm_dataframe %>%
  mutate(InputFile = paste0("/storage/group/klk37/default/R01_Food_Brain_Study/BIDS/derivatives/analyses/foodcue-paper2/level1/sub-",
                            Subj, "/ped_fd-0.9_b20_noGSR/stats.sub-", Subj, "+tlrc'[Large-Small_allED_GLT]'"))

# use labels for quad_sign variables
mvm_dataframe$quad_sign_gram <- ifelse(mvm_dataframe$quad_sign_gram == 0, "neg", ifelse(mvm_dataframe$quad_sign_gram == 1, "pos", mvm_dataframe$quad_sign_gram))
mvm_dataframe$quad_sign_kcal <- ifelse(mvm_dataframe$quad_sign_kcal == 0, "neg", ifelse(mvm_dataframe$quad_sign_kcal == 1, "pos", mvm_dataframe$quad_sign_kcal))

#### Subset Subjs for analyses ####

# remove subs w/out vertex estimates (g_vertex = -999) and g vertex outliers ("094", "129", "043", "121", "089")
mvm_dataframe_g_cerebellum <- mvm_dataframe %>% filter(g_vertex != -999, !Subj %in% c("094", "129", "043", "121", "089"))

# remove subs w/out quad estimates (kcal_vertex = -999) and kcal vertex outliers ("109", "126", "043")
mvm_dataframe_kcal_cerebellum <- mvm_dataframe %>% filter(kcal_vertex != -999, !Subj %in% c("109", "126", "043"))

# remove subs w/out cortical coverage ("105", "119")
mvm_dataframe_g_appetitive <- mvm_dataframe_g_cerebellum %>% filter(!Subj %in% c("105", "119"))
mvm_dataframe_kcal_appetitive <- mvm_dataframe_kcal_cerebellum %>% filter(!Subj %in% c("105", "119"))

#### Add \ column #### -- might not be necessary if calling table from script, rather than embedding table into script
# mvm_dataframe_g_cerebellum <- cbind(mvm_dataframe_g_cerebellum, `\\` = rep("\\"))


#### Export database to BIDS for use in imaging analyses ####
write.table(mvm_dataframe_g_cerebellum, "BIDS/derivatives/analyses/foodcue-paper2/R/mvm_dataframe_g_cerebellum.csv", sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "ASCII")
write.table(mvm_dataframe_kcal_cerebellum, "BIDS/derivatives/analyses/foodcue-paper2/R/mvm_dataframe_kcal_cerebellum.csv", sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "ASCII")
write.table(mvm_dataframe_g_appetitive, "BIDS/derivatives/analyses/foodcue-paper2/R/mvm_dataframe_g_appetitive.csv", sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "ASCII")
write.table(mvm_dataframe_kcal_appetitive, "BIDS/derivatives/analyses/foodcue-paper2/R/mvm_dataframe_kcal_appetitive.csv", sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "ASCII")

