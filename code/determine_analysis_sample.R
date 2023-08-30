# This script determines what will be included in analyses

# generates meets_inclusion_criteria dataframe with 3 columns:
# 1. sub
# 2. has_3goodruns_b20 - does child have 3 good runs if censoring based all blocks (1 = yes, 0 = no)
# 3. has_3goodruns_f20 - does child have 3 good runs if censoring based on food blocks (1 = yes, 0 = no)
# 4. has_3meals - does child have overall intake data for at least 3 meals (1 = yes, 0 = no)
# 5. has_4meals - does child have overall intake data for 4 meals (1 = yes, 0 = no)


#### Setup ####

# Setup data
source("code/setup_data.R")

#### Determine number of "good" fmri runs based on motion ####

# for each sub, determine number of runs with <20% of TRs censored in food and office blocks
good_run_n_blockcensor <- mot_sum %>%
  group_by(sub) %>%
  summarize(n_good_runs_b20 = sum(p_censor_blocks < 20))

# for each sub, determine number of runs with <20% of TRs censored in food blocks
good_run_n_foodcensor <- mot_sum %>%
  group_by(sub) %>%
  summarize(n_good_runs_f20 = sum(p_censor_food < 20))

good_run_count <- merge(good_run_n_blockcensor, good_run_n_foodcensor, by = "sub")

# pad sub in run_count with zeros
good_run_count$sub <- sprintf("%03d", good_run_count$sub)

#### Determine number of meals ####

# Group by "sub" and summarize the counts
meal_count <- intake_long %>%
  group_by(sub) %>%
  summarize(count = sum(!is.na(grams)))

# pad sub in meal_count with zeros
meal_count$sub <- sprintf("%03d", meal_count$sub)


#### Generate inclusion database ####

# make dataframe with "sub" column
meets_inclusion_criteria <- data.frame(sub = unique(mot_sum$sub)) # extract unique subs in mot_sum
meets_inclusion_criteria$sub <- as.character(meets_inclusion_criteria$sub) # make string
meets_inclusion_criteria$sub <- str_pad(meets_inclusion_criteria$sub, width = 3, pad = "0", side = "left") # pad with zeros

#### Fill in inclusion database (1 = meets criteria, 0 = doesn't) ####

for (i in 1:nrow(meets_inclusion_criteria)) {
  sub_value <- meets_inclusion_criteria$sub[i]

  # check for >2 good runs based on n_good_runs_b20
  if ( sub_value %in% good_run_count$sub ) {
    if (good_run_count$n_good_runs_b20[good_run_count$sub == sub_value] > 2) {
      meets_inclusion_criteria$has_3goodruns_b20[meets_inclusion_criteria$sub == sub_value] <- 1
    } else {
      meets_inclusion_criteria$has_3goodruns_b20[meets_inclusion_criteria$sub == sub_value] <- 0
    }
  } else {
        meets_inclusion_criteria$has_3goodruns_b20[meets_inclusion_criteria$sub == sub_value] <- 0
  }

  # check for >2 good runs based on n_good_runs_f20
  if ( sub_value %in% good_run_count$sub ) {
    if (good_run_count$n_good_runs_f20[good_run_count$sub == sub_value] > 2) {
      meets_inclusion_criteria$has_3goodruns_f20[meets_inclusion_criteria$sub == sub_value] <- 1
    } else {
      meets_inclusion_criteria$has_3goodruns_f20[meets_inclusion_criteria$sub == sub_value] <- 0
    }
  } else {
    meets_inclusion_criteria$has_3goodruns_f20[meets_inclusion_criteria$sub == sub_value] <- 0
  }

  # check for data for >2 meals
  if (meal_count$count[meal_count$sub == sub_value] > 2) {
    meets_inclusion_criteria$has_3meals[meets_inclusion_criteria$sub == sub_value] <- 1
  } else {
    meets_inclusion_criteria$has_3meals[meets_inclusion_criteria$sub == sub_value] <- 0
  }

  # check for data for >3 meals
  if (meal_count$count[meal_count$sub == sub_value] > 3) {
    meets_inclusion_criteria$has_4meals[meets_inclusion_criteria$sub == sub_value] <- 1
  } else {
    meets_inclusion_criteria$has_4meals[meets_inclusion_criteria$sub == sub_value] <- 0
  }

}

# Create a new variable "overall_include" based on has_3meals and has_3goodruns_f20
meets_inclusion_criteria$overall_include <- ifelse(meets_inclusion_criteria$has_3meals == 1 & meets_inclusion_criteria$has_3goodruns_f20 == 1, 1, 0)


