# This script determines what will be included in analyses

# generates meets_inclusion_criteria dataframe with 3 columns:
# 1. sub
# 2. has_3goodruns_b20 - does child have 3 good runs after censoring runs
#                        with >=20% TRs censored in task (food and office) blocks (1 = yes, 0 = no)
# 3. has_3meals - does child have overall intake data for at least 3 meals (1 = yes, 0 = no)
# 4. has_4meals - does child have overall intake data for 4 meals (1 = yes, 0 = no)


#### Setup ####

# Setup data
source("code/setup_data.R")

#### Determine number of "good" fmri runs based on motion ####

# for each sub, determine number of runs with <20% of TRs censored in food and office blocks
good_run_count <- mot_sum %>%
  group_by(sub) %>%
  summarize(n_good_runs_b20 = sum(p_censor_blocks < 20))

#### Determine number of meals ####

# Group by "sub" and summarize the counts
meal_count <- intake_long %>%
  group_by(sub) %>%
  summarize(count = sum(!is.na(grams)))


#### Generate inclusion database ####

# make dataframe with "sub" column
meets_inclusion_criteria <- data.frame(sub = unique(mot_sum$sub)) # extract unique subs in mot_sum

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
meets_inclusion_criteria$overall_include_b20 <- ifelse(meets_inclusion_criteria$has_3meals == 1 & meets_inclusion_criteria$has_3goodruns_b20 == 1, 1, 0)


