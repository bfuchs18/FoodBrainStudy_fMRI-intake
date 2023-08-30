# This script report was written by Alaina Pearce and Bari Fuchs in August 2022 to
# extract individual portion size effect estimates.
#
#     Copyright (C) 2022 Bari Fuchs
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.

############ Basic Data Load/Setup ############

library(feisr)
library(lme4)
library(lmerTest)
library(dplyr)

# set project directory
source("code/determine_analysis_sample.R") # sources setup_data.R

############ subset intake_long for children included in fMRI analyses ############

intake_long_analyze <- intake_long[intake_long$sub %in% meets_inclusion_criteria$sub[meets_inclusion_criteria$overall_include == 1], ]

############ fixed effects individual slope models (feis) ############

#### estimate linear slope only ####

# predict overall intake (g) - linear term
grams_ps_feis <- feis(grams ~ preFF + avg_vas + meal_order | ps_prop , data = intake_long_analyze, id = "sub")
grams_ps_feis_data <- data.frame(row.names(slopes(grams_ps_feis)), slopes(grams_ps_feis))
names(grams_ps_feis_data) <- c('sub', 'l_grams_int', 'l_grams_ps_lin')

# predict overall intake (kcal) - linear term
kcal_ps_feis <- feis(kcal ~ preFF + avg_vas + meal_order | ps_prop , data = intake_long_analyze, id = "sub")
kcal_ps_feis_data <- data.frame(row.names(slopes(kcal_ps_feis)), slopes(kcal_ps_feis))
names(kcal_ps_feis_data) <- c('sub', 'l_kcal_int', 'l_kcal_ps_lin')

#### estimate linear and quadratic slopes ####

# predict overall intake (g) - linear and quad term
grams_ps_psquad_feis <- feis(grams ~ preFF + avg_vas + meal_order | ps_prop + I(ps_prop*ps_prop), data = intake_long_analyze, id = "sub")
grams_ps_psquad_feis_data <- data.frame(row.names(slopes(grams_ps_psquad_feis)), slopes(grams_ps_psquad_feis))
names(grams_ps_psquad_feis_data) <- c('sub', 'q_grams_int', 'q_grams_ps_lin', 'q_grams_ps_quad')

# predict overall intake (kcal) - linear and quad term
kcal_ps_psquad_feis <- feis(kcal ~ preFF + avg_vas + meal_order | ps_prop + I(ps_prop*ps_prop), data = intake_long_analyze, id = "sub")
kcal_ps_psquad_feis_data <- data.frame(row.names(slopes(kcal_ps_psquad_feis)), slopes(kcal_ps_psquad_feis))
names(kcal_ps_psquad_feis_data) <- c('sub', 'q_kcal_int', 'q_kcal_ps_lin', 'q_kcal_ps_quad')

#### Calculate vertex ####

# *To calculate effect of portion size by 0.33 proportion increase need to first get total quadratic effect.
# The $\beta$ coefficient for a quadratic effect is half the change in the linear slope for a unit increase,
# so total change in linear slope = 2 x ps_prop2. Since a 1 unit increase = 100% increase in portion, can then
# multiply the total effect by 0.33.
# Therefore, change in linear slope for each 33% increase in amount served = (ps_prop2 x 2) x 0.33.
# To calculate where the slope switches from positive to negative, need to find the vertex = -ps_prop/(ps_prop2 x 2)

grams_ps_psquad_feis_data$g_vertex <- -(grams_ps_psquad_feis_data$q_grams_ps_lin)/(grams_ps_psquad_feis_data$q_grams_ps_quad*2)
kcal_ps_psquad_feis_data$kcal_vertex <- -(kcal_ps_psquad_feis_data$q_kcal_ps_lin)/(kcal_ps_psquad_feis_data$q_kcal_ps_quad*2)


############ merge datasets ############

# merge datasets with all = T to include all rows in both dataframes
intake_feis_data <- merge(grams_ps_feis_data, kcal_ps_feis_data, by = 'sub', all = TRUE)
intake_feis_data <- merge(intake_feis_data, grams_ps_psquad_feis_data, by = 'sub', all = TRUE)
intake_feis_data <- merge(intake_feis_data, kcal_ps_psquad_feis_data, by = 'sub', all = TRUE)

