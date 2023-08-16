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
source("code/setup_data.R")

############ fixed effects individual slope models (feis) ############

#### estimate linear slope only ####

# predict overall intake (g) - linear term
grams_ps_feis <- feis(grams ~ preFF + avg_vas + meal_order | ps_prop , data = intake_long, id = "sub")
grams_ps_feis_data <- data.frame(row.names(slopes(grams_ps_feis)), slopes(grams_ps_feis))
names(grams_ps_feis_data) <- c('sub', 'l_grams_int', 'l_grams_ps_lin')

# predict overall intake (kcal) - linear term
kcal_ps_feis <- feis(kcal ~ preFF + avg_vas + meal_order | ps_prop , data = intake_long, id = "sub")
kcal_ps_feis_data <- data.frame(row.names(slopes(kcal_ps_feis)), slopes(kcal_ps_feis))
names(kcal_ps_feis_data) <- c('sub', 'l_kcal_int', 'l_kcal_ps_lin')

#### estimate linear and quadratic slopes ####

# predict overall intake (g) - linear and quad term
grams_ps_psquad_feis <- feis(grams ~ preFF + avg_vas + meal_order | ps_prop + I(ps_prop*ps_prop), data = intake_long, id = "sub")
grams_ps_psquad_feis_data <- data.frame(row.names(slopes(grams_ps_psquad_feis)), slopes(grams_ps_psquad_feis))
names(grams_ps_psquad_feis_data) <- c('sub', 'q_grams_int', 'q_grams_ps_lin', 'q_grams_ps_quad')

# predict overall intake (kcal) - linear and quad term
kcal_ps_psquad_feis <- feis(kcal ~ preFF + avg_vas + meal_order | ps_prop + I(ps_prop*ps_prop), data = intake_long, id = "sub")
kcal_ps_psquad_feis_data <- data.frame(row.names(slopes(kcal_ps_psquad_feis)), slopes(kcal_ps_psquad_feis))
names(kcal_ps_psquad_feis_data) <- c('sub', 'q_kcal_int', 'q_kcal_ps_lin', 'q_kcal_ps_quad')

############ merge datasets ############

# merge datasets with all = T to include all rows in both dataframes
intake_feis_data <- merge(grams_ps_feis_data, kcal_ps_feis_data, by = 'sub', all = TRUE)
intake_feis_data <- merge(intake_feis_data, grams_ps_psquad_feis_data, by = 'sub', all = TRUE)
intake_feis_data <- merge(intake_feis_data, kcal_ps_psquad_feis_data, by = 'sub', all = TRUE)

# write database
write.csv(intake_feis_data,"data/generated/intake_feis.csv", row.names = TRUE)

#### subs 120 and 128 having missing data for 1 meal each, and therefore do not have enough
#### datapoints to be included in models w/ quadtratic term
