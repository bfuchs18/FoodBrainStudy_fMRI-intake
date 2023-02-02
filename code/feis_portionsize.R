# This script report was written by Alaina Pearce and Bari Fuchs in August 2022 to
# extract individual poriton size effect estimates.
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

# set project directory
source("code/setup_data.R")

############ fixed effects individual slopes (feis) ############

## individual PS slopes for intake - all foods ####

# feis model - all foods - grams
grams_ps_psquad_feis <- feis(grams ~ preFF + avg_vas + meal_order | ps_prop + I(ps_prop*ps_prop), data = intake_long, id = "sub")
grams_ps_psquad_feis_data <- data.frame(row.names(slopes(grams_ps_psquad_feis)), slopes(grams_ps_psquad_feis))
names(grams_ps_psquad_feis_data) <- c('sub', 'grams_int', 'grams_ps_lin', 'grams_ps_quad')

# feis model - all foods - kcal
kcal_ps_feis <- feis(kcal ~ preFF + avg_vas + meal_order | ps_prop, data = intake_long, id = "sub")
kcal_ps_feis_data <- data.frame(row.names(slopes(kcal_ps_feis)), slopes(kcal_ps_feis))
names(kcal_ps_feis_data) <- c('sub', 'kcal_int', 'kcal_ps_lin')

## individual PS slopes for intake - low ED foods ####

# mixed effects models with and without quadratic term
led_mod <- lmer(led_grams ~ preFF + led_vas + meal_order + ps_prop + (1 | sub), data = intake_long)
led_mod_quad <- lmer(led_grams ~ preFF + led_vas + meal_order + ps_prop + I(ps_prop*ps_prop) + (1 | sub), data = intake_long)
anova(led_mod, led_mod_quad) # test whether adding quadratic improves model fit -- it does not, so do not include in feis

# feis model - low ED foods - grams
led_grams_ps_feis <- feis(led_grams ~ preFF + led_vas + meal_order | ps_prop , data = intake_long, id = "sub")
led_grams_ps_feis_data <- data.frame(row.names(slopes(led_grams_ps_feis)), slopes(led_grams_ps_feis))
names(led_grams_ps_feis_data) <- c('sub', 'led_grams_int', 'led_grams_ps_lin')

# feis model - low ED foods - kcal
led_kcal_ps_feis <- feis(led_kcal ~ preFF + led_vas + meal_order | ps_prop, data = intake_long, id = "sub")
led_kcal_ps_feis_data <- data.frame(row.names(slopes(led_kcal_ps_feis)), slopes(led_kcal_ps_feis))
names(led_kcal_ps_feis_data) <- c('sub', 'led_kcal_int', 'led_kcal_ps_lin')

## individual PS slopes for intake - high ED foods ####

# mixed effects models with and without quadratic term
hed_mod <- lmer(hed_grams ~ preFF + hed_vas + meal_order + ps_prop + (1 | sub), data = intake_long)
hed_mod_quad <- lmer(hed_grams ~ preFF + hed_vas + meal_order + ps_prop + I(ps_prop*ps_prop) + (1 | sub), data = intake_long)
anova(hed_mod, hed_mod_quad) # test whether adding quadratic improves model fit -- it does not, so do not include in feis

# feis model - high ED foods - grams
hed_grams_ps_feis <- feis(hed_grams ~ preFF + hed_vas + meal_order | ps_prop , data = intake_long, id = "sub")
hed_grams_ps_feis_data <- data.frame(row.names(slopes(hed_grams_ps_feis)), slopes(hed_grams_ps_feis))
names(hed_grams_ps_feis_data) <- c('sub', 'hed_grams_int', 'hed_grams_ps_lin')

# feis model - high ED foods - kcal
hed_kcal_ps_feis <- feis(hed_kcal ~ preFF + hed_vas + meal_order | ps_prop, data = intake_long, id = "sub")
hed_kcal_ps_feis_data <- data.frame(row.names(slopes(hed_kcal_ps_feis)), slopes(hed_kcal_ps_feis))
names(hed_kcal_ps_feis_data) <- c('sub', 'hed_kcal_int', 'hed_kcal_ps_lin')

# merge datasets
intake_feis_data <- merge(grams_ps_psquad_feis_data, kcal_ps_feis_data, by = 'sub', all = TRUE) #all = T will include all rows in both dataframes*
intake_feis_data <- merge(intake_feis_data, led_grams_ps_feis_data, by = 'sub', all = TRUE)
intake_feis_data <- merge(intake_feis_data, led_kcal_ps_feis_data, by = 'sub', all = TRUE)
intake_feis_data <- merge(intake_feis_data, hed_grams_ps_feis_data, by = 'sub', all = TRUE)
intake_feis_data <- merge(intake_feis_data, hed_kcal_ps_feis_data, by = 'sub', all = TRUE)

# write database
write.csv(intake_feis_data,"data/generated/intake_feis.csv", row.names = TRUE)

#### * subs 120 and 128 having missing data for 1 meal each, and therefore do not have enough
##    datapoints to be included in grams_ps_psquad_feis (which includes quad param). However,
##    they have enough data to be included in kcal and hed/led models (which do not include quad param)
