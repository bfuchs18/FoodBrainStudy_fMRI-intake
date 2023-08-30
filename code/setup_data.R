# This script was written by Alaina Pearce and Bari Fuchs in August 2022
# to set up data for extracting the portion size effect
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

library(haven)
library(reshape2)
#source('functions.R')

#### set up ####

# 1) Demographics Data ####
## a) Load Data ####
r01_demo <- as.data.frame(read_spss(("data/raw/demographics_data.sav")))
names(r01_demo)[1] <- 'sub'

#remove 2 that were removed for ADHD
r01_demo = r01_demo[r01_demo$sub != 31 & r01_demo$sub != 34, ]

# pad with zeros
r01_demo$sub <- sprintf("%03d", r01_demo$sub)

## b) Get Variable Labels and Re-Level ####

# risk status
r01_demo$risk_status_mom <- droplevels(as_factor(r01_demo$risk_status_mom))
r01_demo$risk_status_both <- droplevels(as_factor(r01_demo$risk_status_both))
r01_demo$sex <- as_factor(r01_demo$sex)

# income
r01_demo$income <- ifelse(is.na(r01_demo$income), NA, ifelse(r01_demo$income < 3, '< $51,000', ifelse(r01_demo$income < 5, "$51,000 - $100,000", '>$100,000')))

# parental ed
r01_demo$mom_ed <- ifelse(r01_demo$measured_parent == 0, ifelse(r01_demo$parent_ed == 0, 'High School/GED', ifelse(r01_demo$parent_ed < 3, 'AA/Technical Degree', ifelse(r01_demo$parent_ed == 3, 'Bachelor Degree', ifelse(r01_demo$parent_ed < 8, '> Bachelor Degree', 'Other/NA')))), ifelse(r01_demo$partner_ed == 0, 'High School/GED', ifelse(r01_demo$partner_ed < 3, 'AA/Technical Degree', ifelse(r01_demo$partner_ed == 3, 'Bachelor Degree', ifelse(r01_demo$partner_ed < 8, '> Bachelor Degree', 'Other/NA')))))

r01_demo$dad_ed <- ifelse(r01_demo$measured_parent == 1, ifelse(r01_demo$parent_ed == 0, 'High School/GED', ifelse(r01_demo$parent_ed < 3, 'AA/Technical Degree', ifelse(r01_demo$parent_ed == 3, 'Bachelor Degree', ifelse(r01_demo$parent_ed < 8, '> Bachelor Degree', 'Other/NA')))), ifelse(r01_demo$partner_ed == 0, 'High School/GED', ifelse(r01_demo$partner_ed < 3, 'AA/Technical Degree', ifelse(r01_demo$partner_ed == 3, 'Bachelor Degree', ifelse(r01_demo$partner_ed < 8, '> Bachelor Degree', 'Other/NA')))))

# race
r01_demo$race <- ifelse(r01_demo$race == 0, 'White/Caucasian', ifelse(r01_demo$race == 2, 'Asian', ifelse(r01_demo$race == 3, 'Black/AA', 'Other')))

# ethnicity
r01_demo$ethnicity <- ifelse(r01_demo$ethnicity == 0, 'Not Hispanic/Lantinx', 'Hispanic/Lantinx')


## 2) Anthro Data ####
## a) Load Data ####
r01_anthro <- as.data.frame(read_spss(("data/raw/anthro_data.sav")))
names(r01_anthro)[1] <- 'sub'

#remove 2 that were removed for ADHD
r01_anthro <- r01_anthro[r01_anthro$sub != 31 & r01_anthro$sub != 34, ]

# pad with zeros
r01_anthro$sub <- sprintf("%03d", r01_anthro$sub)

## 3) Intake Data ####
## a) Load Data ####
r01_intake <- as.data.frame(read_spss("data/raw/intake_data.sav"))

names(r01_intake)[1] <- 'sub'

# pad with zeros
r01_intake$sub <- sprintf("%03d", r01_intake$sub)


r01_intake_labels <- lapply(r01_intake, function(x) attributes(x)$label)

#remove 2 that were removed for ADHD
r01_intake <- r01_intake[r01_intake$sub != 31 & r01_intake$sub != 34, ]

# make numeric
r01_intake[c(606, 652, 698, 744, 607, 653, 699, 745, 115, 477, 116, 478, 166, 528, 167, 529)] <- sapply(r01_intake[c(606, 652, 698, 744, 607, 653, 699, 745, 115, 477, 116, 478, 166, 528, 167, 529)], FUN = as.numeric)

## b) Get Variable Labels and Re-Level ####

# risk status
r01_intake$risk_status_mom <- droplevels(as_factor(r01_intake$risk_status_mom))
r01_intake$risk_status_both <- droplevels(as_factor(r01_intake$risk_status_both))
r01_intake$sex <- as_factor(r01_intake$sex)

# income
r01_intake$income <- ifelse(is.na(r01_intake$income), NA, ifelse(r01_intake$income < 3, '< $51,000', ifelse(r01_intake$income < 5, "$51,000 - $100,000", '>$100,000')))

# parental ed
r01_intake$mom_ed <- ifelse(r01_intake$measured_parent == 0, ifelse(r01_intake$parent_ed == 0, 'High School/GED', ifelse(r01_intake$parent_ed < 3, 'AA/Technical Degree', ifelse(r01_intake$parent_ed == 3, 'Bachelor Degree', ifelse(r01_intake$parent_ed < 8, '> Bachelor Degree', 'Other/NA')))), ifelse(r01_intake$partner_ed == 0, 'High School/GED', ifelse(r01_intake$partner_ed < 3, 'AA/Technical Degree', ifelse(r01_intake$partner_ed == 3, 'Bachelor Degree', ifelse(r01_intake$partner_ed < 8, '> Bachelor Degree', 'Other/NA')))))

r01_intake$dad_ed <- ifelse(r01_intake$measured_parent == 1, ifelse(r01_intake$parent_ed == 0, 'High School/GED', ifelse(r01_intake$parent_ed < 3, 'AA/Technical Degree', ifelse(r01_intake$parent_ed == 3, 'Bachelor Degree', ifelse(r01_intake$parent_ed < 8, '> Bachelor Degree', 'Other/NA')))), ifelse(r01_intake$partner_ed == 0, 'High School/GED', ifelse(r01_intake$partner_ed < 3, 'AA/Technical Degree', ifelse(r01_intake$partner_ed == 3, 'Bachelor Degree', ifelse(r01_intake$partner_ed < 8, '> Bachelor Degree', 'Other/NA')))))

## average VAS across all foods
r01_intake[c("ps1_vas_mac_cheese","ps1_vas_chkn_nug", "ps1_vas_broccoli","ps1_vas_grape", "ps2_vas_mac_cheese","ps2_vas_chkn_nug", "ps2_vas_broccoli","ps2_vas_grape", "ps3_vas_mac_cheese","ps3_vas_chkn_nug", "ps3_vas_broccoli","ps3_vas_grape", "ps4_vas_mac_cheese","ps4_vas_chkn_nug", "ps4_vas_broccoli","ps4_vas_grape")] <- sapply(r01_intake[c("ps1_vas_mac_cheese","ps1_vas_chkn_nug", "ps1_vas_broccoli","ps1_vas_grape", "ps2_vas_mac_cheese","ps2_vas_chkn_nug", "ps2_vas_broccoli","ps2_vas_grape", "ps3_vas_mac_cheese","ps3_vas_chkn_nug", "ps3_vas_broccoli","ps3_vas_grape", "ps4_vas_mac_cheese","ps4_vas_chkn_nug", "ps4_vas_broccoli","ps4_vas_grape")], FUN = as.numeric)

r01_intake[['ps1_avg_vas']] <- rowMeans(r01_intake[c("ps1_vas_mac_cheese","ps1_vas_chkn_nug", "ps1_vas_broccoli","ps1_vas_grape")])
r01_intake[['ps2_avg_vas']] <- rowMeans(r01_intake[c("ps2_vas_mac_cheese","ps2_vas_chkn_nug", "ps2_vas_broccoli","ps2_vas_grape")])
r01_intake[['ps3_avg_vas']] <- rowMeans(r01_intake[c("ps3_vas_mac_cheese","ps3_vas_chkn_nug", "ps3_vas_broccoli","ps3_vas_grape")])
r01_intake[['ps4_avg_vas']] <- rowMeans(r01_intake[c("ps4_vas_mac_cheese","ps4_vas_chkn_nug", "ps4_vas_broccoli","ps4_vas_grape")])

#get portion order
r01_intake[['ps1_visit']] <- ifelse(is.na(r01_intake[['v2_meal_ps']]), NA, ifelse(r01_intake[['v2_meal_ps']] == 0, 1, ifelse(r01_intake[['v3_meal_ps']] == 0, 2, ifelse(r01_intake[['v4_meal_ps']] == 0, 3, 4))))

r01_intake[['ps2_visit']] <- ifelse(is.na(r01_intake[['v2_meal_ps']]), NA, ifelse(r01_intake[['v2_meal_ps']] == 1, 1, ifelse(r01_intake[['v3_meal_ps']] == 1, 2, ifelse(r01_intake[['v4_meal_ps']] == 1, 3, 4))))

r01_intake[['ps3_visit']] <- ifelse(is.na(r01_intake[['v2_meal_ps']]), NA, ifelse(r01_intake[['v2_meal_ps']] == 2, 1, ifelse(r01_intake[['v3_meal_ps']] == 2, 2, ifelse(r01_intake[['v4_meal_ps']] == 2, 3, 4))))

r01_intake[['ps4_visit']] <- ifelse(is.na(r01_intake[['v2_meal_ps']]), NA, ifelse(r01_intake[['v2_meal_ps']] == 3, 1, ifelse(r01_intake[['v3_meal_ps']] == 3, 2, ifelse(r01_intake[['v4_meal_ps']] == 3, 3, 4))))

# make high and low ED intake variables
r01_intake[['ps1_led_g']] <- rowSums(r01_intake[c("ps1_consumed_grapes_g", "ps1_consumed_broccoli_g")])
r01_intake[['ps2_led_g']] <- rowSums(r01_intake[c("ps2_consumed_grapes_g", "ps2_consumed_broccoli_g")])
r01_intake[['ps3_led_g']] <- rowSums(r01_intake[c("ps3_consumed_grapes_g", "ps3_consumed_broccoli_g")])
r01_intake[['ps4_led_g']] <- rowSums(r01_intake[c("ps4_consumed_grapes_g", "ps4_consumed_broccoli_g")])

r01_intake[['ps1_hed_g']] <- rowSums(r01_intake[c("ps1_consumed_chkn_nug_g", "ps1_consumed_mac_cheese_g")])
r01_intake[['ps2_hed_g']] <- rowSums(r01_intake[c("ps2_consumed_chkn_nug_g", "ps2_consumed_mac_cheese_g")])
r01_intake[['ps3_hed_g']] <- rowSums(r01_intake[c("ps3_consumed_chkn_nug_g", "ps3_consumed_mac_cheese_g")])
r01_intake[['ps4_hed_g']] <- rowSums(r01_intake[c("ps4_consumed_chkn_nug_g", "ps4_consumed_mac_cheese_g")])

r01_intake[['ps1_led_kcal']] <- rowSums(r01_intake[c("ps1_consumed_grapes_kcal", "ps1_consumed_broccoli_kcal")])
r01_intake[['ps2_led_kcal']] <- rowSums(r01_intake[c("ps2_consumed_grapes_kcal", "ps2_consumed_broccoli_kcal")])
r01_intake[['ps3_led_kcal']] <- rowSums(r01_intake[c("ps3_consumed_grapes_kcal", "ps3_consumed_broccoli_kcal")])
r01_intake[['ps4_led_kcal']] <- rowSums(r01_intake[c("ps4_consumed_grapes_kcal", "ps4_consumed_broccoli_kcal")])

r01_intake[['ps1_hed_kcal']] <- rowSums(r01_intake[c("ps1_consumed_chkn_nug_kcal", "ps1_consumed_mac_cheese_kcal")])
r01_intake[['ps2_hed_kcal']] <- rowSums(r01_intake[c("ps2_consumed_chkn_nug_kcal", "ps2_consumed_mac_cheese_kcal")])
r01_intake[['ps3_hed_kcal']] <- rowSums(r01_intake[c("ps3_consumed_chkn_nug_kcal", "ps3_consumed_mac_cheese_kcal")])
r01_intake[['ps4_hed_kcal']] <- rowSums(r01_intake[c("ps4_consumed_chkn_nug_kcal", "ps4_consumed_mac_cheese_kcal")])

# average VAS by ED
r01_intake[['ps1_led_vas']] <- rowMeans(r01_intake[c("ps1_vas_broccoli","ps1_vas_grape")])
r01_intake[['ps2_led_vas']] <- rowMeans(r01_intake[c("ps2_vas_broccoli","ps2_vas_grape")])
r01_intake[['ps3_led_vas']] <- rowMeans(r01_intake[c("ps3_vas_broccoli","ps3_vas_grape")])
r01_intake[['ps4_led_vas']] <- rowMeans(r01_intake[c("ps4_vas_broccoli","ps4_vas_grape")])

r01_intake[['ps1_hed_vas']] <- rowMeans(r01_intake[c("ps1_vas_mac_cheese","ps1_vas_chkn_nug")])
r01_intake[['ps2_hed_vas']] <- rowMeans(r01_intake[c("ps2_vas_mac_cheese","ps2_vas_chkn_nug")])
r01_intake[['ps3_hed_vas']] <- rowMeans(r01_intake[c("ps3_vas_mac_cheese","ps3_vas_chkn_nug")])
r01_intake[['ps4_hed_vas']] <- rowMeans(r01_intake[c("ps4_vas_mac_cheese","ps4_vas_chkn_nug")])

## c) Make Data Long ####
intake_long <- melt(r01_intake[c(1, 8:12, 21, 606, 652, 698, 744)], id.vars = names(r01_intake)[c(1, 8:12, 21)])
names(intake_long)[8:9] <- c('PortionSize', 'grams')
intake_long$PortionSize <- ifelse(intake_long$PortionSize == 'ps4_total_g', 'PS-4', ifelse(intake_long$PortionSize == 'ps3_total_g', 'PS-3', ifelse(intake_long$PortionSize == 'ps2_total_g', 'PS-2', 'PS-1')))
intake_long$grams <- as.numeric(intake_long$grams)

intake_kcal_long <- melt(r01_intake[c(1, 607, 653, 699, 745)], id.vars = 'sub')
intake_long$kcal <- intake_kcal_long$value

intake_vas_long <- melt(r01_intake[c(1, 748:751)], id.vars = 'sub')
intake_long$avg_vas <- intake_vas_long$value

intake_preFF_long <- melt(r01_intake[c(1, 563, 609, 655, 701)], id.vars = 'sub')
intake_long$preFF <- as.numeric(intake_preFF_long$value)

intake_postFF_long <- melt(r01_intake[c(1, 564, 610, 656, 702)], id.vars = 'sub')
intake_long$postFF <- as.numeric(intake_preFF_long$value)

intake_date_long <- melt(r01_intake[c(1, 562, 608, 654, 700)], id.vars = 'sub')
intake_long$date <- intake_date_long$value

intake_order_long <- melt(r01_intake[c(1, 752:755)], id.vars = 'sub')
intake_long$meal_order <- intake_order_long$value

intake_meal_dur_long <- melt(r01_intake[c(1, 576, 622, 668, 714)], id.vars = 'sub')
intake_long$meal_dur <- intake_meal_dur_long$value

#low ed
intake_led_grams_long <- melt(r01_intake[c(1, 756:759)], id.vars = 'sub')
intake_long$led_grams <- intake_led_grams_long$value

intake_led_kcal_long <- melt(r01_intake[c(1, 764:767)], id.vars = 'sub')
intake_long$led_kcal <- intake_led_kcal_long$value

vas_led_long <- melt(r01_intake[c(1, 772:775)], id.vars = 'sub')
intake_long$led_vas <- vas_led_long$value

#hed ed
intake_hed_grams_long <- melt(r01_intake[c(1, 760:763)], id.vars = 'sub')
intake_long$hed_grams <- intake_hed_grams_long$value

intake_hed_kcal_long <- melt(r01_intake[c(1, 768:771)], id.vars = 'sub')
intake_long$hed_kcal <- intake_hed_kcal_long$value

vas_hed_long <- melt(r01_intake[c(1, 776:779)], id.vars = 'sub')
intake_long$hed_vas <- vas_hed_long$value


#chicken nuggets
intake_chnug_grams_long <- melt(r01_intake[c(1, 580, 626, 672, 718)], id.vars = 'sub')
intake_long$chnug_grams <- intake_chnug_grams_long$value

intake_chnug_kcal_long <- melt(r01_intake[c(1, 581, 627, 673, 719)], id.vars = 'sub')
intake_long$chnug_kcal <- intake_chnug_kcal_long$value

#mac and cheese
intake_mac_grams_long <- melt(r01_intake[c(1, 585, 631, 677, 723)], id.vars = 'sub')
intake_long$mac_grams <- intake_mac_grams_long$value

intake_mac_kcal_long <- melt(r01_intake[c(1, 586, 632, 678, 724)], id.vars = 'sub')
intake_long$mac_kcal <- intake_mac_kcal_long$value

#grapes
intake_grape_grams_long <- melt(r01_intake[c(1, 590, 636, 682, 728)], id.vars = 'sub')
intake_long$grape_grams <- intake_grape_grams_long$value

intake_grape_kcal_long <- melt(r01_intake[c(1, 591, 637, 683, 729)], id.vars = 'sub')
intake_long$grape_kcal <- intake_grape_kcal_long$value

#brocoli
intake_broc_grams_long <- melt(r01_intake[c(1, 595, 641, 687, 733)], id.vars = 'sub')
intake_long$broc_grams <- intake_broc_grams_long$value

intake_broc_kcal_long <- melt(r01_intake[c(1, 596, 642, 688, 734)], id.vars = 'sub')
intake_long$broc_kcal <- intake_broc_kcal_long$value

#individual vas
intake_mac_vas_long <- melt(r01_intake[c(1, 565, 611, 657, 703)], id.vars = 'sub')
intake_long$mac_vas <- intake_mac_vas_long$value

intake_chnug_vas_long <- melt(r01_intake[c(1, 566, 612, 658, 704)], id.vars = 'sub')
intake_long$chnug_vas <- intake_chnug_vas_long$value

intake_broc_vas_long <- melt(r01_intake[c(1, 567, 613, 659, 705)], id.vars = 'sub')
intake_long$broc_vas <- intake_broc_vas_long$value

intake_grape_vas_long <- melt(r01_intake[c(1, 568, 614, 660, 706)], id.vars = 'sub')
intake_long$grape_vas <- intake_grape_vas_long$value


#continuous approach:
intake_long$ps_prop <- ifelse(intake_long[['PortionSize']] == 'PS-1', 0, ifelse(intake_long[['PortionSize']] == 'PS-2', 0.33, ifelse(intake_long[['PortionSize']] == 'PS-3', 0.66, 0.99)))



## 4) Questionnaire eating behavior data ####
## a) Load Data ####
r01_eatbeh <- as.data.frame(read_spss(("data/raw/qs_eatbeh_bodyimage.sav")))
names(r01_eatbeh)[1] <- 'sub'

#remove 2 that were removed for ADHD
r01_eatbeh <- r01_eatbeh[r01_eatbeh$sub != 31 & r01_eatbeh$sub != 34, ]

# pad with zeros
r01_eatbeh$sub <- sprintf("%03d", r01_eatbeh$sub)

## 4) V6 data ####
## a) Load Data ####
r01_V6 <- as.data.frame(read_spss(("data/raw/visit6_data.sav")))
names(r01_V6)[1] <- 'sub'

#remove 2 that were removed for ADHD
r01_V6 <- r01_V6[r01_V6$sub != 31 & r01_V6$sub != 34, ]

# pad with zeros
r01_V6$sub <- sprintf("%03d", r01_V6$sub)

## 5) motion summary ####
mot_sum <- read.delim("BIDS/derivatives/preprocessed/fmriprep/task-foodcue_byrun-censorsummary_fd-0.9.tsv")
names(mot_sum)[names(mot_sum) == "id"] <- "sub"

#remove 2 that were removed for ADHD
mot_sum <- mot_sum[mot_sum$sub != 31 & mot_sum$sub != 34, ]

# pad with zeros
mot_sum$sub <- sprintf("%03d", mot_sum$sub)

## 6) Framewise displacement ####

# import average framewise displacement
fd <- read.delim("BIDS/derivatives/preprocessed/fmriprep/task-foodcue_avg-fd.tsv")
names(fd)[names(fd) == "id"] <- "sub"

#remove 2 that were removed for ADHD
fd <- fd[fd$sub != 31 & fd$sub != 34, ]

# pad with zeros
fd$sub <- sprintf("%03d", fd$sub)


