# This script was written by Bari Fuchs in to remove potentially identifiable information
# from datasets prior to publically sharing data
#
#     Copyright (C) 2024 Bari Fuchs
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

#### Load packages ####
library(haven)


#### Remove PII ####


## Demographics data

# load data
r01_demo <- as.data.frame(read_spss(("data/raw/demographics_data.sav")))

# overwrite identifiable data as NA
r01_demo$v1_date <- NA
r01_demo$dob <- NA
r01_demo$race <- NA
r01_demo$ethnicity <- NA

## Anthro data

# load data
r01_anthro <- as.data.frame(read_spss(("data/raw/anthro_data.sav")))

# overwrite identifiable data as NA
r01_anthro$v1_date <- NA
r01_anthro$dob <- NA
r01_anthro$race <- NA
r01_anthro$ethnicity <- NA

## Intake data

# load data
r01_intake <- as.data.frame(read_spss("data/raw/intake_data.sav"))

# overwrite identifiable data as NA
r01_intake$v1_date <- NA
r01_intake$dob <- NA
r01_intake$race <- NA
r01_intake$ethnicity <- NA

#### Export datasets ####
write.table(r01_demo, "data/raw_deidentified/demographics_data.csv", sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "ASCII")
write.table(r01_anthro, "data/raw_deidentified/anthro_data.csv", sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "ASCII")
write.table(r01_intake, "data/raw_deidentified/intake_data.csv", sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "ASCII")

# copy over files that do not need to be de-identified
file.copy("data/raw/visit6_data.sav", "data/raw_deidentified/visit6_data.sav")
file.copy("data/raw/dict-visit6_data.csv", "data/raw_deidentified/dict-visit6_data.csv")
file.copy("data/raw/FoodAndBrainR01DataP-Scansroar.csv", "data/raw_deidentified/FoodAndBrainR01DataP-Scansroar.csv")
file.copy("data/raw/dict-FoodAndBrainR01DataP-Scansroar.csv", "data/raw_deidentified/dict-FoodAndBrainR01DataP-Scansroar.csv")
file.copy("data/raw/dict-intake_data.csv", "data/raw_deidentified/dict-intake_data.csv")
file.copy("data/raw/dict-demographics_data.csv", "data/raw_deidentified/dict-demographics_data.csv")
file.copy("data/raw/dict-anthro_data.csv", "data/raw_deidentified/dict-anthro_data.csv")
