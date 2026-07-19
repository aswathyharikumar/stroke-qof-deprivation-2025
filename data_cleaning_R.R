# ================================================
# Stroke QOF Deprivation Study — Data Cleaning
# Author: Aswathy Harikumar
# Date: 2026
# ================================================

# Load packages
library(tidyverse)
library(janitor)
library(readxl)
library(openxlsx)
library(PostcodesioR)

# ------------------------------------------------
# SECTION 1 — Load raw datasets
# ------------------------------------------------

# QOF 2024-25 STIA sheet
qof <- read.xlsx("C:/Users/Aswathy Harikumar/OneDrive/Desktop/stroke_study/data/QOF 24-25.xlsx",
                 sheet = "STIA",
                 startRow = 12)

# IMD 2025
imd <- read.xlsx("C:/Users/Aswathy Harikumar/OneDrive/Desktop/stroke_study/data/Index_of_Multiple_Deprivation.xlsx",
                 sheet = "IMD25")

# Practice list sizes
list_sizes <- read_csv("C:/Users/Aswathy Harikumar/OneDrive/Desktop/stroke_study/data/Practice_list_sizes.csv")

# Practice mapping file
mapping <- read_csv("C:/Users/Aswathy Harikumar/OneDrive/Desktop/stroke_study/data/gp-reg-pat-prac-map.csv")

# ------------------------------------------------
# SECTION 2 — Clean QOF data
# ------------------------------------------------

qof_clean <- qof %>%
  select(
    practice_code = Practice.code,
    practice_name = Practice.name,
    overall_achievement = 18,
    stia007_numerator = 32,
    stia007_denominator = 33,
    stia014_numerator = 40,
    stia014_denominator = 41,
    stia015_numerator = 48,
    stia015_denominator = 49
  ) %>%
  mutate(
    overall_achievement = as.numeric(overall_achievement) / 100,
    prop_stia007 = as.numeric(stia007_numerator) / as.numeric(stia007_denominator),
    prop_stia014 = as.numeric(stia014_numerator) / as.numeric(stia014_denominator),
    prop_stia015 = as.numeric(stia015_numerator) / as.numeric(stia015_denominator)
  )

# ------------------------------------------------
# SECTION 3 — Link practices to IMD via postcode
# ------------------------------------------------

# Fix URL encoded spaces in postcodes
mapping <- mapping %>%
  mutate(postcode_nospace = gsub(" ", "", PRACTICE_POSTCODE))

# Get unique postcodes
postcodes_nospace <- unique(mapping$postcode_nospace)

# Bulk postcode lookup to get LSOA codes
batches <- split(postcodes_nospace, ceiling(seq_along(postcodes_nospace)/100))

postcode_lookup <- map_dfr(batches, function(batch) {
  result <- bulk_postcode_lookup(list(postcodes = batch))
  map_dfr(result, function(x) {
    if (!is.null(x$result)) {
      data.frame(
        postcode = x$query,
        lsoa_code = x$result$codes$lsoa,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        postcode = x$query,
        lsoa_code = NA,
        stringsAsFactors = FALSE
      )
    }
  })
})

# Link mapping to LSOA codes
mapping_with_lsoa <- mapping %>%
  left_join(postcode_lookup, by = c("postcode_nospace" = "postcode"))

# Link to IMD scores
mapping_with_imd <- mapping_with_lsoa %>%
  left_join(imd, by = c("lsoa_code" = "LSOA.code.(2021)"))

# Keep only practice code and IMD rank
practice_imd <- mapping_with_imd %>%
  select(
    practice_code = PRACTICE_CODE,
    imd_rank = `Index.of.Multiple.Deprivation.(IMD).Rank.(where.1.is.most.deprived)`
  )

# ------------------------------------------------
# SECTION 4 — Process list sizes
# ------------------------------------------------

# Total patients
list_totals <- list_sizes %>%
  filter(ORG_TYPE == "GP", SEX == "ALL", AGE_GROUP_5 == "ALL") %>%
  group_by(ORG_CODE) %>%
  summarise(total_patients = sum(NUMBER_OF_PATIENTS, na.rm = TRUE))

# Patients aged 65+
list_age <- list_sizes %>%
  filter(ORG_TYPE == "GP",
         SEX %in% c("FEMALE", "MALE"),
         AGE_GROUP_5 %in% c("65_69", "70_74", "75_79",
                            "80_84", "85_89", "90_94", "95+")) %>%
  group_by(ORG_CODE) %>%
  summarise(patients_65_plus = sum(NUMBER_OF_PATIENTS, na.rm = TRUE))

# Female patients
list_female <- list_sizes %>%
  filter(ORG_TYPE == "GP", SEX == "FEMALE", AGE_GROUP_5 == "ALL") %>%
  group_by(ORG_CODE) %>%
  summarise(patients_female = sum(NUMBER_OF_PATIENTS, na.rm = TRUE))

# Join list size components
list_processed <- list_totals %>%
  left_join(list_age, by = "ORG_CODE") %>%
  left_join(list_female, by = "ORG_CODE") %>%
  mutate(
    prop_65_plus = patients_65_plus / total_patients,
    prop_female = patients_female / total_patients
  )

# ------------------------------------------------
# SECTION 5 — Build final dataset
# ------------------------------------------------

final_dataset <- qof_clean %>%
  left_join(practice_imd, by = "practice_code") %>%
  left_join(list_processed, by = c("practice_code" = "ORG_CODE")) %>%
  mutate(
    # Stroke prevalence
    stroke_prevalence = stia007_denominator / total_patients,
    
    # IMD quintile — Q1 least deprived, Q5 most deprived
    imd_quintile = 6 - ntile(imd_rank, 5),
    
    # Confounder quintiles
    listsize_quintile = ntile(total_patients, 5),
    prevalence_quintile = ntile(stroke_prevalence, 5),
    age65_quintile = ntile(prop_65_plus, 5),
    female_quintile = ntile(prop_female, 5)
  )

# ------------------------------------------------
# SECTION 6 — Handle missing data
# ------------------------------------------------

# Report missing data
cat("Original practices:", nrow(final_dataset), "\n")
cat("Practices with missing data:", sum(!complete.cases(final_dataset)), "\n")

# Remove missing data
final_clean <- final_dataset %>%
  filter(complete.cases(.))

cat("Final dataset:", nrow(final_clean), "\n")
cat("Exclusion rate:", round((nrow(final_dataset) - nrow(final_clean)) / nrow(final_dataset) * 100, 1), "%\n")

# ------------------------------------------------
# SECTION 7 — Save cleaned dataset
# ------------------------------------------------

write.csv(final_clean,
          "C:/Users/Aswathy Harikumar/OneDrive/Desktop/stroke_study/data/cleaned_stroke_dataset.csv",
          row.names = FALSE)

cat("Data cleaning complete. Final dataset saved.\n")
