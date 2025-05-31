# Create Cachar cancer study dataset for riskdiff package
# Based on anonymized data from cancer screening study in Cachar District, Assam, India

library(dplyr)
library(readr)

# Set seed for reproducible sampling
set.seed(2025)

# Read the original data
cachar_full <- read_csv("data_cleaned.csv")

# Create a stratified random sample of 2500 participants
# Stratify by key variables to ensure representativeness across age groups
cachar_sample <- cachar_full %>%
  # Remove rows with missing key variables
  filter(
    !is.na(diagnosis_bin),
    !is.na(smoker),
    !is.na(areca),
    !is.na(sex),
    !is.na(age),
    !is.na(age_cat)
  ) %>%
  # Create proper age groups based on age_cat
  mutate(
    age_group_clean = case_when(
      age_cat == "<40" ~ "Under 40",
      age_cat == "40-59" ~ "40-60", 
      age_cat == ">=60" ~ "Over 60",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_group_clean))

# Sample from each age group separately to ensure representation
sample_under40 <- cachar_sample %>%
  filter(age_group_clean == "Under 40") %>%
  slice_sample(n = 1500)

sample_40to60 <- cachar_sample %>%
  filter(age_group_clean == "40-60") %>%
  slice_sample(n = 875)

sample_over60 <- cachar_sample %>%
  filter(age_group_clean == "Over 60") %>%
  slice_sample(n = 125)

# Combine samples
cachar_sample <- bind_rows(sample_under40, sample_40to60, sample_over60) %>%
  # Select and clean variables
  select(
    id,
    age,
    sex = sex,
    residence,
    smoking = smoker,
    tobacco_chewing = chewer,
    areca_nut = areca,
    alcohol = drinker,
    abnormal_screen = diagnosis_bin,  # Updated variable name
    head_neck_abnormal = hn_bin,
    age_group = age_group_clean       # Use the cleaned age groups
  ) %>%
  # Clean and standardize variables
  mutate(
    # Convert binary outcomes to 0/1
    abnormal_screen = case_when(
      abnormal_screen == 1 ~ 1L,
      abnormal_screen == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # Create head/neck binary outcome
    head_neck_abnormal = case_when(
      head_neck_abnormal == "abnormal" ~ 1L,
      head_neck_abnormal == "normal" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # Standardize factor variables
    sex = factor(sex, levels = c("male", "female")),
    residence = factor(residence, levels = c("rural", "urban", "urban slum")),
    smoking = factor(case_when(
      smoking == "Yes" ~ "Yes",
      smoking == "No" ~ "No",
      TRUE ~ NA_character_
    ), levels = c("No", "Yes")),
    
    tobacco_chewing = factor(case_when(
      tobacco_chewing == "Yes" ~ "Yes",
      tobacco_chewing == "No" ~ "No",
      TRUE ~ NA_character_
    ), levels = c("No", "Yes")),
    
    areca_nut = factor(case_when(
      areca_nut == "Yes" ~ "Yes",
      areca_nut == "No" ~ "No",
      TRUE ~ NA_character_
    ), levels = c("No", "Yes")),
    
    alcohol = factor(case_when(
      alcohol == "Yes" ~ "Yes",
      alcohol == "No" ~ "No",
      TRUE ~ NA_character_
    ), levels = c("No", "Yes")),
    
    age_group = factor(age_group, levels = c("Under 40", "40-60", "Over 60"))
  ) %>%
  # Arrange by ID for consistency
  arrange(id) %>%
  # Reset ID to 1:n
  mutate(id = row_number())

# Verify the dataset
cat("Cachar Cancer Study Dataset Summary:\n")
cat("====================================\n")
cat("Total observations:", nrow(cachar_sample), "\n")
cat("Abnormal screening rate:", round(mean(cachar_sample$abnormal_screen, na.rm = TRUE) * 100, 1), "%\n")
cat("Head/neck abnormality rate:", round(mean(cachar_sample$head_neck_abnormal, na.rm = TRUE) * 100, 1), "%\n\n")

cat("Exposure prevalences:\n")
cat("Smoking:", round(prop.table(table(cachar_sample$smoking))["Yes"] * 100, 1), "%\n")
cat("Tobacco chewing:", round(prop.table(table(cachar_sample$tobacco_chewing))["Yes"] * 100, 1), "%\n")
cat("Areca nut use:", round(prop.table(table(cachar_sample$areca_nut))["Yes"] * 100, 1), "%\n")
cat("Alcohol use:", round(prop.table(table(cachar_sample$alcohol))["Yes"] * 100, 1), "%\n\n")

cat("Demographics:\n")
print(table(cachar_sample$sex))
cat("\nAge group distribution:\n")
age_table <- table(cachar_sample$age_group)
print(age_table)
cat("Proportions:")
print(round(prop.table(age_table) * 100, 1))
print(table(cachar_sample$residence))

# Cross-tabulations for key associations
cat("\nAbnormal screening by smoking:\n")
print(with(cachar_sample, round(prop.table(table(smoking, abnormal_screen), 1) * 100, 1)))

cat("\nAbnormal screening by areca nut use:\n")
print(with(cachar_sample, round(prop.table(table(areca_nut, abnormal_screen), 1) * 100, 1)))

# Save the dataset
usethis::use_data(cachar_sample, overwrite = TRUE)

cat("\n✓ Dataset 'cachar_sample' created and saved to data/cachar_sample.rda\n")
cat("✓ Use data(cachar_sample) to load it in examples and tests\n")