# Create synthetic Cachar-inspired dataset for riskdiff package
# Based on authentic epidemiological patterns from Northeast India
# but completely synthetic to avoid ethical concerns

library(dplyr)

# Set seed for reproducibility
set.seed(2025)
n <- 2500

cat("Creating synthetic Cachar-inspired dataset...\n")

# Create synthetic dataset with realistic patterns
cachar_sample <- data.frame(
  id = 1:n
) %>%
  # Create age structure matching observed patterns
  mutate(
    # Sample ages to match target age group distribution
    age_group_target = sample(c("Under 40", "40-60", "Over 60"),
                              n, replace = TRUE,
                              prob = c(0.60, 0.35, 0.05)),

    # Generate ages within groups
    age = case_when(
      age_group_target == "Under 40" ~ sample(18:39, n(), replace = TRUE,
                                              prob = dexp(18:39 - 17, rate = 0.08)),
      age_group_target == "40-60" ~ sample(40:60, n(), replace = TRUE,
                                           prob = dnorm(40:60, 50, 6)),
      age_group_target == "Over 60" ~ sample(61:84, n(), replace = TRUE,
                                             prob = dexp(61:84 - 60, rate = 0.12))
    ),

    # Sex distribution reflecting regional patterns
    sex = factor(sample(c("male", "female"), n, replace = TRUE,
                        prob = c(0.76, 0.24)), levels = c("male", "female")),

    # Residence reflecting Northeast India rural patterns
    residence = factor(sample(c("rural", "urban", "urban slum"), n, replace = TRUE,
                              prob = c(0.87, 0.10, 0.03)),
                       levels = c("rural", "urban", "urban slum"))
  ) %>%
  # Create culturally authentic tobacco use patterns
  mutate(
    # Areca nut use - reflecting authentic regional patterns
    areca_base_prob = case_when(
      age < 25 ~ 0.45,          # Lower in very young adults
      age >= 25 & age < 40 ~ 0.75,  # Peak usage in prime adults
      age >= 40 & age < 60 ~ 0.78,  # Continued high usage
      age >= 60 ~ 0.68          # Slightly lower in elderly
    ),
    # Adjust for sex (slightly higher in males)
    areca_prob = case_when(
      sex == "male" ~ pmin(0.85, areca_base_prob * 1.08),
      sex == "female" ~ areca_base_prob * 0.92
    ),
    areca_nut = factor(rbinom(n, 1, areca_prob),
                       levels = c(0, 1), labels = c("No", "Yes")),

    # Tobacco chewing - strongly correlated with areca nut
    tobacco_base_prob = case_when(
      areca_nut == "Yes" ~ 0.68,  # Often used together
      areca_nut == "No" ~ 0.22    # Lower when areca not used
    ),
    # Age effects on tobacco chewing
    tobacco_age_multiplier = case_when(
      age < 30 ~ 0.85,
      age >= 30 & age < 50 ~ 1.0,
      age >= 50 ~ 1.15
    ),
    tobacco_chewing_prob = pmin(0.90, tobacco_base_prob * tobacco_age_multiplier),
    tobacco_chewing = factor(rbinom(n, 1, tobacco_chewing_prob),
                             levels = c(0, 1), labels = c("No", "Yes")),

    # Smoking - lower prevalence, strong gender patterns
    smoking_base_prob = case_when(
      sex == "male" ~ 0.18,
      sex == "female" ~ 0.025
    ),
    # Age effects on smoking
    smoking_age_multiplier = case_when(
      age < 25 ~ 0.7,   # Lower in very young
      age >= 25 & age < 45 ~ 1.2,  # Peak in middle age
      age >= 45 ~ 0.9   # Decline in older adults
    ),
    smoking_prob = pmin(0.40, smoking_base_prob * smoking_age_multiplier),
    smoking = factor(rbinom(n, 1, smoking_prob),
                     levels = c(0, 1), labels = c("No", "Yes")),

    # Alcohol use - cultural and gender patterns
    alcohol_base_prob = case_when(
      sex == "male" & age >= 25 ~ 0.32,
      sex == "male" & age < 25 ~ 0.12,
      sex == "female" & age >= 30 ~ 0.08,
      sex == "female" & age < 30 ~ 0.03
    ),
    # Urban vs rural differences
    alcohol_residence_multiplier = case_when(
      residence == "urban" ~ 1.3,
      residence == "urban slum" ~ 1.1,
      residence == "rural" ~ 1.0
    ),
    alcohol_prob = pmin(0.50, alcohol_base_prob * alcohol_residence_multiplier),
    alcohol = factor(rbinom(n, 1, alcohol_prob),
                     levels = c(0, 1), labels = c("No", "Yes"))
  ) %>%
  # Create realistic outcome variables with authentic risk patterns
  mutate(
    # Abnormal screening outcome - comprehensive risk model
    abnormal_risk_logit = -3.3 +  # Baseline ~3.5% risk
      0.018 * (age - 40) +        # Age effect (per year)
      0.92 * (areca_nut == "Yes") + # Strong areca effect (OR ~2.5)
      0.38 * (tobacco_chewing == "Yes") + # Tobacco chewing (OR ~1.5)
      0.71 * (smoking == "Yes") +   # Smoking effect (OR ~2.0)
      0.22 * (alcohol == "Yes") +   # Alcohol effect (OR ~1.25)
      0.35 * (sex == "male") +      # Male risk (OR ~1.4)
      -0.15 * (residence == "urban") + # Slightly lower urban risk
      # Interaction: areca + tobacco particularly risky
      0.45 * (areca_nut == "Yes" & tobacco_chewing == "Yes") +
      rnorm(n, 0, 0.25),           # Individual variation

    abnormal_screen_prob = plogis(abnormal_risk_logit),
    abnormal_screen = rbinom(n, 1, abnormal_screen_prob),

    # Head/neck abnormalities - stronger tobacco associations
    hn_risk_logit = -3.8 +  # Baseline ~2.2% risk
      0.015 * (age - 40) +
      1.35 * (areca_nut == "Yes") +     # Very strong association (OR ~3.8)
      0.85 * (tobacco_chewing == "Yes") + # Strong tobacco effect (OR ~2.3)
      1.10 * (smoking == "Yes") +       # Strong smoking effect (OR ~3.0)
      0.28 * (alcohol == "Yes") +       # Moderate alcohol effect
      0.52 * (sex == "male") +          # Male predominance
      # Triple interaction for highest risk
      0.65 * (areca_nut == "Yes" & tobacco_chewing == "Yes" & smoking == "Yes") +
      rnorm(n, 0, 0.22),               # Individual variation

    hn_prob = plogis(hn_risk_logit),
    head_neck_abnormal = rbinom(n, 1, hn_prob),

    # Create final age groups
    age_group = factor(case_when(
      age < 40 ~ "Under 40",
      age >= 40 & age <= 60 ~ "40-60",
      age > 60 ~ "Over 60"
    ), levels = c("Under 40", "40-60", "Over 60"))
  ) %>%
  # Clean up intermediate variables and reorder
  select(id, age, sex, residence, smoking, tobacco_chewing, areca_nut, alcohol,
         abnormal_screen, head_neck_abnormal, age_group) %>%
  # Add combination variable for dual tobacco+areca use
  mutate(
    tobacco_areca_both = factor(
      case_when(
        tobacco_chewing == "Yes" & areca_nut == "Yes" ~ "Yes",
        TRUE ~ "No"
      ),
      levels = c("No", "Yes")
    )
  ) %>%
  # Final randomization of order
  sample_frac(1) %>%
  arrange(id) %>%
  mutate(id = row_number())  # Reset IDs

# Comprehensive verification
cat("Synthetic Cachar-Inspired Dataset Summary:\n")
cat("==========================================\n")
cat("Total observations:", nrow(cachar_sample), "\n")
cat("Abnormal screening rate:", round(mean(cachar_sample$abnormal_screen) * 100, 1), "%\n")
cat("Head/neck abnormality rate:", round(mean(cachar_sample$head_neck_abnormal) * 100, 1), "%\n\n")

cat("Exposure prevalences:\n")
cat("Areca nut use:", round(prop.table(table(cachar_sample$areca_nut))["Yes"] * 100, 1), "%\n")
cat("Tobacco chewing:", round(prop.table(table(cachar_sample$tobacco_chewing))["Yes"] * 100, 1), "%\n")
cat("Smoking:", round(prop.table(table(cachar_sample$smoking))["Yes"] * 100, 1), "%\n")
cat("Alcohol use:", round(prop.table(table(cachar_sample$alcohol))["Yes"] * 100, 1), "%\n")
cat("Both tobacco chewing + areca nut:", round(prop.table(table(cachar_sample$tobacco_areca_both))["Yes"] * 100, 1), "%\n\n")

cat("Demographics:\n")
print(table(cachar_sample$sex))
cat("\nAge group distribution:\n")
age_table <- table(cachar_sample$age_group)
print(age_table)
cat("Proportions:")
print(round(prop.table(age_table) * 100, 1))
cat("\nResidence:\n")
print(table(cachar_sample$residence))

# Verify realistic associations
cat("\nKey associations (synthetic data verification):\n")
cat("Abnormal screening by areca nut use:\n")
areca_crosstab <- with(cachar_sample, table(areca_nut, abnormal_screen))
print(round(prop.table(areca_crosstab, 1) * 100, 1))

cat("\nHead/neck abnormalities by tobacco use combinations:\n")
tobacco_combo <- with(cachar_sample,
                      table(paste(areca_nut, tobacco_chewing, smoking, sep = "+"), head_neck_abnormal))
print(round(prop.table(tobacco_combo, 1) * 100, 1)[1:4,])  # Show top combinations

cat(paste0(.safe_check()), "Synthetic dataset created successfully!")
cat(paste0(.safe_check()), "Preserves authentic epidemiological patterns")
cat(paste0(.safe_check()), "No privacy or ethical concerns")
cat(paste0(.safe_check()), "Ready for use in riskdiff package examples")

# Save the dataset (uncomment when ready to save)
# save(cachar_sample, file = "data/cachar_sample.rda", compress = "bzip2")
# cat("\n✓ Dataset saved to data/cachar_sample.rda")
