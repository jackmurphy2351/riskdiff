# Create simulated birthweight dataset for riskdiff package

library(dplyr)

# Set seed for reproducibility
set.seed(809)

# Sample size
n <- 2500

# Create base demographic variables
birthweight <- data.frame(
  id = 1:n,

  # Maternal smoking (20% prevalence)
  smoking = factor(
    sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.8, 0.2)),
    levels = c("No", "Yes")
  ),

  # Maternal age (normal distribution, mean 28, SD 6)
  maternal_age = pmax(15, pmin(45, round(rnorm(n, 28, 6)))),

  # Race/ethnicity (realistic US distribution)
  race = factor(
    sample(c("White", "Black", "Hispanic", "Other"),
           n, replace = TRUE, prob = c(0.60, 0.15, 0.20, 0.05)),
    levels = c("White", "Black", "Hispanic", "Other")
  ),

  # Education level (correlated with age and race)
  education = factor(
    sample(c("Less than HS", "HS", "Some college", "College+"),
           n, replace = TRUE, prob = c(0.10, 0.30, 0.35, 0.25)),
    levels = c("Less than HS", "HS", "Some college", "College+")
  ),

  # Prenatal care adequacy (85% adequate)
  prenatal_care = factor(
    sample(c("Adequate", "Inadequate"), n, replace = TRUE, prob = c(0.85, 0.15)),
    levels = c("Adequate", "Inadequate")
  ),

  # Parity (number of previous births, Poisson distribution capped at 3)
  parity = pmin(3, rpois(n, lambda = 1.2))
)

# Create realistic correlations between variables
# Adjust education based on age (older mothers more likely to have higher education)
age_edu_adjust <- ifelse(birthweight$maternal_age > 30,
                         sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
                         sample(c(0, 1), n, replace = TRUE, prob = c(0.8, 0.2)))

# Upgrade education for some older mothers
upgrade_idx <- which(age_edu_adjust == 1 & birthweight$education %in% c("Less than HS", "HS"))
if (length(upgrade_idx) > 0) {
  new_edu <- ifelse(birthweight$education[upgrade_idx] == "Less than HS", "HS", "Some college")
  birthweight$education[upgrade_idx] <- new_edu
}

# Re-factor education to maintain proper levels
birthweight$education <- factor(birthweight$education,
                                levels = c("Less than HS", "HS", "Some college", "College+"))

# Adjust prenatal care based on education (higher education -> better care)
poor_care_risk <- case_when(
  birthweight$education == "Less than HS" ~ 0.30,
  birthweight$education == "HS" ~ 0.18,
  birthweight$education == "Some college" ~ 0.12,
  birthweight$education == "College+" ~ 0.08,
  TRUE ~ 0.15
)

inadequate_care <- rbinom(n, 1, poor_care_risk)
birthweight$prenatal_care[inadequate_care == 1] <- "Inadequate"
birthweight$prenatal_care <- factor(birthweight$prenatal_care,
                                    levels = c("Adequate", "Inadequate"))

# Create low birth weight outcome with realistic associations
# Log-odds model for realistic effect sizes
logit_prob <- with(birthweight, {
  -2.8 +                                    # Baseline (about 6% risk)
    0.85 * (smoking == "Yes") +             # Smoking: OR ≈ 2.3
    0.02 * (maternal_age - 28) +            # Age: slight increase with age
    0.55 * (race == "Black") +              # Black race: OR ≈ 1.7
    0.35 * (race == "Hispanic") +           # Hispanic: OR ≈ 1.4
    0.15 * (race == "Other") +              # Other race: OR ≈ 1.2
    -0.30 * (education == "HS") +           # HS vs <HS: OR ≈ 0.7
    -0.50 * (education == "Some college") + # Some college: OR ≈ 0.6
    -0.75 * (education == "College+") +     # College+: OR ≈ 0.5
    0.45 * (prenatal_care == "Inadequate") + # Poor prenatal care: OR ≈ 1.6
    0.15 * parity                           # Parity: OR ≈ 1.2 per birth
})

# Convert to probabilities and generate outcome
prob_low_bw <- plogis(logit_prob)
birthweight$low_birthweight <- rbinom(n, 1, prob_low_bw)

# Add some realistic interactions
# Smoking effect stronger in younger mothers
young_smoker_idx <- which(birthweight$smoking == "Yes" & birthweight$maternal_age < 25)
if (length(young_smoker_idx) > 0) {
  # Additional risk for young smokers
  extra_risk <- rbinom(length(young_smoker_idx), 1, 0.15)
  birthweight$low_birthweight[young_smoker_idx[extra_risk == 1]] <- 1
}

# Education effect stronger in certain racial groups
high_edu_minority <- which(birthweight$race %in% c("Black", "Hispanic") &
                             birthweight$education %in% c("Some college", "College+"))
if (length(high_edu_minority) > 0) {
  # Additional protection for educated minorities
  extra_protection <- rbinom(length(high_edu_minority), 1, 0.20)
  birthweight$low_birthweight[high_edu_minority[extra_protection == 1]] <- 0
}

# Final data cleaning and validation
birthweight <- birthweight %>%
  # Ensure all factors have proper levels
  mutate(
    smoking = factor(smoking, levels = c("No", "Yes")),
    race = factor(race, levels = c("White", "Black", "Hispanic", "Other")),
    education = factor(education, levels = c("Less than HS", "HS", "Some college", "College+")),
    prenatal_care = factor(prenatal_care, levels = c("Adequate", "Inadequate"))
  ) %>%
  # Arrange by ID for consistency
  arrange(id)

# Verify the dataset looks reasonable
cat("Dataset Summary:\n")
cat("================\n")
cat("Total observations:", nrow(birthweight), "\n")
cat("Overall low birth weight rate:", round(mean(birthweight$low_birthweight) * 100, 1), "%\n\n")

cat("Low birth weight by smoking:\n")
print(with(birthweight, round(prop.table(table(smoking, low_birthweight), 1) * 100, 1)))

cat("\nLow birth weight by race:\n")
print(with(birthweight, round(prop.table(table(race, low_birthweight), 1) * 100, 1)))

cat("\nLow birth weight by education:\n")
print(with(birthweight, round(prop.table(table(education, low_birthweight), 1) * 100, 1)))

# Save the dataset
usethis::use_data(birthweight, overwrite = TRUE)

cat("\n✓ Dataset 'birthweight' created and saved to data/birthweight.rda\n")
cat("✓ Use data(birthweight) to load it in examples and tests\n")
