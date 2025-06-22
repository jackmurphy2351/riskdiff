# ==============================================================================
# R/data.R - Dataset documentation for riskdiff package
# ==============================================================================

#' Synthetic Cancer Risk Factor Study Data
#'
#' A synthetic dataset inspired by cancer screening and risk factor patterns
#' observed during an opportunistic screening program conducted at the
#' Cachar Cancer Hospital and Research Centre in Northeast India, specifically
#' designed to reflect authentic epidemiological relationships without using
#' real patient data.
#'
#' @format A data frame with 2,500 rows and 12 variables:
#' \describe{
#'   \item{id}{Participant identifier (1 to 2500)}
#'   \item{age}{Age in years (continuous, range 18-84)}
#'   \item{sex}{Biological sex: "male" or "female"}
#'   \item{residence}{Residence type: "rural", "urban", or "urban slum"}
#'   \item{smoking}{Current smoking status: "No" or "Yes"}
#'   \item{tobacco_chewing}{Current tobacco chewing: "No" or "Yes"}
#'   \item{areca_nut}{Current areca nut use: "No" or "Yes"}
#'   \item{alcohol}{Current alcohol use: "No" or "Yes"}
#'   \item{abnormal_screen}{Binary outcome: 1 = abnormal screening (precancerous lesions or cancer), 0 = normal}
#'   \item{head_neck_abnormal}{Binary outcome: 1 = head/neck abnormality detected, 0 = normal}
#'   \item{age_group}{Age categories: "Under 40", "40-60", "Over 60"}
#'   \item{tobacco_areca_both}{Combined exposure: "Yes" if both tobacco_chewing and areca_nut are "Yes", "No" otherwise}
#' }
#'
#' @details
#' This synthetic dataset was designed to reflect authentic epidemiological
#' patterns observed in Northeast India, particularly the distinctive tobacco
#' and areca nut use patterns of the region. All data points are mathematically
#' generated rather than collected from real individuals.
#'
#' **Key epidemiological features modeled:**
#' * **Areca nut use**: Very high prevalence (~69%) reflecting regional cultural practices
#' * **Tobacco chewing**: Moderate to high prevalence (~53%), often used with areca nut
#' * **Smoking**: Lower prevalence (~13%) with strong male predominance
#' * **Cancer outcomes**: Realistic prevalence (~3.5%) for population-based screening,
#'   including both precancerous lesions and invasive cancers
#' * **Geographic patterns**: Predominantly rural population (~87%)
#'
#' **Synthetic Data Advantages:**
#' The synthetic approach preserves authentic statistical relationships while:
#' * Avoiding any privacy or ethical concerns
#' * Ensuring reproducible examples and tests
#' * Providing controlled demonstration scenarios
#' * Maintaining cultural authenticity for educational purposes
#'
#' **Risk Factor Relationships:**
#' The data models realistic dose-response relationships between multiple
#' tobacco exposures and cancer outcomes, with particularly strong associations
#' for areca nut use and head/neck abnormalities, reflecting authentic
#' epidemiological patterns from this region.
#'
#' @source
#' Synthetic dataset created for the riskdiff package. Inspired by cancer
#' screening patterns observed in Northeast India but contains no real patient
#' data. Statistical relationships designed to reflect authentic epidemiological
#' patterns from this region for educational and methodological purposes.
#'
#' @references
#' Epidemiological patterns modeled after studies of tobacco use and cancer
#' risk in Northeast India. For research involving actual populations from
#' this region, consult published literature on areca nut and tobacco-related
#' cancer risks in South Asian populations.
#'
#' Warnakulasuriya S, Trivedy C, Peters TJ (2002). "Areca nut use: an independent
#' risk factor for oral cancer." BMJ, 324(7341), 799-800.
#'
#' Gupta PC, Ray CS (2004). "Epidemiology of betel quid use." Annals of the
#' Academy of Medicine, Singapore, 33(4 Suppl), 31-36.
#'
#' @note
#' This synthetic dataset is designed for educational and software demonstration
#' purposes. While the statistical relationships reflect authentic epidemiological
#' patterns, the data should not be used for research conclusions about real
#' populations. The cultural patterns represented (high areca nut use, specific
#' tobacco consumption practices) are authentic to Northeast India.
#'
#' @examples
#' data(cachar_sample)
#' head(cachar_sample)
#'
#' # Basic descriptive statistics
#' table(cachar_sample$areca_nut, cachar_sample$abnormal_screen)
#'
#' # Regional tobacco use patterns
#' with(cachar_sample, table(areca_nut, tobacco_chewing))
#'
#' # Simple risk difference for areca nut and abnormal screening
#' rd_areca <- calc_risk_diff(
#'   data = cachar_sample,
#'   outcome = "abnormal_screen",
#'   exposure = "areca_nut"
#' )
#' print(rd_areca)
#'
#' # Age-adjusted analysis
#' rd_adjusted <- calc_risk_diff(
#'   data = cachar_sample,
#'   outcome = "abnormal_screen",
#'   exposure = "areca_nut",
#'   adjust_vars = "age"
#' )
#' print(rd_adjusted)
#'
#' # Stratified by sex
#' rd_stratified <- calc_risk_diff(
#'   data = cachar_sample,
#'   outcome = "head_neck_abnormal",
#'   exposure = "smoking",
#'   strata = "sex"
#' )
#' print(rd_stratified)
#'
#' # Multiple tobacco exposures comparison
#' rd_smoking <- calc_risk_diff(cachar_sample, "abnormal_screen", "smoking")
#' rd_chewing <- calc_risk_diff(cachar_sample, "abnormal_screen", "tobacco_chewing")
#' rd_areca <- calc_risk_diff(cachar_sample, "abnormal_screen", "areca_nut")
#'
#' # Compare risk differences
#' cat("Risk differences for abnormal screening:\n")
#' cat("Smoking:", sprintf("%.1f%%", rd_smoking$rd * 100), "\n")
#' cat("Tobacco chewing:", sprintf("%.1f%%", rd_chewing$rd * 100), "\n")
#' cat("Areca nut:", sprintf("%.1f%%", rd_areca$rd * 100), "\n")
#'
#' # Create summary table
#' cat(create_simple_table(rd_areca, "Abnormal Screening Risk by Areca Nut Use"))
#'
"cachar_sample"

# Note: The previous "birthweight" dataset has been replaced with "cachar_sample"
# to provide more authentic and culturally relevant examples for risk difference
# analysis while maintaining complete ethical compliance through synthetic data.
