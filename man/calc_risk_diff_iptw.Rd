% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iptw_causal.R
\name{calc_risk_diff_iptw}
\alias{calc_risk_diff_iptw}
\title{Calculate Standardized Risk Differences Using IPTW}
\usage{
calc_risk_diff_iptw(
  data,
  outcome,
  treatment,
  covariates,
  iptw_weights = NULL,
  weight_type = "ATE",
  ps_method = "logistic",
  stabilize = TRUE,
  trim_weights = TRUE,
  alpha = 0.05,
  bootstrap_ci = FALSE,
  boot_n = 1000,
  verbose = FALSE
)
}
\arguments{
\item{data}{A data frame containing outcome, treatment, and covariate data}

\item{outcome}{Character string naming the binary outcome variable}

\item{treatment}{Character string naming the binary treatment variable}

\item{covariates}{Character vector of covariate names for propensity score model}

\item{iptw_weights}{Optional vector of pre-calculated IPTW weights}

\item{weight_type}{Type of weights if calculating: "ATE", "ATT", or "ATC" (default: "ATE")}

\item{ps_method}{Method for propensity score estimation (default: "logistic")}

\item{stabilize}{Whether to use stabilized weights (default: TRUE)}

\item{trim_weights}{Whether to trim extreme weights (default: TRUE)}

\item{alpha}{Significance level for confidence intervals (default: 0.05)}

\item{bootstrap_ci}{Whether to use bootstrap confidence intervals (default: FALSE)}

\item{boot_n}{Number of bootstrap replicates if bootstrap_ci=TRUE (default: 1000)}

\item{verbose}{Whether to print diagnostic information (default: FALSE)}
}
\value{
A tibble of class "riskdiff_iptw_result" containing:
\describe{
\item{treatment_var}{Character. Name of treatment variable}
\item{rd_iptw}{Numeric. IPTW-standardized risk difference}
\item{ci_lower}{Numeric. Lower confidence interval bound}
\item{ci_upper}{Numeric. Upper confidence interval bound}
\item{p_value}{Numeric. P-value for test of null hypothesis}
\item{weight_type}{Character. Type of weights used}
\item{effective_n}{Numeric. Effective sample size}
\item{risk_treated}{Numeric. Risk in treated group}
\item{risk_control}{Numeric. Risk in control group}
}
}
\description{
Calculates standardized risk differences using inverse probability of treatment
weighting. This approach estimates causal effects under the assumption of no
unmeasured confounding by creating a pseudo-population where treatment assignment
is independent of measured confounders.
}
\details{
\subsection{Causal Interpretation}{

IPTW estimates causal effects by weighting observations to create balance
on measured confounders. The estimand depends on the weight type:
\itemize{
\item \strong{ATE}: Average treatment effect in the population
\item \strong{ATT}: Average treatment effect among those who received treatment
\item \strong{ATC}: Average treatment effect among those who did not receive treatment
}
}

\subsection{Standard Errors}{

By default, uses robust (sandwich) standard errors that account for propensity
score estimation uncertainty. Bootstrap confidence intervals are available
as an alternative that may perform better with small samples.
}

\subsection{Assumptions}{
\enumerate{
\item \strong{No unmeasured confounding}: All confounders are measured and included
\item \strong{Positivity}: All subjects have non-zero probability of receiving either treatment
\item \strong{Correct model specification}: Propensity score model is correctly specified
}
}
}
\examples{
data(cachar_sample)

# Standard ATE estimation
rd_iptw <- calc_risk_diff_iptw(
  data = cachar_sample,
  outcome = "abnormal_screen",
  treatment = "areca_nut",
  covariates = c("age", "sex", "residence", "smoking")
)
print(rd_iptw)

# ATT estimation with bootstrap CI
rd_att <- calc_risk_diff_iptw(
  data = cachar_sample,
  outcome = "head_neck_abnormal",
  treatment = "tobacco_chewing",
  covariates = c("age", "sex", "residence", "areca_nut"),
  weight_type = "ATT",
  bootstrap_ci = TRUE,
  boot_n = 500
)
print(rd_att)

}
