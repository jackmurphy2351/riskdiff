% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_risk_diff.R
\name{format_risk_diff}
\alias{format_risk_diff}
\title{Format Risk Difference Results for Display}
\usage{
format_risk_diff(
  results,
  digits = 2,
  p_accuracy = 0.001,
  show_ci_method = FALSE,
  show_quality = TRUE
)
}
\arguments{
\item{results}{Results tibble from calc_risk_diff()}

\item{digits}{Number of decimal places for percentages (default: 2)}

\item{p_accuracy}{Accuracy for p-values (default: 0.001)}

\item{show_ci_method}{Logical indicating whether to show CI method in output (default: FALSE)}

\item{show_quality}{Logical indicating whether to add quality indicators (default: TRUE)}
}
\value{
Tibble with additional formatted columns including:
\describe{
\item{rd_formatted}{Risk difference as formatted percentage string}
\item{ci_formatted}{Confidence interval as formatted string}
\item{p_value_formatted}{P-value with appropriate precision}
\item{quality_indicator}{Quality assessment (if show_quality = TRUE)}
\item{ci_method_display}{CI method information (if show_ci_method = TRUE)}
}
}
\description{
Formats numerical values in risk difference results for presentation,
with appropriate percentage formatting and rounding. Enhanced for v0.2.1
to handle boundary information and quality indicators with robust error handling.
}
\examples{
data(cachar_sample)
results <- calc_risk_diff(cachar_sample, "abnormal_screen", "areca_nut")
formatted <- format_risk_diff(results)
print(formatted)

# Show CI methods and quality indicators
formatted_detailed <- format_risk_diff(results, show_ci_method = TRUE, show_quality = TRUE)
print(formatted_detailed)

# Customize formatting
formatted_custom <- format_risk_diff(results, digits = 3, p_accuracy = 0.01, show_quality = FALSE)
print(formatted_custom)

}
