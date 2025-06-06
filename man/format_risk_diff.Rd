% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_risk_diff.R
\name{format_risk_diff}
\alias{format_risk_diff}
\title{Format Risk Difference Results for Display}
\usage{
format_risk_diff(results, digits = 2, p_accuracy = 0.001)
}
\arguments{
\item{results}{Results tibble from calc_risk_diff()}

\item{digits}{Number of decimal places for percentages (default: 2)}

\item{p_accuracy}{Accuracy for p-values (default: 0.001)}
}
\value{
Tibble with additional formatted columns
}
\description{
Formats numerical values in risk difference results for presentation,
with appropriate percentage formatting and rounding.
}
\examples{
data(cachar_sample)
results <- calc_risk_diff(cachar_sample, "abnormal_screen", "smoking")
formatted <- format_risk_diff(results)
print(formatted)

}
