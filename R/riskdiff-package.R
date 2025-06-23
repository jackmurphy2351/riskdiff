#' @keywords internal
"_PACKAGE"

# Add these imports:
#' @importFrom dplyr %>%
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr group_by
#' @importFrom dplyr group_keys
#' @importFrom dplyr group_split
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr arrange
#' @importFrom dplyr n
#' @importFrom purrr map2_dfr
#' @importFrom purrr transpose
#' @importFrom rlang .data
#' @importFrom scales percent
#' @importFrom scales pvalue
#' @importFrom tibble tibble
#' @importFrom stats setNames
#' @importFrom stats coef confint fitted model.matrix
#' @importFrom stats glm binomial plogis qnorm rbinom
#' @importFrom stats reformulate vcov predict
#' @importFrom stats plogis
#' @importFrom stats predict
#' @importFrom stats fitted
#' @importFrom stats weighted.mean
#' @importFrom stats var
#' @importFrom stats complete.cases
#' @importFrom stats quantile
#' @importFrom graphics abline axis hist legend lines par points
#' @importFrom grDevices rgb
#' @importFrom utils globalVariables

# Suppress R CMD check notes for NSE variables
utils::globalVariables(c(
  "std_diff",
  "variable",
  "timing",
  "ps",
  "treatment"
))
NULL
