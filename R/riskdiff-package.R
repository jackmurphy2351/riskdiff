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
#' @importFrom dplyr filter
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
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_errorbarh
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom stringr str_to_title

# Suppress R CMD check notes for NSE variables
utils::globalVariables(c(
  # Your existing IPTW variables
  "std_diff",
  "variable",
  "timing",
  "ps",
  "treatment",

  # Variables used in risk difference calculations and formatting
  "rd", "ci_lower", "ci_upper", "ci_lower_percent", "ci_upper_percent",
  "ci_width", "is_stable", "rd_percent", "label", "on_boundary",

  # Variables used in validation and quality assessment functions
  "n_total", "n_outcome", "n_exposed", "n_unexposed", "outcome_rate",
  "too_small", "no_events", "no_variation_exposure", "extreme_outcome_rate",

  # Variables used in forest plots and visualization
  "residence", "n_obs", "model_type", "boundary_type", "ci_method",

  # Variables used in summary and formatting functions
  "exposure_var", "p_value", "Risk_Difference", "CI_95", "P_value"
))

NULL
