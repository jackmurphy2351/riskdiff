#' Create Formatted Table of Risk Difference Results
#'
#' @description
#' Creates a publication-ready table of risk difference results with
#' appropriate grouping and formatting. Requires the kableExtra package
#' for full functionality.
#'
#' @param results Results tibble from calc_risk_diff()
#' @param caption Table caption (default: "Risk Differences")
#' @param include_model_type Whether to include model type column (default: FALSE)
#' @param ... Additional arguments passed to kableExtra::kable()
#'
#' @return
#' If kableExtra is available, returns a kable table object suitable for
#' rendering in R Markdown or HTML. The table includes formatted risk differences,
#' confidence intervals, and p-values with appropriate styling and footnotes.
#' If kableExtra is not available, returns a formatted tibble with the same
#' information in a basic data frame structure.
#'
#' @examples
#' data(birthweight)
#' results <- calc_risk_diff(birthweight, "low_birthweight", "smoking")
#'
#' # Basic table (works without kableExtra)
#' basic_table <- create_rd_table(results, caption = "Risk of Low Birth Weight")
#' print(basic_table)
#'
#' # Enhanced table (requires kableExtra)
#' if (requireNamespace("kableExtra", quietly = TRUE)) {
#'   enhanced_table <- create_rd_table(
#'     results,
#'     caption = "Risk of Low Birth Weight by Smoking Status",
#'     include_model_type = TRUE
#'   )
#'   print(enhanced_table)
#' }
#'
#' @export
create_rd_table <- function(results,
                            caption = "Risk Differences",
                            include_model_type = FALSE,
                            ...) {

  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    message("kableExtra not available. Returning formatted tibble instead of table.")
    return(format_risk_diff(results))
  }

  # Auto-detect stratification variables
  strata_vars <- .detect_strata_vars(results)

  # Format results
  formatted <- format_risk_diff(results)

  # Prepare display data
  display_cols <- c("exposure_var", "rd_formatted", "ci_formatted", "p_value_formatted")
  if (include_model_type) {
    display_cols <- c(display_cols, "model_type")
  }

  display_data <- formatted %>%
    dplyr::select(dplyr::all_of(c(strata_vars, display_cols)))

  # Rename columns for display
  names(display_data) <- .clean_column_names(names(display_data))

  # Create basic table
  table_data <- if (length(strata_vars) > 0) {
    display_data %>% dplyr::select(-dplyr::all_of(stringr::str_to_title(strata_vars)))
  } else {
    display_data
  }

  table_obj <- kableExtra::kable(
    table_data,
    caption = caption,
    format = "html",
    escape = FALSE,
    ...
  ) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE
    ) %>%
    kableExtra::footnote(
      general = "Risk differences represent absolute differences in risk between exposed and unexposed groups.",
      number = c("Confidence intervals calculated using robust standard errors.")
    )

  # Add grouping for stratified results
  if (length(strata_vars) > 0) {
    table_obj <- .add_table_grouping(table_obj, display_data, strata_vars)
  }

  return(table_obj)
}

#' Create a Simple Summary Table
#'
#' @description
#' Creates a simple text-based summary table that doesn't require kableExtra.
#'
#' @param results Results tibble from calc_risk_diff()
#' @param title Optional title for the table
#'
#' @return A formatted character vector representing the table
#'
#' @examples
#' data(birthweight)
#' results <- calc_risk_diff(birthweight, "low_birthweight", "smoking")
#' cat(create_simple_table(results))
#'
#' @export
create_simple_table <- function(results, title = "Risk Difference Results") {

  # Create simple formatting using base R
  rd_fmt <- sprintf("%.2f%%", results$rd * 100)
  ci_fmt <- sprintf("(%.2f%%, %.2f%%)", results$ci_lower * 100, results$ci_upper * 100)
  p_fmt <- ifelse(results$p_value < 0.001, "<0.001", sprintf("%.3f", results$p_value))

  # Create simple text table
  header <- sprintf("%-20s %-15s %-25s %-10s %-10s",
                    "Exposure", "Risk Diff", "95% CI", "P-value", "Model")
  separator <- paste(rep("=", nchar(header)), collapse = "")

  rows <- character(nrow(results))
  for (i in seq_len(nrow(results))) {
    rows[i] <- sprintf("%-20s %-15s %-25s %-10s %-10s",
                       results$exposure_var[i],
                       rd_fmt[i],
                       ci_fmt[i],
                       p_fmt[i],
                       results$model_type[i])
  }

  # Combine all parts
  table_lines <- c(
    if (!is.null(title)) title else character(0),
    if (!is.null(title)) separator else character(0),
    header,
    separator,
    rows,
    separator
  )

  return(paste(table_lines, collapse = "\n"))
}

# Utility functions for table creation
.detect_strata_vars <- function(results) {
  potential_strata <- c("sex", "residence", "age_group", "group", "stratum")
  intersect(names(results), potential_strata)
}

.clean_column_names <- function(names) {
  clean_names <- names
  clean_names[clean_names == "exposure_var"] <- "Exposure"
  clean_names[clean_names == "rd_formatted"] <- "Risk Difference"
  clean_names[clean_names == "ci_formatted"] <- "95% CI"  # Keep proper capitalization
  clean_names[clean_names == "p_value_formatted"] <- "P-value"
  clean_names[clean_names == "model_type"] <- "Model"

  # Apply title case to other variables, but preserve specific ones we've already set
  preserved_names <- c("Exposure", "Risk Difference", "95% CI", "P-value", "Model")

  for (i in seq_along(clean_names)) {
    if (!clean_names[i] %in% preserved_names) {
      clean_names[i] <- stringr::str_to_title(clean_names[i])
    }
  }

  return(clean_names)
}

.add_table_grouping <- function(table_obj, display_data, strata_vars) {

  if (length(strata_vars) == 1) {
    # Single stratification variable
    strata_col <- stringr::str_to_title(strata_vars[1])
    if (strata_col %in% names(display_data)) {
      groups_info <- display_data %>%
        dplyr::group_by(.data[[strata_col]]) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::arrange(.data[[strata_col]])

      # Create group labels
      group_labels <- sprintf("%s: %s",
                              strata_col,
                              groups_info[[strata_col]])

      # Create index for pack_rows
      group_index <- setNames(groups_info$n, group_labels)

      table_obj <- table_obj %>%
        kableExtra::pack_rows(index = group_index)
    }
  }

  return(table_obj)
}
