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
#' data(cachar_sample)
#' results <- calc_risk_diff(cachar_sample, "abnormal_screen", "smoking")
#'
#' # Basic table (works without kableExtra)
#' basic_table <- create_rd_table(results, caption = "Risk of Abnormal Cancer Screening")
#' print(basic_table)
#'
#' # Enhanced table (requires kableExtra)
#' if (requireNamespace("kableExtra", quietly = TRUE)) {
#'   enhanced_table <- create_rd_table(
#'     results,
#'     caption = "Risk of Abnormal Cancer Screening by Smoking Status",
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
#' data(cachar_sample)
#' results <- calc_risk_diff(cachar_sample, "abnormal_screen", "smoking")
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

#' Create Forest Plot for Risk Difference Results
#'
#' @description
#' Creates a forest plot visualization of risk difference results,
#' automatically detecting stratification variables and creating appropriate labels.
#'
#' @param results Results tibble from calc_risk_diff()
#' @param title Plot title (default: "Risk Differences")
#' @param max_ci_width Maximum CI width for display (default: 50)
#' @param ... Additional arguments passed to ggplot
#'
#' @return A ggplot object
#'
#' @examples
#' data(cachar_sample)
#' results <- calc_risk_diff(cachar_sample, "abnormal_screen", "areca_nut", strata = "residence")
#' create_forest_plot(results)
#'
#' @export
create_forest_plot <- function(results,
                               title = "Risk Differences",
                               max_ci_width = 50,
                               ...) {

  # Detect stratification variables automatically
  standard_cols <- c("exposure_var", "rd", "ci_lower", "ci_upper", "p_value",
                     "model_type", "n_obs", "on_boundary", "boundary_type", "ci_method")

  # Find stratification columns (any columns not in standard set)
  strata_cols <- setdiff(names(results), standard_cols)

  # Create labels BEFORE the mutate call to avoid scoping issues
  if (length(strata_cols) == 0) {
    # No stratification
    label_values <- paste0("Overall\n(n=", results$n_obs, ")")
  } else if (length(strata_cols) == 1) {
    # Single stratification variable
    strata_col <- strata_cols[1]
    strata_values <- results[[strata_col]]
    label_values <- paste0(strata_values, "\n(n=", results$n_obs, ")")
  } else {
    # Multiple stratification variables
    label_values <- character(nrow(results))
    for (i in seq_len(nrow(results))) {
      values <- sapply(strata_cols, function(col) as.character(results[[col]][i]))
      combined_value <- paste(values, collapse = paste(" ", .safe_times(), " "))
      label_values[i] <- paste0(combined_value, "\n(n=", results$n_obs[i], ")")
    }
  }

  # Create plot data with pre-computed labels
  plot_data <- results %>%
    dplyr::mutate(
      label = label_values,
      rd_percent = rd * 100,
      ci_lower_percent = ci_lower * 100,
      ci_upper_percent = ci_upper * 100,
      # Flag very wide CIs
      ci_width = ci_upper_percent - ci_lower_percent,
      is_stable = ci_width <= max_ci_width & !is.na(rd)
    )

  # Filter out unstable estimates if needed
  stable_data <- plot_data %>%
    dplyr::filter(is_stable)

  if (nrow(stable_data) == 0) {
    warning("No stable estimates to plot (all CIs too wide)")
    return(NULL)
  }

  # Create the plot
  p <- ggplot2::ggplot(stable_data, ggplot2::aes(x = rd_percent, y = label)) +
    ggplot2::geom_point(size = 3, color = "darkblue") +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = ci_lower_percent, xmax = ci_upper_percent),
      height = 0.2,
      color = "darkblue"
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
    ggplot2::labs(
      title = title,
      subtitle = if (nrow(plot_data) > nrow(stable_data)) {
        paste("Showing", nrow(stable_data), "of", nrow(plot_data), "estimates (others too unstable)")
      } else {
        paste("All", nrow(stable_data), "estimates shown")
      },
      x = "Risk Difference (percentage points)",
      y = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11, color = "gray60"),
      axis.text.y = ggplot2::element_text(size = 11),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Add boundary indicators if available
  if ("on_boundary" %in% names(stable_data)) {
    boundary_data <- stable_data %>%
      dplyr::filter(on_boundary)

    if (nrow(boundary_data) > 0) {
      p <- p +
        ggplot2::geom_point(
          data = boundary_data,
          ggplot2::aes(x = rd_percent, y = label),
          shape = 21, size = 4, color = "orange", fill = "transparent", stroke = 1.5
        ) +
        ggplot2::labs(
          caption = "Orange circles indicate boundary cases with robust confidence intervals"
        )
    }
  }

  return(p)
}

#' Create Summary Table for Risk Difference Results
#'
#' @description
#' Creates a formatted summary table that works with any stratification variables.
#'
#' @param results Results tibble from calc_risk_diff()
#' @param caption Table caption
#'
#' @return A data frame suitable for knitr::kable()
#'
#' @export
create_summary_table <- function(results, caption = "Risk Difference Results") {

  # Detect stratification variables
  standard_cols <- c("exposure_var", "rd", "ci_lower", "ci_upper", "p_value",
                     "model_type", "n_obs", "on_boundary", "boundary_type", "ci_method")
  strata_cols <- setdiff(names(results), standard_cols)

  # Create formatted table
  formatted_results <- results %>%
    dplyr::mutate(
      Risk_Difference = sprintf("%.1f%%", rd * 100),
      CI_95 = sprintf("(%.1f%%, %.1f%%)", ci_lower * 100, ci_upper * 100),
      P_value = dplyr::case_when(
        is.na(p_value) ~ .safe_em_dash(),
        p_value < 0.001 ~ "<0.001",
        TRUE ~ sprintf("%.3f", p_value)
      )
    )

  # Select columns dynamically
  table_cols <- c(strata_cols, "Risk_Difference", "CI_95", "P_value", "model_type", "n_obs")
  table_cols <- intersect(table_cols, names(formatted_results))  # Only include existing columns

  # Rename stratification columns to title case
  result_table <- formatted_results %>%
    dplyr::select(dplyr::all_of(table_cols))

  # Clean up column names
  names(result_table)[names(result_table) == "model_type"] <- "Model"
  names(result_table)[names(result_table) == "n_obs"] <- "N"

  # Make stratification column names title case
  for (col in strata_cols) {
    if (col %in% names(result_table)) {
      names(result_table)[names(result_table) == col] <- stringr::str_to_title(col)
    }
  }

  return(result_table)
}
