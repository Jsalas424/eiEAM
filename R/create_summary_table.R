#' Preprocess data for summary table
#'
#' Converts character columns to factors with NA values coded as "Unknown",
#' converts difftime objects to numeric, and drops unused factor levels.
#'
#' @param data A data frame to preprocess.
#'
#' @return A processed data frame.
#' @noRd
prep_summary_data <- function(data) {
  dplyr::mutate(
    data,
    dplyr::across(
      dplyr::where(is.character),
      ~ {
        f <- as.factor(.x)
        forcats::fct_drop(forcats::fct_na_value_to_level(f, level = "Unknown"))
      }
    ),
    dplyr::across(
      dplyr::where(~ inherits(.x, "difftime")),
      as.numeric
    ),
    dplyr::across(
      dplyr::where(is.factor),
      forcats::fct_drop
    )
  )
}

#' Build digits specification for tbl_summary
#'
#' Constructs the digits argument list for \code{gtsummary::tbl_summary()}.
#'
#' @param digits NULL or numeric vector of length 1 or 2. If length 1, applied
#'   to all statistics. If length 2, first value applies to mean/SD, second to
#'   median/IQR/min/max.
#'
#' @return A list suitable for \code{gtsummary::tbl_summary(digits = ...)}, or
#'
#' @noRd
build_digits_spec <- function(digits) {
  if (is.null(digits)) {
    return(NULL)
  }
  
  if (length(digits) == 1L) {
    digits <- rep(digits, 2L)
  }
  
  list(
    gtsummary::all_continuous2() ~ list(
      mean = digits[1],
      sd = digits[1],
      median = digits[2],
      p25 = digits[2],
      p75 = digits[2],
      min = digits[2],
      max = digits[2]
    )
  )
}

#' Build a single summary table
#'
#' Creates a \code{gtsummary::tbl_summary} object with optional grouping,
#' overall column, and observation counts.
#'
#' @param data Preprocessed data frame.
#' @param var_name Character vector of variable names to summarize.
#' @param by_quo Quosure for the \code{by} variable.
#' @param has_by Logical indicating if \code{by} is specified.
#' @param has_strata Logical indicating if \code{strata} is specified.
#' @param include_overall Logical indicating whether to add overall column.
#' @param include_n Logical indicating whether to add N column.
#' @param digits_spec Digits specification from \code{build_digits_spec()}.
#'
#' @return A gtsummary tbl_summary object.
#' @noRd
build_summary_tbl <- function(data,
                              var_name,
                              by_quo,
                              has_by,
                              has_strata,
                              include_overall,
                              include_n,
                              digits_spec) {
  # Define summary statistics (Unicode escape for plus-minus symbol)
  stats_strings <- c(
    "{mean} \u00B1 {sd}",
    "{median} [{p25}-{p75}]",
    "{min}, {max}"
  )
  
  cols <- c(if (has_by) rlang::as_name(by_quo), var_name)
  df <- dplyr::select(data, dplyr::all_of(cols))
  
  # Automatically determine missing argument behavior:
  # - Use "always" when strata is present to prevent row disassociation
  # - Use gtsummary default "ifany" otherwise
  missing_arg <- if (has_strata) "always" else "ifany"
  
  base_args <- list(
    data = df,
    type = list(
      dplyr::where(is.numeric) ~ "continuous2",
      dplyr::where(~ !is.numeric(.x)) ~ "categorical"
    ),
    statistic = list(
      gtsummary::all_continuous2() ~ stats_strings,
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    missing = missing_arg,
    missing_text = "Unknown"
  )
  
  if (!is.null(digits_spec)) {
    base_args$digits <- digits_spec
  }
  
  if (has_by) {
    base_args$by <- rlang::as_label(by_quo)
  }
  
  tbl <- do.call(gtsummary::tbl_summary, base_args)
  
  if (include_overall && has_by) {
    tbl <- gtsummary::add_overall(tbl)
  }
  if (include_n) {
    tbl <- gtsummary::add_n(tbl)
  }
  
  tbl
}

#' Create a Formatted Summary Table
#'
#' @description
#' This function wraps \code{\link[gtsummary]{tbl_summary}} to generate a summary
#' table with optional stratification, grouping, and overall summaries. Continuous
#' variables are summarized by mean (SD), median [IQR], and range, while
#' categorical variables are summarized as counts (percentages).
#'
#' @details
#' # Handling Missing Values with Stratification
#'
#' When using stratification (\code{strata} argument), the function automatically
#' sets \code{missing = "always"} to ensure missing value rows remain properly
#' associated with their parent variables during table merging operations. This
#' prevents missing rows from becoming disassociated and appearing at the bottom
#' of the table. When no stratification is used, the function uses the gtsummary
#' default behavior (\code{missing = "ifany"}), which displays missing rows only
#' when missing values are present.
#'
#' # Factor Handling
#'
#' Character variables are automatically converted to factors with missing values
#' coded as "Unknown". Factor levels are dropped to show only observed values.
#' \code{difftime} objects are converted to numeric for summary calculations.
#'
#' @param data A data frame containing the variables to summarize.
#' @param var_name Character vector of variable names to include in the summary.
#'   If \code{NULL} (default), all columns except \code{by} and \code{strata}
#'   are summarized.
#' @param by Bare column name or character string specifying the grouping variable.
#'   Summary statistics will be stratified by this variable. Default is \code{NULL}.
#' @param strata Bare column name or character string specifying the stratification
#'   variable. Creates separate tables for each stratum level. Default is \code{NULL}.
#' @param include_n Logical indicating whether to include a column showing the
#'   number of non-missing observations. If not specified, defaults to \code{TRUE}
#'   when \code{by} or \code{strata} is provided, \code{FALSE} otherwise.
#' @param include_overall Logical indicating whether to include an "Overall" column
#'   when \code{by} is specified. If not specified, defaults to \code{TRUE} when
#'   \code{by} is provided.
#' @param title Optional character string for table title. Markdown formatting is
#'   supported via \code{\link[gt]{md}}. Default is \code{NULL}.
#' @param caption Optional character string for table caption. Markdown formatting is
#'   supported via \code{\link[gt]{md}}. Default is \code{NULL}.
#' @param digits Optional specification for the number of decimal places used to
#'   round continuous statistics. Must be either:
#'   \itemize{
#'     \item A single integer: applied to all statistics (mean, SD, median, IQR,
#'       min, max)
#'     \item A vector of length 2: first value for mean/SD, second for
#'       median/IQR/min/max
#'   }
#'   When \code{NULL} (default), gtsummary determines appropriate rounding
#'   automatically.
#' @param grand_overall Logical indicating whether to add a grand overall column
#'   when both \code{by} and \code{strata} are specified. If \code{NULL} (default),
#'   automatically set to \code{TRUE} when both \code{by} and \code{strata} are
#'   provided, \code{FALSE} otherwise.
#' @param grand_label Character string for the grand overall column header when
#'   \code{grand_overall = TRUE}. Markdown is supported. Default is \code{"**All**"}.
#'
#' @return A \code{gt_tbl} object that can be further customized with \pkg{gt}
#'   functions.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[gtsummary]{tbl_summary}} for the underlying summary function
#'   \item \code{\link[gtsummary]{tbl_strata}} for stratified table creation
#'   \item \code{\link[gtsummary]{tbl_merge}} for merging tables
#'   \item \code{\link[gt]{gt}} for additional table customization
#' }
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(dplyr)
#'
#' # Create example data
#' set.seed(123)
#' df <- data.frame(
#'   group   = sample(c("A", "B"), 100, replace = TRUE),
#'   strata  = sample(c("X", "Y"), 100, replace = TRUE),
#'   age     = rnorm(100, mean = 50, sd = 10),
#'   weight  = rnorm(100, mean = 70, sd = 15),
#'   sex     = sample(c("Male", "Female"), 100, replace = TRUE),
#'   outcome = sample(c("Yes", "No"), 100, replace = TRUE)
#' )
#'
#' # Example 1: Basic summary table
#' create_summary_table(df, var_name = c("age", "weight", "sex"))
#'
#' # Example 2: Summary by group
#' create_summary_table(df, var_name = c("age", "sex"), by = group)
#'
#' # Example 3: Stratified table (automatically uses missing = "always")
#' create_summary_table(
#'   df,
#'   var_name = c("age", "weight"),
#'   by = group,
#'   strata = strata
#' )
#'
#' # Example 4: Custom decimal places
#' create_summary_table(
#'   df,
#'   var_name = "age",
#'   by = group,
#'   digits = c(1, 0),
#'   title = "Age Summary by Group"
#' )
#'
#' # Example 5: Grand overall column
#' create_summary_table(
#'   df,
#'   var_name = c("age", "sex"),
#'   by = group,
#'   strata = strata,
#'   grand_overall = TRUE,
#'   grand_label = "**All Participants**"
#' )
#' }
create_summary_table <- function(data,
                                 var_name = NULL,
                                 by = NULL,
                                 strata = NULL,
                                 include_n,
                                 include_overall,
                                 title = NULL,
                                 caption = NULL,
                                 digits = NULL,
                                 grand_overall = NULL,
                                 grand_label = "**All**") {
  # Check required packages
  if (!requireNamespace("gtsummary", quietly = TRUE)) {
    stop("Package 'gtsummary' is required but not installed.", call. = FALSE)
  }
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Package 'gt' is required but not installed.", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.", call. = FALSE)
  }
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Package 'purrr' is required but not installed.", call. = FALSE)
  }
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("Package 'rlang' is required but not installed.", call. = FALSE)
  }
  if (!requireNamespace("forcats", quietly = TRUE)) {
    stop("Package 'forcats' is required but not installed.", call. = FALSE)
  }
  
  # Capture enquoted arguments
  by_quo <- rlang::enquo(by)
  strata_quo <- rlang::enquo(strata)
  has_by <- !rlang::quo_is_null(by_quo)
  has_strata <- !rlang::quo_is_null(strata_quo)
  
  # Set defaults for include_n and include_overall
  if (missing(include_overall)) {
    include_overall <- has_by
  }
  if (missing(include_n)) {
    include_n <- has_by || has_strata
  }
  if (is.null(grand_overall)) {
    grand_overall <- has_by && has_strata
  }
  
  # Determine variables to summarize
  if (is.null(var_name)) {
    drop <- unique(c(
      if (has_by) rlang::as_name(by_quo) else character(),
      if (has_strata) rlang::as_name(strata_quo) else character()
    ))
    var_name <- setdiff(names(data), drop)
  }
  
  if (length(var_name) == 0L) {
    stop(
      "No variables selected. Supply var_name or ensure data has columns beyond by/strata.",
      call. = FALSE
    )
  }
  
  # Validate digits argument
  if (!is.null(digits) && !(length(digits) %in% c(1L, 2L))) {
    stop(
      "`digits` must be length 1 or 2 (first for mean/SD, second for median/IQR/min/max).",
      call. = FALSE
    )
  }
  
  # Build digits specification
  digits_spec <- build_digits_spec(digits)
  
  # Preprocess data
  data_processed <- prep_summary_data(data)
  
  # Create tables based on stratification
  if (has_strata && grand_overall) {
    tbl_all <- build_summary_tbl(
      data = data_processed,
      var_name = var_name,
      by_quo = by_quo,
      has_by = has_by,
      has_strata = has_strata,
      include_overall = include_overall,
      include_n = include_n,
      digits_spec = digits_spec
    )
    
    strata_col <- rlang::as_name(strata_quo)
    strata_vals <- data_processed[[strata_col]]
    
    strata_levels <- if (is.factor(strata_vals)) {
      levels(strata_vals)[levels(strata_vals) %in% unique(strata_vals)]
    } else {
      unique(strata_vals)
    }
    strata_levels <- strata_levels[!is.na(strata_levels)]
    
    tbl_list <- purrr::map(
      strata_levels,
      ~ build_summary_tbl(
        data = dplyr::filter(data_processed, !!strata_quo == .x),
        var_name = var_name,
        by_quo = by_quo,
        has_by = has_by,
        has_strata = has_strata,
        include_overall = include_overall,
        include_n = include_n,
        digits_spec = digits_spec
      )
    )
    
    tbl_out <- gtsummary::tbl_merge(
      tbls = c(list(tbl_all), tbl_list),
      tab_spanner = c(grand_label, paste0("**", strata_levels, "**"))
    )
  } else if (has_strata) {
    tbl_out <- gtsummary::tbl_strata(
      data = data_processed,
      strata = !!strata_quo,
      .tbl_fun = ~ build_summary_tbl(
        data = .x,
        var_name = var_name,
        by_quo = by_quo,
        has_by = has_by,
        has_strata = has_strata,
        include_overall = include_overall,
        include_n = include_n,
        digits_spec = digits_spec
      )
    )
  } else {
    tbl_out <- build_summary_tbl(
      data = data_processed,
      var_name = var_name,
      by_quo = by_quo,
      has_by = has_by,
      has_strata = has_strata,
      include_overall = include_overall,
      include_n = include_n,
      digits_spec = digits_spec
    )
  }
  
  # Convert to gt table
  gt_tbl <- gtsummary::as_gt(tbl_out)
  
  if (!is.null(title)) {
    gt_tbl <- gt::tab_header(gt_tbl, title = gt::md(title))
  }
  if (!is.null(caption)) {
    gt_tbl <- gt::tab_caption(gt_tbl, caption = gt::md(caption))
  }
  
  gt_tbl
}
