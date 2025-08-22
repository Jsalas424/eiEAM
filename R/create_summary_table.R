#' Create a continuous summary table (optionally stratified)
#'
#' Build a `gtsummary` table summarizing numeric variables, optionally
#' stratified by a grouping variable (`by`) and/or split into per-stratum
#' tables via `tbl_strata()` (`strata`). The output is returned as a `gt`
#' table. Defaults:
#' * If `by` is supplied, `include_overall` defaults to **TRUE** (adds an
#'   "Overall" column within each table).
#' * If either `by` or `strata` is supplied, `include_n` defaults to **TRUE**
#'   (adds column Ns; with `strata`, counts are **within each stratum**).
#'
#' @importFrom cli cli_abort
#'
#' @param data A data frame.
#' @param var_name `NULL` or a character vector of column names to summarize.
#'   If `NULL`, all numeric columns (excluding `by`/`strata`) are summarized.
#' @param by Optional grouping variable (bare name or string) to create
#'   column-wise strata in the summary.
#' @param strata Optional variable (bare name or string). When supplied, the
#'   data are split by `strata` and the summary is built **within each stratum**
#'   using `gtsummary::tbl_strata()`.
#' @param include_n Logical; add N per column. If missing, defaults to `TRUE`
#'   when `by` or `strata` is provided, otherwise `FALSE`.
#' @param include_overall Logical; add an "Overall" column. If missing,
#'   defaults to `TRUE` when `by` is provided, otherwise `FALSE`. Ignored
#'   when `by` is `NULL`.
#' @param title Optional title for the `gt` table (markdown supported).
#' @param caption Optional caption for the `gt` table (markdown supported).
#' @param decimals Optional control for number of decimal places displayed in the outputs
#'
#' @return A `gt_tbl` object.
#'
#' @examples
#' \dontrun{
#' # Basic overall (no by/strata): no overall/no N by default
#' create_summary_table(mtcars)
#'
#' # By present -> overall & N default to TRUE
#' create_summary_table(mtcars, by = cyl)
#'
#' # With strata -> overall & N are applied within each stratum
#' create_summary_table(mtcars, by = cyl, strata = gear)
#'
#' # Manually specify variables to summarize
#' create_summary_table(mtcars, var_name = c("mpg","hp"), by = cyl,
#'                      title = "**Engine summary by cyl**")
#' }
#'
#' @export
create_summary_table <- function(
    data,
    var_name = NULL,
    by = NULL,                  # bare name or "string"
    strata = NULL,              # bare name or "string"
    include_n,                  # default set below
    include_overall,            # default set below
    title = NULL,
    caption = NULL,
    decimals = NULL             # <- NEW: integer or vector, e.g. 2 or c(2,2,0)
) {
  # capture (support bare or string)
  by_q     <- rlang::enquo(by)
  strata_q <- rlang::enquo(strata)
  has_by     <- !rlang::quo_is_null(by_q)
  has_strata <- !rlang::quo_is_null(strata_q)

  # defaults
  if (missing(include_overall)) include_overall <- has_by
  if (missing(include_n))       include_n       <- has_by || has_strata

  # auto-pick numeric vars if var_name omitted (exclude by/strata)
  if (is.null(var_name)) {
    num_vars <- names(data)[vapply(data, is.numeric, logical(1))]
    drop <- unique(c(
      if (has_by)     rlang::as_name(by_q)     else character(),
      if (has_strata) rlang::as_name(strata_q) else character()
    ))
    var_name <- setdiff(num_vars, drop)
  }
  if (length(var_name) == 0L) {
    stop("No numeric variables selected. Supply `var_name` or ensure data has numeric columns.")
  }

  # statistics shown (3 rows)
  stats_strings <- c(
    "{mean}\u00B1{sd}",          # mean±SD in Unicode
    "{median} [{p25}-{p75}]",
    "{min}, {max}"
  )
  stats_spec <- list(gtsummary::all_continuous2() ~ stats_strings)

  # digits spec (optional)
  digits_spec <- NULL
  if (!is.null(decimals)) {
    if (!(length(decimals) %in% c(1L, length(stats_strings))))
      stop("`decimals` must be length 1 or ", length(stats_strings), " (one per row).")
    digits_spec <- rlang::list2(gtsummary::all_continuous2() ~ rep(decimals, length(stats_strings)))
  }

  # builder for whole data or within each stratum
  build_tbl <- function(df) {
    cols <- c(if (has_by) rlang::as_name(by_q), var_name)
    df2  <- dplyr::select(df, dplyr::all_of(cols))

    base_args <- list(
      data = df2,
      type = where(is.numeric) ~ "continuous2",
      statistic = stats_spec
    )
    if (!is.null(digits_spec)) base_args$digits <- digits_spec
    if (has_by) base_args$by <- rlang::as_label(by_q)

    tbl <- do.call(gtsummary::tbl_summary, base_args)
    if (include_overall && has_by) tbl <- gtsummary::add_overall(tbl)
    if (include_n)                 tbl <- gtsummary::add_n(tbl)
    tbl
  }

  # single table or per-stratum (counts/overall applied within each stratum)
  tbl_out <- if (has_strata) {
    gtsummary::tbl_strata(
      data     = data,
      strata   = !!strata_q,
      .tbl_fun = build_tbl
    )
  } else {
    build_tbl(data)
  }

  # finalize as gt with optional title/caption
  gt_tbl <- gtsummary::as_gt(tbl_out)
  if (!is.null(title))   gt_tbl <- gt::tab_header(gt_tbl, title = gt::md(title))
  if (!is.null(caption)) gt_tbl <- gt::tab_caption(gt_tbl, gt::md(caption))
  gt_tbl
}
