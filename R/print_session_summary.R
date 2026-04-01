#' Print a clean R session summary with hardcoded RStudio version
#'
#' Displays R version, hardcoded RStudio version, and attached packages with versions.
#' The RStudio version should be set in the variable RSTUDIO_VERSION before knitting.
#'
#' @param rstudio_version Optional character string with the RStudio version to display.
#' @importFrom dplyr filter select all_of
#' @importFrom rlang .data
#' @importFrom sessioninfo package_info
#' @export
print_session_summary <- function(rstudio_version = NULL) {
  # --- R version ---
  r_ver <- paste(R.version$version.string, R.version$nickname)

  # --- Use provided or fallback RStudio version ---
  rstudio_ver <- if (!is.null(rstudio_version)) {
    rstudio_version
  } else {
    "RStudio not detected"
  }

  # --- Continue with attached packages and printing ---
  pkg_info <- tryCatch({
    sessioninfo::package_info() |> as.data.frame()
  }, error = function(e) data.frame(package = character(0), loadedversion = character(0)))

  cols_to_select <- intersect(c("package", "loadedversion", "version"), names(pkg_info))

  pkg_info_attached <- pkg_info |>
    dplyr::filter(.data$attached == TRUE) |>
    dplyr::select(dplyr::all_of(cols_to_select))

  # --- Print nicely ---
  cat("===== R Session Summary =====\n")
  cat("R:", r_ver, "\n")
  cat(rstudio_ver, "\n\n")

  cat("Attached Packages:\n")
  if (nrow(pkg_info_attached) > 0) {
    print(pkg_info_attached, row.names = FALSE)
  } else {
    cat("No attached packages found.\n")
  }

  cat("=============================\n")
}
