#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom purrr imap_dfr
#' @importFrom purrr map2_dfr
#' @importFrom stats var
#' @importFrom utils browseURL
#' @importFrom utils installed.packages
#' @importFrom utils sessionInfo
## usethis namespace: end
NULL

#' @noRd
.onLoad <- function(libname, pkgname) {
  
  # Suppress RGL X11/OpenGL warnings on headless systems
  
  # This prevents "rgl.init failed, will use null device" warnings
  
  # when no display is available (e.g., during R CMD check, CI, SSH)
  
  
  
  if (Sys.getenv("DISPLAY") == "" && .Platform$OS.type == "unix") {
    options(rgl.useNULL = TRUE)
  }
  
  invisible()
}