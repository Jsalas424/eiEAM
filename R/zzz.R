# This hook runs whenever the package is loaded
.onLoad <- function(libname, pkgname) {
  if (requireNamespace("rgl", quietly = TRUE)) {
    # Use null device so rgl doesn't try to open a window in headless environments
    options(rgl.useNULL = TRUE)
  }
}
