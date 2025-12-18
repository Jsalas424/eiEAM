#' Clean R Environment and Detach Non-Base Packages
#'
#' Clears all objects from the global environment and detaches non-base packages.
#' Safe for RMarkdown rendering; in non-interactive sessions, detachment is skipped
#' to avoid breaking the knit process.
#'
#' @param exclude Character vector of packages to keep loaded. Default is all base packages.
#' @param silent Logical, if TRUE suppress messages. Default is TRUE.
#' @export
#' @importFrom utils installed.packages sessionInfo
clean_environment <- function(
    exclude = rownames(utils::installed.packages(priority = "base")),
    silent = TRUE
) {
  # --- Skip detachment if running in RMarkdown render ---
  if (nzchar(Sys.getenv("RSTUDIO")) && !interactive()) {
    if (!silent) message("Non-interactive session detected; skipping package detachment for RMarkdown safety.")
  } else {
    # --- Detach non-base packages ---
    loaded_pkgs <- names(utils::sessionInfo()$otherPkgs)
    pkgs_to_detach <- setdiff(loaded_pkgs, exclude)
    invisible(lapply(pkgs_to_detach, function(pkg) {
      try(detach(
        name = paste0("package:", pkg),
        character.only = TRUE,
        unload = TRUE,
        force = TRUE
      ), silent = silent)
    }))

    if (!silent && length(pkgs_to_detach) > 0) {
      message("Detached packages: ", paste(pkgs_to_detach, collapse = ", "))
    }
  }

  # --- Remove all objects from global environment ---
  objs <- ls(all.names = TRUE, envir = .GlobalEnv)
  if (length(objs) > 0) {
    rm(list = objs, envir = .GlobalEnv)
    if (!silent) message("Removed objects from global environment: ", paste(objs, collapse = ", "))
  }

  invisible(NULL)
}
