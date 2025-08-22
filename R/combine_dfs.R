#' Combine DF's across pacing directions within a timepoint
#'
#' @param id Subject id
#' @param grouping The group you're intending to overwite ("lv_volt" vs. "lat_coord" vs. etc.)
#' @param df Data frame containing data from the pacing direction that you want to merge
#' @param base_dir Directory where you're saving this data to
#' @param suffix Any suffix you want to add to the data file name, name looks like step2_t2_rvpaced_full.rds
#' @param step What step in your process are you in?
#' @param timepoint The timepoint that you're currently working with
#' @param wavefront The pacing wavefront that you're currently merging in
#' @param grouping "carto_mesh" or "lv_volt" or other group that you've filtering within
#'
#' @return Creates a file in wherever we declared our base_dir
#'
#' @export
combine_rds_pacing_dir_v2 <- function(id,
                                     df,
                                     grouping,
                                     base_dir = "data/",
                                     suffix = "_full",
                                     step,
                                     timepoint,
                                     wavefront){
  # Avoid global variable binding issues
  group <- pacing_dir <- NULL
  
  # We assume the file looks something like "data/processed/working_data/step2_t2_rvpaced_full.rds"
  # so we provided each of those individual elements and combine here
  file_dir <- paste0(base_dir,
                     step, "_",
                     timepoint,
                     suffix,
                     ".rds")

  # Check if the combined file exists
  if (file.exists(file_dir)) {
    # If the file exists, check that the id matches the one we're currently trying to merge
    tmp_id <- readRDS(paste0(file_dir))[1, "id"]
    if (tmp_id == id) {
      # If the ID matches, look for the pacing_dir that you're currently trying to update
      # and filter it out. Add the current df/wavefront that you just processed.
      # This assumes that we're combining a master DF in the preceding step
      tmp_combined <- readRDS(paste0(file_dir)) |>
        dplyr::filter(!(group == grouping & pacing_dir == wavefront)) |>
        # Add the current df/wavefront to the combined df
        dplyr::bind_rows(df |>
                           filter(group == grouping & (pacing_dir == wavefront |
                                    is.na(pacing_dir)) == TRUE))
      # Save the now combined data to your directory
      saveRDS(tmp_combined, file_dir)
      paste0("Saved ",
             tmp_id,
             " ",
             pacing_dir,
             " ",
             timepoint,
             " to ",
             file_dir)
    } else {
      # If the ID's don't match, overwrite the current working df and print an error code
      saveRDS(df, file_dir)
      warning(
        paste0(
          "The combined data in the working data directory belonged to ",
          tmp_id,
          ", not ",
          id,
          " and has now been overwritten."
        )
      )
    }

    # If the file doesn't exist, save the input df as the combined df
  } else{
    saveRDS(df, file_dir)
  }

}

