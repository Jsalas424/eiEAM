## code to prepare `sample_mesh` dataset

# This script creates the sample_mesh dataset from a PLY file
# Run this script from the package root directory

# Requirements:
# - Rvcg package for reading PLY files
# - The source PLY file

library(Rvcg)

# Read the mesh from PLY file
# Note: Update the path to your PLY file location
ply_path <- "data-raw/carto_lv_sr_t1_remeshed.ply"

if (!file.exists(ply_path)) {
  stop(
    "PLY file not found at: ", ply_path, "\n",
    "Please copy your PLY file to data-raw/ directory.",
    call. = FALSE
  )
}

sample_mesh <- Rvcg::vcgPlyRead(ply_path, updateNormals = FALSE, clean = FALSE)

# Validate the mesh
stopifnot(
  inherits(sample_mesh, "mesh3d"),
  !is.null(sample_mesh$vb),
  !is.null(sample_mesh$it)
)

cat(sprintf(
  "sample_mesh created:\n  Vertices: %d\n  Faces: %d\n",
  ncol(sample_mesh$vb),
  ncol(sample_mesh$it)
))

# Save to data/ directory
usethis::use_data(sample_mesh, overwrite = TRUE)

# Check file size
rda_path <- "data/sample_mesh.rda"
if (file.exists(rda_path)) {
  size_kb <- file.size(rda_path) / 1024
  cat(sprintf("File size: %.1f KB\n", size_kb))
  
  if (size_kb > 1000) {
    warning(
      "sample_mesh.rda is larger than 1 MB. ",
      "Consider using compression or qs package."
    )
  }
}