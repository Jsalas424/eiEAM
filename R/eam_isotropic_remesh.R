#' EAM Isotropic Remeshing with ROI Optimization
#'
#' @description
#' Performs isotropic remeshing using Rvcg and optimizes the mesh by
#' removing unused vertices and creating clean vertex/face data structures suitable
#' for cardiovascular mesh analysis.
#'
#' @param mesh3d_obj A mesh3d object from the rgl package to be remeshed
#' @param target_edge_length Numeric. Target edge length for uniform triangle sizing.
#' @param iterations Integer. Number of remeshing iterations to perform (default: 5).
#' @param feature_angle_deg Numeric. Angle threshold in degrees for preserving sharp features (default: 90).
#' @param max_surf_dist Numeric. Maximum allowed surface distance during remeshing (default: 2).
#' @param surf_dist_check Logical. Whether to monitor distance to original surface. When TRUE, we maintain higher fidelity to the original mesh. For EAM reprocessing, we want this FALSE. We want to algorithm to freely restructure triangles. (default: FALSE).
#' @param adaptive Logical. Whether to use adaptive triangle sizing. When true, this will generate different triangle sizes depending on the curvature of the mesh at that point. For EAM post-processing purposes, we usually want this FALSE. (default: FALSE).
#' @param split,collapse,swap,smooth,project Logical flags for Rvcg remesher (all TRUE by default).
#'
#' @return A list with `vertices_roi`, `faces_roi`, `mesh3d_roi`,
#'   `original_vertex_count`, `roi_vertex_count`, and `face_count`.
#'
#' @examples
#' ## Minimal, non-interactive example (CRAN-safe):
#' if (requireNamespace("rgl", quietly = TRUE) &&
#'     requireNamespace("Rvcg", quietly = TRUE)) {
#'   # Build a small mesh without opening a graphics device
#'   mesh <- rgl::icosahedron3d()
#'   mesh <- rgl::subdivision3d(mesh, depth = 1)   # keep small for speed
#'   # Run remeshing quickly (few iterations, coarse target length)
#'   res <- eam_isotropic_remesh(
#'     mesh3d_obj         = mesh,
#'     target_edge_length = 0.5,
#'     iterations         = 2,
#'     surf_dist_check    = FALSE,
#'     adaptive           = FALSE
#'   )
#'   # Light checks / output that don't require a display
#'   stopifnot(
#'     is.list(res),
#'     all(c("vertices_roi","faces_roi","mesh3d_roi") %in% names(res)),
#'     nrow(res$vertices_roi) > 0L,
#'     nrow(res$faces_roi)    > 0L
#'   )
#'   print(c(
#'     original_vertices = res$original_vertex_count,
#'     roi_vertices      = res$roi_vertex_count,
#'     roi_faces         = res$face_count
#'   ))
#' }
#' ## Optional interactive visualization (not run on CRAN):
#' \dontrun{
#' if (interactive() &&
#'     requireNamespace("rgl", quietly = TRUE) &&
#'     requireNamespace("Rvcg", quietly = TRUE)) {
#'   mesh <- rgl::subdivision3d(rgl::icosahedron3d(), depth = 2)
#'   res  <- eam_isotropic_remesh(mesh, target_edge_length = 0.25, iterations = 3)
#'   rgl::clear3d()
#'   rgl::shade3d(mesh, color = "deepskyblue", alpha = 0.35); rgl::wire3d(mesh, color = "blue")
#'   rgl::shade3d(res$mesh3d_roi, color = "tomato", alpha = 0.5)
#'   rgl::wire3d(res$mesh3d_roi, color = "red")
#'   rgl::axes3d(); rgl::aspect3d(1,1,1)
#' }
#' }
#'
#' @export
eam_isotropic_remesh <- function(
    mesh3d_obj,
    target_edge_length,
    iterations = 5,
    feature_angle_deg = 90,
    max_surf_dist = 2,
    surf_dist_check = FALSE,
    adaptive = FALSE,
    split = TRUE,
    collapse = TRUE,
    swap = TRUE,
    smooth = TRUE,
    project = TRUE
) {

  # Store original vertex count for reporting
  original_vertex_count <- ncol(mesh3d_obj$vb)

  # Perform isotropic remeshing using Rvcg
  remeshed <- Rvcg::vcgIsotropicRemeshing(
    mesh3d_obj,
    TargetLen = target_edge_length,
    iterations = iterations,
    FeatureAngleDeg = feature_angle_deg,
    MaxSurfDist = max_surf_dist,
    surfDistCheck = surf_dist_check,
    Adaptive = adaptive,
    split = split,
    collapse = collapse,
    swap = swap,
    smooth = smooth,
    project = project
  )

  # Post-remeshing cleaning pipeline
  remeshed <- remeshed |>
    Rvcg::vcgUpdateNormals() |>
    Rvcg::vcgClean(sel = 0.7, iterate = TRUE) |>
    Rvcg::vcgIsolated(facenum = 0)

  # Extract vertex and face data for processing
  vertices_matrix <- t(remeshed$vb[1:3, ])
  faces_matrix <- t(remeshed$it)

  # Create vertices dataframe with row indices using base R
  vertices_df <- as.data.frame(vertices_matrix)
  names(vertices_df) <- c("surf_x", "surf_y", "surf_z")
  vertices_df$idx <- seq_len(nrow(vertices_df))

  # Create faces dataframe
  faces_df <- as.data.frame(faces_matrix)

  # Find ROI vertices (vertices actually referenced in faces)
  roi_vertex_indices <- unique(as.vector(faces_matrix))
  vertices_df$ROI <- vertices_df$idx %in% roi_vertex_indices

  # Extract only ROI vertices and create new sequential indices
  vertices_roi_df <- vertices_df[vertices_df$ROI == TRUE, ]
  vertices_roi_df$new_idx <- seq_len(nrow(vertices_roi_df))
  vertices_roi_df <- vertices_roi_df[, c("surf_x", "surf_y", "surf_z", "idx", "new_idx")]

  # Vectorized face index remapping
  face_indices_vector <- as.vector(faces_matrix)
  lookup_table <- stats::setNames(vertices_roi_df$new_idx, vertices_roi_df$idx)
  remapped_indices <- lookup_table[as.character(face_indices_vector)]

  # Handle any missing values
  if(any(is.na(remapped_indices))) {
    warning("Some face indices could not be remapped - check mesh integrity")
    remapped_indices[is.na(remapped_indices)] <- 1
  }

  # Reshape to face matrix and ensure integer type
  remesh_faces_roi <- matrix(
    remapped_indices,
    ncol = 3,
    nrow = nrow(faces_df),
    byrow = FALSE
  )
  storage.mode(remesh_faces_roi) <- "integer"

  # Create final mesh3d object with only ROI vertices/faces
  final_mesh_roi <- rgl::tmesh3d(
    vertices = t(as.matrix(vertices_roi_df[, 1:3])),
    indices = t(remesh_faces_roi),
    homogeneous = FALSE
  )

  # Prepare output data frames
  vertices_roi_final <- vertices_roi_df[, c("surf_x", "surf_y", "surf_z", "idx")]

  faces_roi_final <- as.data.frame(remesh_faces_roi)
  names(faces_roi_final) <- c("v1", "v2", "v3")

  # Return comprehensive results
  list(
    vertices_roi = vertices_roi_final,
    faces_roi = faces_roi_final,
    mesh3d_roi = final_mesh_roi,
    original_vertex_count = original_vertex_count,
    roi_vertex_count = nrow(vertices_roi_df),
    face_count = nrow(remesh_faces_roi)
  )
}
