#' Create a wireframe mesh
#'
#' @description Meshing and remeshing is central to voltage and conduction velocity. This tool allows us to visualize the mesh in plotly.
#'
#' @param mesh3d_obj A mesh3d object from the rgl package
#'
#' @return A data frame with x, y, z coordinates for wireframe edges compatible with plotly
#' @examples
#' # Only run if rgl is available
#' if (requireNamespace("rgl", quietly = TRUE)) {
#'   # Create a simple triangular mesh (tetrahedron)
#'   vertices <- rbind(
#'     c(0, 0, 0),    # vertex 1
#'     c(1, 0, 0),    # vertex 2
#'     c(0.5, 1, 0),  # vertex 3
#'     c(0.5, 0.5, 1) # vertex 4
#'   )
#'
#'   faces <- rbind(
#'     c(1, 2, 3),    # bottom face
#'     c(1, 2, 4),    # front face
#'     c(2, 3, 4),    # right face
#'     c(1, 3, 4)     # left face
#'   )
#'
#'   # Create mesh3d object
#'   test_mesh <- rgl::tmesh3d(
#'     vertices = t(vertices),
#'     indices = t(faces),
#'     homogeneous = FALSE
#'   )
#'
#'   # Generate wireframe coordinates
#'   wireframe_coords <- create_wireframe_mesh(test_mesh)
#'   print(head(wireframe_coords))
#' }
#'
#' @export
create_wireframe_mesh <- function(mesh3d_obj) {
  # Check if rgl is available
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Package 'rgl' is required but not available. Please install it with: install.packages('rgl')")
  }

  # Validate input
  if (!inherits(mesh3d_obj, "mesh3d")) {
    stop("Input must be a mesh3d object from the rgl package")
  }

  # Extract vertices and faces from mesh3d object
  vertices_matrix <- t(mesh3d_obj$vb[1:3, ])  # Convert to n x 3 matrix
  faces_matrix <- t(mesh3d_obj$it)            # Convert to n x 3 matrix

  # Ensure faces are numeric and create edge vectors directly
  face1 <- as.numeric(faces_matrix[, 1])
  face2 <- as.numeric(faces_matrix[, 2])
  face3 <- as.numeric(faces_matrix[, 3])

  # Create all edge pairs as vectors
  from_edges <- c(face1, face2, face3)
  to_edges <- c(face2, face3, face1)

  # Create data frame with explicit numeric conversion
  all_edges <- data.frame(
    from = as.numeric(from_edges),
    to = as.numeric(to_edges),
    stringsAsFactors = FALSE
  )

  # Remove duplicates and sort using explicit numeric vectors
  edges <- unique(all_edges)
  edge_order <- order(as.numeric(edges$from), as.numeric(edges$to))
  edges <- edges[edge_order, ]

  # Get coordinates for each edge endpoint
  coords1 <- vertices_matrix[edges$from, ]
  coords2 <- vertices_matrix[edges$to, ]

  # Create wireframe coordinates with line breaks (NAs)
  n_edges <- nrow(edges)
  x_coords <- c(rbind(coords1[, 1], coords2[, 1], rep(NA, n_edges)))
  y_coords <- c(rbind(coords1[, 2], coords2[, 2], rep(NA, n_edges)))
  z_coords <- c(rbind(coords1[, 3], coords2[, 3], rep(NA, n_edges)))

  # Return as data frame
  data.frame(
    x = x_coords,
    y = y_coords,
    z = z_coords
  )
}
