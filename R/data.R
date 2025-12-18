#' Sample Cardiac Mesh
#'
#' A sample left ventricular mesh from a cardiac mapping procedure,
#' useful for demonstrating mesh visualization and remeshing functions.
#'
#' @format A \code{mesh3d} object from the rgl package with:
#' \describe{
#'   \item{vb}{Vertex buffer matrix (4 x n_vertices), with rows x, y, z, w}
#'   \item{it}{Index matrix for triangular faces (3 x n_faces)}
#' }
#'
#' @source Electroanatomical mapping data, remeshed for demonstration.
#'
#' @examples
#' # View mesh dimensions
#' ncol(sample_mesh$vb)  
#' ncol(sample_mesh$it)
#'
#' \dontrun{
#' # Visualize with plotly
#' plot_meshes(list(sample = sample_mesh))
#' }
"sample_mesh"