# inst/examples/interactive_mesh_viewer.R
#' Interactive Mesh Visualization for eiEAM Package
#' 
#' This script generates interactive HTML visualizations of mesh remeshing
#' Run this script to create three HTML files that can be opened in any browser
#' 
#' @examples
#' # Run from R:
#' source(system.file("examples/interactive_mesh_viewer.R", package = "eiEAM"))

# Check and install required packages
required_packages <- c("eiEAM", "rgl", "Rvcg", "plotly", "htmlwidgets")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
}

# Load libraries
library(eiEAM)
library(rgl)
library(Rvcg)
library(plotly)
library(htmlwidgets)

cat("========================================\n")
cat("eiEAM Interactive Mesh Viewer\n")
cat("========================================\n\n")

# Set output directory (user's working directory)
output_dir <- getwd()
cat("Output directory:", output_dir, "\n\n")

# Create example mesh data (same as in your README)
create_example_mesh <- function() {
  mesh_obj <- list(
    faces = structure(c(1L, 1L, 4L, 8L, 11L, 12L, 17L, 6L, 19L, 
                        13L, 3L, 5L, 22L, 1L, 26L, 18L, 18L, 8L, 21L, 14L, 27L, 4L, 14L, 
                        7L, 6L, 29L, 22L, 16L, 23L, 26L, 3L, 28L, 2L, 13L, 12L, 31L, 
                        33L, 35L, 32L, 28L, 33L, 31L, 20L, 9L, 36L, 12L, 29L, 20L, 31L, 
                        27L, 17L, 26L, 8L, 15L, 9L, 29L, 9L, 35L, 15L, 11L, 16L, 16L, 
                        38L, 31L, 37L, 32L, 24L, 35L, 23L, 23L, 23L, 37L, 10L, 35L, 2L, 
                        4L, 5L, 7L, 10L, 13L, 16L, 18L, 20L, 2L, 2L, 7L, 3L, 3L, 25L, 
                        17L, 27L, 17L, 7L, 2L, 18L, 27L, 28L, 5L, 7L, 1L, 30L, 29L, 30L, 
                        31L, 13L, 5L, 5L, 12L, 14L, 33L, 10L, 25L, 25L, 20L, 12L, 12L, 
                        28L, 36L, 15L, 19L, 4L, 7L, 37L, 4L, 27L, 13L, 36L, 36L, 7L, 
                        16L, 10L, 32L, 16L, 15L, 23L, 30L, 16L, 34L, 34L, 37L, 35L, 24L, 
                        13L, 24L, 26L, 11L, 11L, 38L, 3L, 2L, 2L, 6L, 9L, 14L, 15L, 8L, 
                        9L, 14L, 13L, 21L, 23L, 22L, 24L, 8L, 17L, 15L, 20L, 28L, 6L, 
                        6L, 19L, 4L, 4L, 22L, 16L, 22L, 22L, 32L, 23L, 21L, 28L, 31L, 
                        19L, 34L, 34L, 32L, 26L, 19L, 9L, 33L, 21L, 11L, 11L, 9L, 1L, 
                        9L, 32L, 29L, 29L, 31L, 9L, 8L, 8L, 17L, 33L, 38L, 38L, 38L, 
                        39L, 23L, 39L, 37L, 10L, 38L, 39L, 25L, 26L, 39L, 24L, 38L, 37L, 
                        39L), 
                      dim = c(74L, 3L), 
                      dimnames = list(NULL, c("V1", "V2", "V3"))),
    vertices = structure(c(1.919, 11.144, 5.425, -1.663, 7.442, 
                           -6.695, -0.033, -4.232, 21.154, 28.656, 26.039, 24.7, 38.495, 
                           22.12, -2.174, 0.293, -11.404, -10.334, 17.513, 11.779, 9.429, 
                           -1.229, 23.178, 31.624, 38.598, 45.126, -11.158, 12.006, -2.916, 
                           0.309, 30.989, 42.613, 23.712, 26.903, 30.351, 7.967, 31.164, 
                           31.153, 25.446, -61.424, -56.548, -45.974, -66.275, -66.617, 
                           -84.518, -78.743, -92.504, -86.822, -90.445, -100.261, -64.495, 
                           -54.848, -57.949, -109.207, -76.79, -95.804, -88.342, -64.251, 
                           -69.068, -68.236, -69.708, -76.694, -76.476, -78.494, -64.288, 
                           -86.863, -61.31, -68.854, -74.483, -79.064, -81.495, -86.376, 
                           -87.745, -83.035, -109.459, -90.442, -90.17, -80.83, 189.26, 
                           207.377, 187.389, 200.772, 215.514, 214.222, 217.531, 215.624, 
                           211.211, 205.351, 203.87, 210.062, 195.598, 210.308, 195.291, 
                           177.159, 194.172, 210.24, 213.33, 216.294, 216.293, 180.885, 
                           163.472, 163.423, 168.801, 186.712, 207.276, 212.506, 190.655, 
                           176.78, 204.541, 191.066, 209.147, 206.617, 172.962, 211.961, 
                           203.224, 179.055, 170.082), 
                         dim = c(39L, 3L))
  )
  
  rgl::tmesh3d(
    vertices = t(as.matrix(mesh_obj$vertices[, 1:3])),
    indices = t(as.matrix(mesh_obj$faces[, 1:3])),
    homogeneous = FALSE
  )
}

# Function to create interactive mesh plot with enhanced features
create_interactive_mesh <- function(mesh, 
                                    title = "3D Mesh", 
                                    mesh_color = "lightblue",
                                    wireframe_color = "black",
                                    opacity = 0.7,
                                    show_wireframe = TRUE) {
  
  # Create base mesh plot
  p <- plot_ly() %>%
    add_mesh(
      name = "Mesh Surface",
      x = t(mesh$vb[1:3, ])[, 1],
      y = t(mesh$vb[1:3, ])[, 2],
      z = t(mesh$vb[1:3, ])[, 3],
      i = t(mesh$it)[, 1] - 1,
      j = t(mesh$it)[, 2] - 1,
      k = t(mesh$it)[, 3] - 1,
      flatshading = TRUE,
      showlegend = TRUE,
      type = "mesh3d",
      opacity = opacity,
      color = I(mesh_color),
      hovertemplate = paste(
        '<b>Mesh Surface</b><br>',
        'X: %{x:.2f}<br>',
        'Y: %{y:.2f}<br>',
        'Z: %{z:.2f}<br>',
        '<extra></extra>'
      )
    )
  
  # Add wireframe if requested
  if (show_wireframe) {
    wireframe_coords <- create_wireframe_mesh(mesh)
    p <- p %>%
      add_trace(
        name = "Wireframe",
        data = wireframe_coords,
        x = ~ x,
        y = ~ y,
        z = ~ z,
        type = "scatter3d",
        mode = "lines",
        line = list(color = wireframe_color, width = 2),
        showlegend = TRUE,
        hoverinfo = "skip"
      )
  }
  
  # Configure layout with enhanced interactivity
  p <- p %>%
    layout(
      title = list(
        text = title,
        font = list(size = 20)
      ),
      showlegend = TRUE,
      scene = list(
        camera = list(
          eye = list(x = -1.5, y = 0.2, z = 1.5),
          up  = list(x = 0, y = 1, z = 0),
          center = list(x = 0, y = 0, z = 0)
        ),
        xaxis = list(
          title = "X (mm)",
          gridcolor = "lightgray",
          showbackground = TRUE,
          backgroundcolor = "rgb(245, 245, 245)"
        ),
        yaxis = list(
          title = "Y (mm)",
          gridcolor = "lightgray",
          showbackground = TRUE,
          backgroundcolor = "rgb(245, 245, 245)"
        ),
        zaxis = list(
          title = "Z (mm)",
          gridcolor = "lightgray",
          showbackground = TRUE,
          backgroundcolor = "rgb(245, 245, 245)"
        ),
        aspectmode = "cube"
      ),
      paper_bgcolor = "white",
      plot_bgcolor = "rgba(245, 245, 245, 0.5)",
      margin = list(l = 0, r = 0, b = 0, t = 50)
    )
  
  return(p)
}

# Main execution
cat("Creating example mesh...\n")
mesh <- create_example_mesh()

# Create original mesh plot
cat("Generating original mesh visualization...\n")
p1 <- create_interactive_mesh(
  mesh, 
  title = "Original Mesh",
  mesh_color = "lightblue",
  opacity = 0.7
)

# Perform remeshing with different parameters
cat("\nPerforming isotropic remeshing...\n")
cat("Parameters:\n")
cat("  - Target edge length: 4mm\n")
cat("  - Iterations: 5\n")
cat("  - Surface distance check: FALSE\n")
cat("  - Adaptive: FALSE\n\n")

res_4mm <- eam_isotropic_remesh(
  mesh3d_obj = mesh,
  target_edge_length = 4,
  iterations = 5,
  surf_dist_check = FALSE,
  adaptive = FALSE
)

cat(sprintf("Results (4mm edge length):\n"))
cat(sprintf("  Original vertices: %d\n", res_4mm$original_vertex_count))
cat(sprintf("  ROI vertices: %d\n", res_4mm$roi_vertex_count))
cat(sprintf("  ROI faces: %d\n\n", res_4mm$face_count))

# Create remeshed plot
p2 <- create_interactive_mesh(
  res_4mm$mesh3d_roi, 
  title = "Remeshed (4mm edge length)",
  mesh_color = "tomato",
  opacity = 0.7
)

# Perform another remeshing with finer resolution
cat("Performing fine remeshing (2mm edge length)...\n")
res_2mm <- eam_isotropic_remesh(
  mesh3d_obj = mesh,
  target_edge_length = 2,
  iterations = 5,
  surf_dist_check = FALSE,
  adaptive = FALSE
)

cat(sprintf("Results (2mm edge length):\n"))
cat(sprintf("  Original vertices: %d\n", res_2mm$original_vertex_count))
cat(sprintf("  ROI vertices: %d\n", res_2mm$roi_vertex_count))
cat(sprintf("  ROI faces: %d\n\n", res_2mm$face_count))

p3 <- create_interactive_mesh(
  res_2mm$mesh3d_roi, 
  title = "Remeshed (2mm edge length)",
  mesh_color = "lightgreen",
  opacity = 0.7
)

# Create comparison plots
cat("Creating comparison visualizations...\n")

# Side-by-side comparison
comparison_plot <- subplot(
  p1, p2,
  nrows = 1,
  shareX = TRUE,
  shareY = TRUE,
  titleX = TRUE,
  titleY = TRUE
) %>%
  layout(
    title = list(
      text = "eiEAM: Original vs Remeshed (4mm) Mesh Comparison",
      font = list(size = 18)
    ),
    showlegend = TRUE
  )

# Three-way comparison
three_way_comparison <- subplot(
  p1, p2, p3,
  nrows = 1,
  shareX = TRUE,
  shareY = TRUE,
  titleX = TRUE,
  titleY = TRUE,
  widths = c(0.33, 0.33, 0.34)
) %>%
  layout(
    title = list(
      text = "eiEAM: Multi-Resolution Mesh Comparison",
      font = list(size = 18)
    ),
    showlegend = FALSE
  )

# Save all visualizations
cat("\nSaving interactive HTML files...\n")

# Save individual plots
output_files <- list(
  "eiEAM_original_mesh.html" = p1,
  "eiEAM_remeshed_4mm.html" = p2,
  "eiEAM_remeshed_2mm.html" = p3,
  "eiEAM_mesh_comparison.html" = comparison_plot,
  "eiEAM_three_way_comparison.html" = three_way_comparison
)

for (filename in names(output_files)) {
  filepath <- file.path(output_dir, filename)
  htmlwidgets::saveWidget(
    output_files[[filename]], 
    filepath,
    selfcontained = TRUE,
    libdir = NULL,
    title = gsub("_", " ", gsub("\\.html", "", filename))
  )
  cat(sprintf("  ✓ Saved: %s\n", filename))
}

cat("\n========================================\n")
cat("Interactive plots successfully created!\n")
cat("========================================\n\n")
cat("Files saved in:", output_dir, "\n\n")
cat("To view the visualizations:\n")
cat("1. Open any of the HTML files in your web browser\n")
cat("2. Use mouse to rotate, zoom, and pan the 3D meshes\n")
cat("3. Click legend items to show/hide mesh components\n\n")
cat("Files created:\n")
for (filename in names(output_files)) {
  cat(sprintf("  - %s\n", filename))
}

# Optional: Open in browser automatically
if (interactive()) {
  cat("\nOpening comparison in browser...\n")
  browseURL(file.path(output_dir, "eiEAM_mesh_comparison.html"))
}