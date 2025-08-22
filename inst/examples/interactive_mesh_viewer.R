# inst/examples/interactive_mesh_viewer.R
#' Interactive Mesh Visualization for eiEAM Package
#' 
#' This script generates interactive HTML visualizations comparing
#' original mesh with 4mm edge length remeshed version
#' 
#' @examples
#' # Run from R:
#' source(system.file("examples/interactive_mesh_viewer.R", package = "eiEAM"))

# Load required libraries
library(eiEAM)
library(rgl)
library(Rvcg)
library(plotly)
library(htmlwidgets)

cat("========================================\n")
cat("eiEAM Interactive Mesh Viewer\n")
cat("========================================\n\n")

# Set output directory
output_dir <- getwd()
cat("Output directory:", output_dir, "\n\n")

# Function to load mesh from file - corrected for your data structure
load_example_mesh <- function() {
  # Try to load from package data first
  mesh_file <- system.file("extdata/example_mesh.rds", package = "eiEAM")
  
  if (file.exists(mesh_file)) {
    cat("Loading mesh from package data...\n")
    
    tryCatch({
      mesh_data <- readRDS(mesh_file)
      
      # Validate the expected structure
      if (!is.list(mesh_data) || 
          !("vertices" %in% names(mesh_data)) || 
          !("faces" %in% names(mesh_data))) {
        stop("Mesh data does not have expected structure (vertices, faces)")
      }
      
      # Check data dimensions
      cat("Data structure validation:\n")
      cat("  Vertices matrix dimensions:", dim(mesh_data$vertices), "\n")
      cat("  Faces matrix dimensions:", dim(mesh_data$faces), "\n")
      
      # Ensure vertices is a matrix with 3 columns (x, y, z)
      vertices <- mesh_data$vertices
      if (!is.matrix(vertices) || ncol(vertices) != 3) {
        stop("Vertices must be a matrix with 3 columns")
      }
      
      # Ensure faces is a matrix with 3 columns (triangle indices)
      faces <- mesh_data$faces
      if (!is.matrix(faces) || ncol(faces) != 3) {
        stop("Faces must be a matrix with 3 columns")
      }
      
      # Create mesh3d object with correct data access
      mesh <- rgl::tmesh3d(
        vertices = t(vertices),  # vertices is already a matrix
        indices = t(faces),      # faces is already a matrix
        homogeneous = FALSE
      )
      
      cat(sprintf("Mesh loaded successfully: %d vertices, %d faces\n", 
                  ncol(mesh$vb), ncol(mesh$it)))
      
      return(mesh)
      
    }, error = function(e) {
      cat("Error loading mesh from package:", e$message, "\n")
      return(NULL)
    })
    
  } else {
    cat("Package mesh file not found at:", mesh_file, "\n")
  }
  
  # Try current directory
  local_files <- c("example_mesh.rds", "inst/extdata/example_mesh.rds")
  
  for (file in local_files) {
    if (file.exists(file)) {
      cat("Trying local file:", file, "\n")
      tryCatch({
        mesh_data <- readRDS(file)
        
        # Same validation and conversion as above
        vertices <- mesh_data$vertices
        faces <- mesh_data$faces
        
        mesh <- rgl::tmesh3d(
          vertices = t(vertices),
          indices = t(faces),
          homogeneous = FALSE
        )
        
        cat(sprintf("Mesh loaded from local file: %d vertices, %d faces\n", 
                    ncol(mesh$vb), ncol(mesh$it)))
        return(mesh)
        
      }, error = function(e) {
        cat("Failed to load", file, ":", e$message, "\n")
      })
    }
  }
  
  # If no file found, create a default mesh
  cat("No mesh file found. Creating default icosahedron mesh...\n")
  mesh <- rgl::icosahedron3d()
  mesh <- rgl::scale3d(mesh, 10, 10, 10)  # Scale to reasonable size
  
  cat(sprintf("Created default mesh: %d vertices, %d faces\n", 
              ncol(mesh$vb), ncol(mesh$it)))
  
  return(mesh)
}

# Function to create wireframe coordinates
create_wireframe_mesh <- function(mesh) {
  # Get edges from triangular faces
  edges <- rbind(
    cbind(t(mesh$vb[1:3, mesh$it[1, ]]), t(mesh$vb[1:3, mesh$it[2, ]])),
    cbind(t(mesh$vb[1:3, mesh$it[2, ]]), t(mesh$vb[1:3, mesh$it[3, ]])),
    cbind(t(mesh$vb[1:3, mesh$it[3, ]]), t(mesh$vb[1:3, mesh$it[1, ]]))
  )
  
  # Create line segments for wireframe
  wireframe_data <- data.frame(
    x = c(rbind(edges[, 1], edges[, 4], NA)),
    y = c(rbind(edges[, 2], edges[, 5], NA)),
    z = c(rbind(edges[, 3], edges[, 6], NA))
  )
  
  return(wireframe_data)
}

# Function to create interactive mesh plot
create_interactive_mesh <- function(mesh, 
                                    title = "3D Mesh", 
                                    mesh_color = "lightblue",
                                    wireframe_color = "black",
                                    opacity = 0.95,
                                    show_wireframe = TRUE,
                                    mesh_name = "Mesh",
                                    wireframe_name = "Wireframe") {
  
  # Create base mesh plot
  p <- plot_ly() %>%
    add_mesh(
      name = mesh_name,
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
        '<b>', mesh_name, '</b><br>',
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
        name = wireframe_name,
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
  
  # Configure layout
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
cat("\n--- Loading Mesh Data ---\n")
mesh <- load_example_mesh()

# Validate mesh was loaded successfully
if (is.null(mesh)) {
  stop("Failed to load mesh data")
}

# Create original mesh plot
cat("\n--- Creating Original Mesh Visualization ---\n")
p_original <- create_interactive_mesh(
  mesh, 
  title = "Original Mesh",
  mesh_color = "lightblue",
  opacity = 0.95,
  mesh_name = "Original",
  wireframe_name = "Original Wireframe"
)

# Perform remeshing with 4mm edge length
cat("\n--- Performing Isotropic Remeshing ---\n")
cat("Parameters:\n")
cat("  Target edge length: 4mm\n")
cat("  Iterations: 5\n")
cat("  Surface distance check: FALSE\n")
cat("  Adaptive: FALSE\n\n")

res_4mm <- eam_isotropic_remesh(
  mesh3d_obj = mesh,
  target_edge_length = 4,
  iterations = 5,
  surf_dist_check = FALSE,
  adaptive = FALSE
)

cat("\nRemeshing Results:\n")
cat(sprintf("  Original vertices: %d\n", res_4mm$original_vertex_count))
cat(sprintf("  Remeshed vertices: %d\n", res_4mm$roi_vertex_count))
cat(sprintf("  Remeshed faces: %d\n", res_4mm$face_count))
cat(sprintf("  Vertex ratio: %.2fx\n", res_4mm$roi_vertex_count / res_4mm$original_vertex_count))

# Create remeshed plot
cat("\n--- Creating Remeshed Mesh Visualization ---\n")
p_remeshed <- create_interactive_mesh(
  res_4mm$mesh3d_roi, 
  title = "Remeshed Mesh (4mm edge length)",
  mesh_color = "tomato",
  opacity = 0.95,
  mesh_name = "Remeshed",
  wireframe_name = "Remeshed Wireframe"
)

# Create comparison plot
cat("\n--- Creating Comparison Visualization ---\n")
comparison_plot <- subplot(
  p_original, p_remeshed,
  nrows = 1,
  shareX = TRUE,
  shareY = TRUE
) %>%
  layout(
    title = list(
      text = "eiEAM: Original vs Remeshed (4mm) Mesh Comparison",
      font = list(size = 18)
    ),
    showlegend = TRUE,
    scene = list(
#      domain = list(x = c(0, 0.5), y = c(0, 1)),
      camera = list(
        eye = list(x = -1.5, y = 0.2, z = 1.5),
        up = list(x = 0, y = 1, z = 0)
      )
    ),
    scene2 = list(
#      domain = list(x = c(0.5, 1), y = c(0, 1)),
      camera = list(
        eye = list(x = -1.5, y = 0.2, z = 1.5),
        up = list(x = 0, y = 1, z = 0)
      )
    )
  )

# Save visualizations
cat("\n--- Saving HTML Files ---\n")

output_files <- list(
  "eiEAM_original_mesh.html" = p_original,
  "eiEAM_remeshed_4mm.html" = p_remeshed,
  "eiEAM_mesh_comparison.html" = comparison_plot
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
  cat(sprintf("  ✓ %s\n", filename))
}

cat("\n========================================\n")
cat("Successfully created interactive plots!\n")
cat("========================================\n\n")
cat("Files saved in:", output_dir, "\n\n")
cat("Main file: eiEAM_mesh_comparison.html\n")
cat("\nVisualization features:\n")
cat("  • Rotate: Click and drag\n")
cat("  • Zoom: Mouse wheel\n")
cat("  • Pan: Right-click and drag\n")
cat("  • Toggle components: Click legend items\n")
cat("\nLegend components:\n")
cat("  • Original / Original Wireframe\n")
cat("  • Remeshed / Remeshed Wireframe\n")

# Open comparison in browser if interactive
if (interactive()) {
  cat("\nOpening comparison in browser...\n")
  browseURL(file.path(output_dir, "eiEAM_mesh_comparison.html"))
}