#' Plot multiple cardiac meshes with LAT, CV, and optional point cloud data
#'
#' @param mesh_list Named list of \code{mesh3d} objects.
#' @param show_wireframe Logical; overlay wireframe if TRUE. Default TRUE.
#' @param opacity Mesh opacity. Default 0.9.
#' @param lat_data Named list of data.frames, each with column \code{lat}
#'   (length = n vertices of corresponding mesh).
#' @param cv_data Named list of data.frames, each with columns \code{u, v, w}
#'   (length = n vertices of corresponding mesh). If \code{x,y,z} are not
#'   supplied, vertex coords are retrieved from the mesh.
#' @param pointcloud_data Named list of data.frames, each with \code{x,y,z}.
#'
#' @return A plotly 3D figure object with all meshes overlaid in a single scene.
#'
#' @examples
#' \dontrun{
#' # Example: base mesh with wireframe
#' plot_meshes(list(example = sample_mesh))
#'
#' # Example with LAT coloring
#' df_lat <- list(example = data.frame(lat = rnorm(ncol(sample_mesh$vb))))
#' plot_meshes(list(example = sample_mesh), lat_data = df_lat)
#'
#' # Example with cone vectors
#' df_cv <- list(example = data.frame(
#'   u = rnorm(ncol(sample_mesh$vb)),
#'   v = rnorm(ncol(sample_mesh$vb)),
#'   w = rnorm(ncol(sample_mesh$vb))
#' ))
#' plot_meshes(list(example = sample_mesh), cv_data = df_cv)
#' }
#'
#' @export
plot_meshes <- function(mesh_list,
                        show_wireframe = TRUE,
                        opacity = 0.9,
                        lat_data = NULL,
                        cv_data = NULL,
                        pointcloud_data = NULL) {
  
  
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required but not installed.", call. = FALSE)
  }
  
  stopifnot(is.list(mesh_list))
  
  # Default color palette for multiple meshes
  
  mesh_colors <- c(
    "#9467bd", "#1f77b4", "#2ca02c", "#d62728",
    "#ff7f0e", "#8c564b", "#e377c2", "#7f7f7f"
  )
  
  # Start with empty plotly object
  p <- plotly::plot_ly()
  
  # Iterate over meshes and add traces
  mesh_names <- names(mesh_list)
  for (i in seq_along(mesh_list)) {
    name <- mesh_names[i]
    mesh <- mesh_list[[i]]
    color <- mesh_colors[((i - 1L) %% length(mesh_colors)) + 1L]
    
    verts <- mesh$vb[1:3, ]
    faces <- mesh$it - 1
    has_faces <- !is.null(faces) && length(faces) > 0
    
    # LAT values for this mesh
    lat_vals <- if (!is.null(lat_data) && name %in% names(lat_data)) {
      stopifnot(nrow(lat_data[[name]]) == ncol(verts))
      lat_data[[name]]$lat
    } else {
      NULL
    }
    
    # Add mesh surface if faces exist
    if (has_faces) {
      p <- p |>
        plotly::add_trace(
          x = verts[1, ],
          y = verts[2, ],
          z = verts[3, ],
          i = faces[1, ],
          j = faces[2, ],
          k = faces[3, ],
          type = "mesh3d",
          intensity = lat_vals,
          colorscale = if (!is.null(lat_vals)) "Viridis" else NULL,
          facecolor = if (is.null(lat_vals)) rep(color, ncol(faces)) else NULL,
          opacity = opacity,
          name = name,
          showscale = !is.null(lat_vals)
        )
      
      # Add wireframe overlay
      if (show_wireframe) {
        wireframe <- create_wireframe_mesh(mesh)
        p <- p |>
          plotly::add_trace(
            data = wireframe,
            x = ~x, y = ~y, z = ~z,
            type = "scatter3d",
            mode = "lines",
            line = list(color = "black", width = 1),
            inherit = FALSE,
            showlegend = FALSE,
            name = paste(name, "wireframe"),
            hoverinfo = "skip"
          )
      }
    }
    
    # Add pointcloud-only mesh (no faces) or overlay pointcloud
    if (!has_faces && !is.null(pointcloud_data) && name %in% names(pointcloud_data)) {
      p <- p |>
        plotly::add_trace(
          data = pointcloud_data[[name]],
          x = ~x, y = ~y, z = ~z,
          type = "scatter3d",
          mode = "markers",
          marker = list(size = 2, color = "red"),
          name = name
        )
    } else if (!is.null(pointcloud_data) && name %in% names(pointcloud_data)) {
      # Overlay pointcloud on mesh
      p <- p |>
        plotly::add_trace(
          data = pointcloud_data[[name]],
          x = ~x, y = ~y, z = ~z,
          type = "scatter3d",
          mode = "markers",
          marker = list(size = 2, color = "red", opacity = 0.6),
          name = paste(name, "pointcloud")
        )
    }
    
    # Add CV (conduction velocity) cone vectors
    if (!is.null(cv_data) && name %in% names(cv_data)) {
      df_cv <- cv_data[[name]]
      if (!all(c("u", "v", "w") %in% names(df_cv))) {
        stop("cv_data must contain u, v, w columns", call. = FALSE)
      }
      
      # Add vertex coordinates if not present
      if (!all(c("x", "y", "z") %in% names(df_cv))) {
        df_cv <- cbind(
          data.frame(
            x = verts[1, ],
            y = verts[2, ],
            z = verts[3, ]
          ),
          df_cv
        )
      }
      
      p <- p |>
        plotly::add_trace(
          data = df_cv,
          x = ~x, y = ~y, z = ~z,
          u = ~u, v = ~v, w = ~w,
          type = "cone",
          sizemode = "absolute",
          anchor = "tail",
          showscale = FALSE,
          colorscale = "Viridis",
          name = paste(name, "cv")
        )
    }
  }
  
  # Configure 3D scene layout
  p |>
    plotly::layout(
      scene = list(
        aspectmode = "data",
        xaxis = list(title = "X"),
        yaxis = list(title = "Y"),
        zaxis = list(title = "Z")
      )
    )
}