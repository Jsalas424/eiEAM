# R/launch_mesh_viewer.R

#' Launch eiEAM Interactive Mesh Viewer
#'
#' @description
#' Launches the Shiny application for interactive mesh visualization and remeshing.
#' This provides a web-based interface for exploring mesh processing capabilities.
#'
#' @param launch.browser Logical. Should the app be launched in a browser? Default is TRUE.
#' @param port Integer. The port to run the app on. Default is NULL (auto-select).
#' @param host String. The host to run the app on. Default is "127.0.0.1".
#'
#' @return Launches the Shiny application (no return value).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch the mesh viewer
#' launch_mesh_viewer()
#' 
#' # Launch on a specific port
#' launch_mesh_viewer(port = 3838)
#' 
#' # Launch without opening browser
#' launch_mesh_viewer(launch.browser = FALSE)
#' }
launch_mesh_viewer <- function(launch.browser = TRUE, 
                               port = NULL, 
                               host = "127.0.0.1") {
  
  # Check for required packages
  required_packages <- c("shiny", "shinydashboard", "plotly", "DT")
  missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  
  if(length(missing_packages) > 0) {
    stop(paste("Please install missing packages:", 
               paste(missing_packages, collapse = ", "),
               "\nRun: install.packages(c('", 
               paste(missing_packages, collapse = "', '"), "'))"))
  }
  
  # Find the app directory
  app_dir <- system.file("shiny/mesh_viewer", package = "eiEAM")
  
  if (app_dir == "") {
    stop("Could not find the mesh_viewer app. Please reinstall the eiEAM package.")
  }
  
  # Launch the app
  shiny::runApp(app_dir, 
                launch.browser = launch.browser,
                port = port,
                host = host)
}

#' Generate Interactive Mesh HTML Files
#'
#' @description
#' Generates standalone HTML files with interactive 3D mesh visualizations
#' that can be opened in any web browser.
#'
#' @param output_dir Directory where HTML files will be saved. Default is current working directory.
#' @param edge_lengths Numeric vector of edge lengths to process. Default is c(2, 4, 6).
#' @param open_browser Logical. Should the comparison file be opened in browser? Default is TRUE.
#'
#' @return Invisible NULL. Creates HTML files as side effect.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate default visualizations
#' generate_mesh_html()
#' 
#' # Generate with custom edge lengths
#' generate_mesh_html(edge_lengths = c(1, 3, 5, 7))
#' 
#' # Save to specific directory
#' generate_mesh_html(output_dir = "~/Desktop/mesh_visualizations")
#' }
generate_mesh_html <- function(output_dir = getwd(), 
                               edge_lengths = c(2, 4, 6),
                               open_browser = TRUE) {
  
  # Check for required packages
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Package 'htmlwidgets' is required. Install with: install.packages('htmlwidgets')")
  }
  
  # Source the interactive viewer script
  script_path <- system.file("examples/interactive_mesh_viewer.R", package = "eiEAM")
  
  if (script_path == "") {
    stop("Could not find interactive_mesh_viewer.R. Please reinstall the eiEAM package.")
  }
  
  # Set output directory temporarily
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(output_dir)
  
  # Source the script
  source(script_path, local = TRUE)
  
  if (open_browser && interactive()) {
    browseURL(file.path(output_dir, "eiEAM_mesh_comparison.html"))
  }
  
  invisible(NULL)
}