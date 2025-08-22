# eiEAM Interactive Mesh Viewer - Shiny Application

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(plotly)
  library(DT)
  library(eiEAM)
  library(rgl)
  library(Rvcg)
})

# Function to load mesh from file
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
      cat("Data structure validation:\n") |> 
        cat("  Vertices matrix dimensions:", dim(mesh_data$vertices), "\n") |> 
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
  mesh <- rgl::icosahedron3d() |> 
    rgl::scale3d(10, 10, 10)  # Scale to reasonable size
  
  cat(sprintf("Created default mesh: %d vertices, %d faces\n", 
              ncol(mesh$vb), ncol(mesh$it)))
  
  return(mesh)
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "eiEAM Mesh Viewer",
    titleWidth = 250
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Mesh Viewer", tabName = "viewer", icon = icon("cube")),
      menuItem("Batch Processing", tabName = "batch", icon = icon("layer-group")),
      menuItem("Documentation", tabName = "docs", icon = icon("book")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    
    hr(),
    
    # Global controls
    h4("Visualization Settings", style = "padding-left: 15px;"),
    
    div(style = "padding: 0 15px;",
        checkboxInput("global_wireframe",
                      "Show Wireframe",
                      value = TRUE),
        
        sliderInput("global_opacity",
                    "Mesh Opacity:",
                    min = 0.1,
                    max = 1,
                    value = 0.9,
                    step = 0.1),
        
        selectInput("global_colorscheme",
                    "Color Scheme:",
                    choices = c("Default" = "default",
                                "Viridis" = "viridis",
                                "Heat" = "heat",
                                "Cool" = "cool"),
                    selected = "default")
    )
  ),
  
  dashboardBody(
    # Add custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 5px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .info-box {
          cursor: pointer;
        }
        .info-box:hover {
          box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        }
      "))
    ),
    
    tabItems(
      # Main Viewer Tab
      tabItem(tabName = "viewer",
              fluidRow(
                # Info boxes
                infoBox(
                  "Vertices",
                  textOutput("vertex_count"),
                  icon = icon("dot-circle"),
                  color = "blue",
                  width = 4
                ),
                infoBox(
                  "Faces",
                  textOutput("face_count"),
                  icon = icon("shapes"),
                  color = "green",
                  width = 4
                ),
                infoBox(
                  "Edge Length",
                  textOutput("edge_length_display"),
                  icon = icon("ruler"),
                  color = "yellow",
                  width = 4
                )
              ),
              
              fluidRow(
                # Control Panel
                box(
                  title = "Remeshing Controls",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  
                  h4("Parameters"),
                  sliderInput("edge_length",
                              "Target Edge Length (mm):",
                              min = 2,
                              max = 10,
                              value = 4,
                              step = 0.5),
                  
                  sliderInput("iterations",
                              "Iterations:",
                              min = 1,
                              max = 7,
                              value = 5,
                              step = 1),
                  
                  sliderInput("feature_angle",
                              "Feature Angle (degrees):",
                              min = 30,
                              max = 180,
                              value = 90,
                              step = 10),
                  
                  br(),
                  actionButton("remesh", 
                               "Perform Remeshing",
                               class = "btn-primary btn-block",
                               icon = icon("play")),
                  
                  br(),
                  downloadButton("download_mesh",
                                 "Download Remeshed Mesh",
                                 class = "btn-success btn-block")
                ),
                
                # Main visualization
                box(
                  title = "3D Mesh Visualization",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  
                  tabsetPanel(
                    id = "mesh_tabs",
                    tabPanel("Original",
                             plotlyOutput("original_plot", height = "500px")),
                    tabPanel("Remeshed",
                             plotlyOutput("remeshed_plot", height = "500px")),
                    tabPanel("Comparison",
                             plotlyOutput("comparison_plot", height = "500px")),
                    tabPanel("Statistics",
                             br(),
                             DT::dataTableOutput("stats_table"))
                  )
                )
              )
      ),
      
      # Batch Processing Tab
      tabItem(tabName = "batch",
              fluidRow(
                box(
                  title = "Batch Processing",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  
                  h4("Process Multiple Meshes"),
                  p("Configure parameters for batch processing multiple edge lengths:"),
                  
                  fluidRow(
                    column(4,
                           numericInput("batch_start",
                                        "Start Edge Length (mm):",
                                        value = 2,
                                        min = 2,
                                        max = 10)
                    ),
                    column(4,
                           numericInput("batch_end",
                                        "End Edge Length (mm):",
                                        value = 8,
                                        min = 2,
                                        max = 10)
                    ),
                    column(4,
                           numericInput("batch_step",
                                        "Step Size (mm):",
                                        value = 2,
                                        min = 0.5,
                                        max = 5)
                    )
                  ),
                  
                  actionButton("run_batch",
                               "Run Batch Processing",
                               class = "btn-warning",
                               icon = icon("cogs")),
                  
                  br(), br(),
                  
                  plotlyOutput("batch_comparison", height = "400px"),
                  
                  br(),
                  
                  DT::dataTableOutput("batch_results")
                )
              )
      ),
      
      # Documentation Tab
      tabItem(tabName = "docs",
              fluidRow(
                box(
                  title = "Documentation",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  
                  h3("How to Use the Mesh Viewer"),
                  
                  h4("1. Basic Operations"),
                  tags$ul(
                    tags$li("Rotate: Click and drag on the mesh"),
                    tags$li("Zoom: Use mouse wheel or pinch gesture"),
                    tags$li("Pan: Right-click and drag"),
                    tags$li("Reset view: Double-click on the plot")
                  ),
                  
                  h4("2. Remeshing Parameters"),
                  tags$dl(
                    tags$dt("Target Edge Length"),
                    tags$dd("Controls the desired edge length in the remeshed mesh (in millimeters)"),
                    
                    tags$dt("Iterations"),
                    tags$dd("Number of remeshing iterations to perform"),
                    
                    tags$dt("Feature Angle"),
                    tags$dd("Angle threshold for preserving sharp features")
                  ),
                  
                  h4("3. Batch Processing"),
                  p("Process multiple remeshing configurations at once to compare results."),
                  
                  h4("4. Export Options"),
                  p("Download remeshed meshes in various formats for use in other applications."),
                  
                  hr(),
                  
                  h4("Keyboard Shortcuts"),
                  tags$table(class = "table table-striped",
                             tags$tr(tags$th("Key"), tags$th("Action")),
                             tags$tr(tags$td("R"), tags$td("Reset camera view")),
                             tags$tr(tags$td("W"), tags$td("Toggle wireframe")),
                             tags$tr(tags$td("Space"), tags$td("Play/pause rotation"))
                  )
                )
              )
      ),
      
      # About Tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About eiEAM Mesh Viewer",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  
                  h3("Electroanatomical Mapping Post-Processing Tools"),
                  
                  p("This interactive application is part of the eiEAM package, providing 
              advanced mesh processing capabilities for electroanatomical mapping data."),
                  
                  h4("Features"),
                  tags$ul(
                    tags$li("Interactive 3D visualization using plotly"),
                    tags$li("Real-time mesh remeshing with Rvcg"),
                    tags$li("Multiple parameter configurations"),
                    tags$li("Batch processing capabilities"),
                    tags$li("Export functionality for processed meshes")
                  ),
                  
                  h4("Package Information"),
                  tags$ul(
                    tags$li(paste("eiEAM Version:", packageVersion("eiEAM"))),
                    tags$li("Author: Jonathan Salas"),
                    tags$li("License: MIT"),
                    tags$li(HTML("Repository: <a href='https://github.com/yourusername/eiEAM' target='_blank'>GitHub</a>"))
                  ),
                  
                  h4("Citation"),
                  tags$pre(
                    "Salas J (2025). eiEAM: Electroanatomical Mapping Post-Processing Tools.
R package version 0.1."
                  ),
                  
                  h4("Acknowledgments"),
                  p("This package uses the following key dependencies:"),
                  tags$ul(
                    tags$li("rgl - 3D visualization in R"),
                    tags$li("Rvcg - Interface to VCGLIB"),
                    tags$li("plotly - Interactive web graphics"),
                    tags$li("shiny - Web application framework")
                  )
                )
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    original_mesh = NULL,
    remeshed_mesh = NULL,
    batch_results = NULL,
    current_stats = data.frame(
      Metric = c("Original Vertices", "Original Faces", "Remeshed Vertices", "Remeshed Faces"),
      Count = c(0, 0, 0, 0)
    )
  )
  
  # Initialize with example mesh
  observe({
    mesh <- load_example_mesh()
    
    if (!is.null(mesh)) {
      values$original_mesh <- mesh
      
      # Update initial stats
      values$current_stats <- data.frame(
        Metric = c("Original Vertices", "Original Faces", "Remeshed Vertices", "Remeshed Faces"),
        Count = c(ncol(values$original_mesh$vb), 
                  ncol(values$original_mesh$it),
                  0, 0)
      )
    } else {
      showNotification("Failed to load example mesh", type = "error", duration = 5)
    }
  })
  
  # Helper function to get color based on scheme
  get_mesh_color <- function(scheme, type = "primary") {
    colors <- switch(scheme,
                     "default" = c("lightblue", "tomato"),
                     "viridis" = c("#440154", "#31688e"),
                     "heat" = c("#ffeda0", "#f03b20"),
                     "cool" = c("#d0d1e6", "#016c59"),
                     c("lightblue", "tomato")
    )
    return(colors[ifelse(type == "primary", 1, 2)])
  }
  
  # Function to create an empty plot
  create_empty_plot <- function(title) {
    plot_ly() |> 
      add_trace(
        x = numeric(0),
        y = numeric(0),
        z = numeric(0),
        type = "scatter3d",
        mode = "none",
        showlegend = FALSE
      ) |> 
      layout(
        title = list(text = title, x = 0.5, xanchor = "center"),
        scene = list(
          xaxis = list(title = "X (mm)", showgrid = FALSE, zeroline = FALSE, visible = FALSE),
          yaxis = list(title = "Y (mm)", showgrid = FALSE, zeroline = FALSE, visible = FALSE),
          zaxis = list(title = "Z (mm)", showgrid = FALSE, zeroline = FALSE, visible = FALSE),
          camera = list(
            eye = list(x = -1.5, y = 0.2, z = 1.5),
            up = list(x = 0, y = 1, z = 0)
          ),
          aspectmode = "cube"
        ),
        showlegend = FALSE
      )
  }
  
  # Function to create mesh plot
  create_mesh_plot <- function(mesh, title, color = NULL) {
    if (is.null(mesh)) return(create_empty_plot(title))
    
    if (is.null(color)) {
      color <- get_mesh_color(input$global_colorscheme, "primary")
    }
    
    p <- plot_ly() |> 
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
        opacity = input$global_opacity,
        color = I(color),
        hovertemplate = paste(
          '<b>', title, '</b><br>',
          'X: %{x:.2f}<br>',
          'Y: %{y:.2f}<br>',
          'Z: %{z:.2f}<br>',
          '<extra></extra>'
        )
      )
    
    if (input$global_wireframe) {
      wireframe_coords <- eiEAM::create_wireframe_mesh(mesh)
      p <- p |> 
        add_trace(
          name = "Wireframe",
          data = wireframe_coords,
          x = ~x, y = ~y, z = ~z,
          type = "scatter3d",
          mode = "lines",
          line = list(color = "black", width = 1),
          showlegend = FALSE,
          hoverinfo = "skip"
        )
    }
    
    p |> 
      layout(
        title = list(text = title, x = 0.5, xanchor = "center"),
        showlegend = TRUE,
        scene = list(
          camera = list(
            eye = list(x = -1.5, y = 0.2, z = 1.5),
            up = list(x = 0, y = 1, z = 0)
          ),
          xaxis = list(title = "X (mm)"),
          yaxis = list(title = "Y (mm)"),
          zaxis = list(title = "Z (mm)"),
          aspectmode = "cube"
        )
      )
  }
  
  # Perform remeshing
  observeEvent(input$remesh, {
    req(values$original_mesh)
    
    withProgress(message = 'Performing remeshing...', value = 0, {
      incProgress(0.3, detail = "Processing mesh...")
      
      tryCatch({
        res <- eam_isotropic_remesh(
          mesh3d_obj = values$original_mesh,
          target_edge_length = input$edge_length,
          iterations = input$iterations,
          feature_angle_deg = input$feature_angle
        )
        
        incProgress(0.6, detail = "Finalizing...")
        
        # Verify remeshing output
        if (is.null(res$mesh3d_roi) || is.null(res$roi_vertex_count) || is.null(res$face_count)) {
          stop("Remeshing output is invalid or incomplete")
        }
        
        values$remeshed_mesh <- res$mesh3d_roi
        
        # Update stats
        values$current_stats <- data.frame(
          Metric = c("Original Vertices", "Original Faces", 
                     "Remeshed Vertices", "Remeshed Faces",
                     "Vertex Reduction", "Face Change"),
          Count = c(res$original_vertex_count, 
                    ncol(values$original_mesh$it),
                    res$roi_vertex_count, 
                    res$face_count,
                    paste0(round((1 - res$roi_vertex_count/res$original_vertex_count) * 100, 1), "%"),
                    paste0(round((res$face_count/ncol(values$original_mesh$it) - 1) * 100, 1), "%"))
        )
        
        incProgress(0.3, detail = "Complete!")
        
        showNotification("Remeshing completed successfully!", 
                         type = "message", 
                         duration = 3)
        
        # Switch to remeshed tab
        updateTabsetPanel(session, "mesh_tabs", selected = "Remeshed")
        
      }, error = function(e) {
        # Log the error for debugging
        cat("Remeshing error:", e$message, "\n")
        showNotification(paste("Remeshing failed:", e$message), 
                         type = "error", 
                         duration = 5)
      })
    })
  })
  
  # Batch processing
  observeEvent(input$run_batch, {
    req(values$original_mesh)
    
    edge_lengths <- seq(input$batch_start, input$batch_end, by = input$batch_step)
    n_configs <- length(edge_lengths)
    
    results <- list()
    
    withProgress(message = 'Running batch processing...', value = 0, {
      for (i in seq_along(edge_lengths)) {
        incProgress(1/n_configs, detail = paste("Edge length:", edge_lengths[i], "mm"))
        
        tryCatch({
          res <- eam_isotropic_remesh(
            mesh3d_obj = values$original_mesh,
            target_edge_length = edge_lengths[i],
            iterations = input$iterations,
            feature_angle_deg = input$feature_angle
          )
          
          results[[i]] <- data.frame(
            EdgeLength = edge_lengths[i],
            Vertices = res$roi_vertex_count,
            Faces = res$face_count,
            VertexRatio = round(res$roi_vertex_count / res$original_vertex_count, 3),
            FaceRatio = round(res$face_count / ncol(values$original_mesh$it), 3)
          )
        }, error = function(e) {
          cat("Batch remeshing error for edge length", edge_lengths[i], ":", e$message, "\n")
          results[[i]] <- data.frame(
            EdgeLength = edge_lengths[i],
            Vertices = NA,
            Faces = NA,
            VertexRatio = NA,
            FaceRatio = NA
          )
        })
      }
    })
    
    values$batch_results <- do.call(rbind, results)
    
    showNotification("Batch processing completed!", 
                     type = "message", 
                     duration = 3)
  })
  
  # Output: Info boxes
  output$vertex_count <- renderText({
    if (!is.null(values$remeshed_mesh)) {
      paste(ncol(values$remeshed_mesh$vb))
    } else {
      paste(ncol(values$original_mesh$vb))
    }
  })
  
  output$face_count <- renderText({
    if (!is.null(values$remeshed_mesh)) {
      paste(ncol(values$remeshed_mesh$it))
    } else if (!is.null(values$original_mesh)) {
      paste(ncol(values$original_mesh$it))
    } else {
      "0"
    }
  })
  
  output$edge_length_display <- renderText({
    paste(input$edge_length, "mm")
  })
  
  # Output: Plots
  output$original_plot <- renderPlotly({
    req(values$original_mesh)
    create_mesh_plot(values$original_mesh, "Original Mesh")
  })
  
  output$remeshed_plot <- renderPlotly({
    if (is.null(values$remeshed_mesh)) {
      create_empty_plot("Click 'Perform Remeshing' to generate")
    } else {
      create_mesh_plot(values$remeshed_mesh, 
                       paste("Remeshed (", input$edge_length, "mm)"),
                       get_mesh_color(input$global_colorscheme, "secondary"))
    }
  })
  
  output$comparison_plot <- renderPlotly({
    if (is.null(values$remeshed_mesh)) {
      create_empty_plot("Remeshing required for comparison")
    } else {
      p1 <- create_mesh_plot(values$original_mesh, "Original")
      p2 <- create_mesh_plot(values$remeshed_mesh, "Remeshed",
                             get_mesh_color(input$global_colorscheme, "secondary"))
      
      subplot(p1, p2, nrows = 1, shareX = TRUE, shareY = TRUE) |> 
        layout(title = list(text = "Mesh Comparison", x = 0.5, xanchor = "center"))
    }
  })
  
  output$batch_comparison <- renderPlotly({
    if (is.null(values$batch_results)) {
      create_empty_plot("Run batch processing to see results")
    } else {
      plot_ly(values$batch_results) |> 
        add_trace(x = ~EdgeLength, y = ~Vertices, 
                  type = 'scatter', mode = 'lines+markers',
                  name = 'Vertices', yaxis = 'y') |> 
        add_trace(x = ~EdgeLength, y = ~Faces, 
                  type = 'scatter', mode = 'lines+markers',
                  name = 'Faces', yaxis = 'y2') |> 
        layout(
          title = list(text = "Batch Processing Results", x = 0.5, xanchor = "center"),
          xaxis = list(title = "Edge Length (mm)"),
          yaxis = list(title = "Vertex Count"),
          yaxis2 = list(
            title = "Face Count",
            overlaying = "y",
            side = "right"
          ),
          hovermode = 'x unified'
        )
    }
  })
  
  # Output: Tables
  output$stats_table <- DT::renderDataTable({
    DT::datatable(values$current_stats,
                  options = list(
                    dom = 't',
                    paging = FALSE,
                    searching = FALSE
                  ),
                  rownames = FALSE)
  })
  
  output$batch_results <- DT::renderDataTable({
    if (!is.null(values$batch_results)) {
      DT::datatable(values$batch_results,
                    options = list(
                      pageLength = 10,
                      dom = 'Bfrtip'
                    ),
                    rownames = FALSE) |> 
        DT::formatRound(columns = c('VertexRatio', 'FaceRatio'), digits = 3)
    }
  })
  
  # Download handler
  output$download_mesh <- downloadHandler(
    filename = function() {
      paste0("remeshed_mesh_", input$edge_length, "mm_", Sys.Date(), ".rds")
    },
    content = function(file) {
      if (!is.null(values$remeshed_mesh)) {
        saveRDS(values$remeshed_mesh, file)
      }
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)