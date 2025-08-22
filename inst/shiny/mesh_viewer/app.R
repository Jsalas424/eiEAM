# inst/shiny/mesh_viewer/app.R
# eiEAM Interactive Mesh Viewer - Shiny Application

# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(eiEAM)
library(rgl)
library(Rvcg)

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
                    value = 0.7,
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
                              min = 1,
                              max = 10,
                              value = 4,
                              step = 0.5),
                  
                  sliderInput("iterations",
                              "Iterations:",
                              min = 1,
                              max = 10,
                              value = 5,
                              step = 1),
                  
                  sliderInput("feature_angle",
                              "Feature Angle (degrees):",
                              min = 30,
                              max = 180,
                              value = 90,
                              step = 10),
                  
                  h4("Advanced Options"),
                  checkboxInput("surf_dist_check",
                                "Surface Distance Check",
                                value = FALSE),
                  
                  checkboxInput("adaptive",
                                "Adaptive Remeshing",
                                value = FALSE),
                  
                  conditionalPanel(
                    condition = "input.adaptive == true",
                    sliderInput("max_surf_dist",
                                "Max Surface Distance:",
                                min = 0.5,
                                max = 5,
                                value = 2,
                                step = 0.5)
                  ),
                  
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
                                        min = 1,
                                        max = 10)
                    ),
                    column(4,
                           numericInput("batch_end",
                                        "End Edge Length (mm):",
                                        value = 8,
                                        min = 1,
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
                    tags$dd("Angle threshold for preserving sharp features"),
                    
                    tags$dt("Surface Distance Check"),
                    tags$dd("When enabled, maintains higher fidelity to the original mesh"),
                    
                    tags$dt("Adaptive Remeshing"),
                    tags$dd("Generates different triangle sizes based on mesh curvature")
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
                          39L), dim = c(74L, 3L)),
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
                             203.224, 179.055, 170.082), dim = c(39L, 3L))
    )
    
    values$original_mesh <- rgl::tmesh3d(
      vertices = t(as.matrix(mesh_obj$vertices[, 1:3])),
      indices = t(as.matrix(mesh_obj$faces[, 1:3])),
      homogeneous = FALSE
    )
    
    # Update initial stats
    values$current_stats <- data.frame(
      Metric = c("Original Vertices", "Original Faces", "Remeshed Vertices", "Remeshed Faces"),
      Count = c(ncol(values$original_mesh$vb), 
                ncol(values$original_mesh$it),
                0, 0)
    )
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
  
  # Function to create mesh plot
  create_mesh_plot <- function(mesh, title, color = NULL) {
    if (is.null(mesh)) return(plotly_empty())
    
    if (is.null(color)) {
      color <- get_mesh_color(input$global_colorscheme, "primary")
    }
    
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
      wireframe_coords <- create_wireframe_mesh(mesh)
      p <- p %>%
        add_trace(
          name = "Wireframe",
          data = wireframe_coords,
          x = ~ x, y = ~ y, z = ~ z,
          type = "scatter3d",
          mode = "lines",
          line = list(color = "black", width = 1),
          showlegend = FALSE,
          hoverinfo = "skip"
        )
    }
    
    p %>%
      layout(
        title = title,
        showlegend = TRUE,
        scene = list(
          camera = list(
            eye = list(x = -1.5, y = 0.2, z = 1.5),
            up  = list(x = 0, y = 1, z = 0)
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
          feature_angle_deg = input$feature_angle,
          surf_dist_check = input$surf_dist_check,
          adaptive = input$adaptive,
          max_surf_dist = ifelse(input$adaptive, input$max_surf_dist, 2)
        )
        
        incProgress(0.6, detail = "Finalizing...")
        
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
                         type = "success", 
                         duration = 3)
        
        # Switch to remeshed tab
        updateTabsetPanel(session, "mesh_tabs", selected = "Remeshed")
        
      }, error = function(e) {
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
        
        res <- eam_isotropic_remesh(
          mesh3d_obj = values$original_mesh,
          target_edge_length = edge_lengths[i],
          iterations = input$iterations,
          feature_angle_deg = input$feature_angle,
          surf_dist_check = input$surf_dist_check,
          adaptive = input$adaptive
        )
        
        results[[i]] <- data.frame(
          EdgeLength = edge_lengths[i],
          Vertices = res$roi_vertex_count,
          Faces = res$face_count,
          VertexRatio = round(res$roi_vertex_count / res$original_vertex_count, 3),
          FaceRatio = round(res$face_count / ncol(values$original_mesh$it), 3)
        )
      }
    })
    
    values$batch_results <- do.call(rbind, results)
    
    showNotification("Batch processing completed!", 
                     type = "success", 
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
      plotly_empty() %>%
        layout(
          title = "Click 'Perform Remeshing' to generate",
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
    } else {
      create_mesh_plot(values$remeshed_mesh, 
                       paste("Remeshed (", input$edge_length, "mm)"),
                       get_mesh_color(input$global_colorscheme, "secondary"))
    }
  })
  
  output$comparison_plot <- renderPlotly({
    if (is.null(values$remeshed_mesh)) {
      plotly_empty() %>%
        layout(title = "Remeshing required for comparison")
    } else {
      p1 <- create_mesh_plot(values$original_mesh, "Original")
      p2 <- create_mesh_plot(values$remeshed_mesh, "Remeshed",
                             get_mesh_color(input$global_colorscheme, "secondary"))
      
      subplot(p1, p2, nrows = 1, shareX = TRUE, shareY = TRUE) %>%
        layout(title = "Mesh Comparison")
    }
  })
  
  output$batch_comparison <- renderPlotly({
    if (is.null(values$batch_results)) {
      plotly_empty()
    } else {
      plot_ly(values$batch_results) %>%
        add_trace(x = ~EdgeLength, y = ~Vertices, 
                  type = 'scatter', mode = 'lines+markers',
                  name = 'Vertices', yaxis = 'y') %>%
        add_trace(x = ~EdgeLength, y = ~Faces, 
                  type = 'scatter', mode = 'lines+markers',
                  name = 'Faces', yaxis = 'y2') %>%
        layout(
          title = "Batch Processing Results",
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
                    rownames = FALSE) %>%
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