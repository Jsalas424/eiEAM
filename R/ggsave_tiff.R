#' Save ggplot as High Resolution TIFF with LZW Compression
#'
#' @description
#' A wrapper around [ggplot2::ggsave()] that uses the ragg device to save
#' plots as high-resolution TIFF files with LZW compression. This format is
#' commonly required for scientific publications.
#'
#' @param filename File name to create on disk. Should have a `.tif` or 
#'   `.tiff` extension.
#' @param path Path of the directory to save plot to. If `NULL` (default),
#'   uses the current working directory.
#' @param plot Plot to save. Defaults to last plot displayed.
#' @param width,height Plot size in `units` (defaults to the size of the 
#'   current graphics device).
#' @param units One of "in" (inches, default), "cm", "mm", or "px" (pixels).
#' @param dpi Plot resolution. Defaults to 300 DPI for publication quality.
#' @param compression Compression type for TIFF. Defaults to "lzw" for 
#'   lossless compression. Other options include "none", "rle", "jpeg", 
#'   "deflate", or "zip".
#' @param scale Multiplicative scaling factor. Defaults to 1.
#' @param limitsize When `TRUE`, ggsave will not save images larger 
#'   than 50x50 inches to prevent common errors.
#' @param ... Other arguments passed on to [ragg::agg_tiff()] and 
#'   [ggplot2::ggsave()].
#'
#' @return Invisibly returns the filename, following [ggplot2::ggsave()] 
#'   convention.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' 
#' # Create a simple plot
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_minimal()
#' 
#' # Save as high-res TIFF with default settings (300 DPI, LZW compression)
#' ggsave_tiff("myplot.tiff", plot = p, width = 6, height = 4)
#' 
#' # Save with custom DPI
#' ggsave_tiff("myplot_600dpi.tiff", plot = p, width = 6, height = 4, dpi = 600)
#' 
#' # Save with different compression
#' ggsave_tiff("myplot_zip.tiff", plot = p, width = 6, height = 4, 
#'             compression = "zip")
#' 
#' # Save last displayed plot (no need to specify plot argument)
#' print(p)
#' ggsave_tiff("lastplot.tiff", width = 8, height = 6)
#' }
#'
#' @references
#' Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis. 
#'   Springer-Verlag New York. ISBN 978-3-319-24277-4, 
#'   \url{https://ggplot2.tidyverse.org}.
#'
#' @seealso [ggplot2::ggsave()], [ragg::agg_tiff()]
ggsave_tiff <- function(filename,
                        path = NULL,
                        plot = ggplot2::last_plot(),
                        width = NA,
                        height = NA,
                        units = c("in", "cm", "mm", "px"),
                        dpi = 300,
                        compression = "lzw",
                        scale = 1,
                        limitsize = FALSE,
                        ...) {
  
  # Input validation
  if (!is.character(filename) || length(filename) != 1) {
    stop("`filename` must be a single character string", call. = FALSE)
  }
  
  if (!requireNamespace("ragg", quietly = TRUE)) {
    stop(
      "Package 'ragg' is required for this function. ",
      "Install it with: install.packages('ragg')",
      call. = FALSE
    )
  }
  
  # Match units argument
  units <- match.arg(units)
  
  # Validate compression type
  valid_compression <- c("none", "rle", "lzw", "jpeg", "deflate", "zip")
  if (!compression %in% valid_compression) {
    stop(
      "`compression` must be one of: ",
      paste(valid_compression, collapse = ", "),
      call. = FALSE
    )
  }
  
  # Call ggsave with ragg device
  ggplot2::ggsave(
    filename = filename,
    path = path,
    plot = plot,
    width = width,
    height = height,
    units = units,
    scale = scale,
    dpi = dpi,
    device = ragg::agg_tiff,
    compression = compression,
    limitsize = limitsize,
    ...
  )
}