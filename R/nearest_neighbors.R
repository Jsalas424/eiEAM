#' n_nearest_neighbors
#'
#' @param training_data Training data is what you want to project on to (matrix or data frame)
#' @param testing_data Testing data is what the data you want to project (matrix or data frame)
#' @param n_neighbors Number of neighbors you want to identify. Higher numbers result in lower specificity coregistration.
#'
#' @return A dataframe with n nearest neighbors
#' @export
#' @importFrom dplyr bind_cols
#' @importFrom dbscan kNN
#'
#' @examples
#' # Create sample data
#' training_data <- matrix(seq(from = 1, to = 9), nrow = 3, ncol = 3)
#' testing_data <- matrix(seq(from = 10, to = 27), ncol = 3)
#' result <- n_nearest_neighbors(training_data, testing_data, n_neighbors = 1)
#' print(result)
n_nearest_neighbors <- function(training_data,
                                     testing_data,
                                     n_neighbors = 1) {

  # Convert to matrices once
  training_data <- as.matrix(training_data)
  testing_data <- as.matrix(testing_data)

  # Run kNN search
  surf_knn <- dbscan::kNN(x = training_data,
                          k = n_neighbors,
                          query = testing_data)

  # Vectorized approach
  n_test <- nrow(testing_data)

  # Create expanded indices
  row_indices <- rep(seq_len(n_test), each = n_neighbors)

  # Extract neighbor indices and distances using correct ordering
  neighbor_indices <- as.vector(t(surf_knn$id[, seq_len(n_neighbors), drop = FALSE]))
  distances <- as.vector(t(surf_knn$dist[, seq_len(n_neighbors), drop = FALSE]))

  # Create output matrices using vectorized indexing
  surf_coord <- training_data[neighbor_indices, , drop = FALSE]
  testing_coord <- testing_data[row_indices, , drop = FALSE]

  # Set column names
  colnames(surf_coord) <- c("surf_x", "surf_y", "surf_z")
  colnames(testing_coord) <- c("x", "y", "z")

  # Combine results
  surf_data <- dplyr::bind_cols(
    as.data.frame(surf_coord),
    surf_dist = distances,
    as.data.frame(testing_coord)
  )

  return(surf_data)
}


#' fr_nearest_neighbors
#'
#' @param training_data Training data is what you want to project on to (matrix or data frame)
#' @param testing_data Testing data is what the data you want to project (matrix or data frame)
#' @param neighbors_radius Radius (Euclidean Distance in millimeter) between the training and testing data you want to coregister. Higher numbers result in lower specificity coregistration
#'
#' @return Return a dataframe with fixed radius nearest neighbors
#' @export
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom dbscan frNN
#' @importFrom purrr map2_dfr imap_dfr
#'
#' @examples
#' # Create sample data
#' training_data <- matrix(seq(from = 1, to = 9), nrow = 3, ncol = 3)
#' testing_data <- matrix(seq(from = 3, to = 32), ncol = 3)
#' result <- fr_nearest_neighbors(training_data, testing_data, neighbors_radius = 20)
#' print(result)
fr_nearest_neighbors <- function(training_data,
                                 testing_data,
                                 neighbors_radius = 1) {

  # Convert to matrices once
  training_data <- as.matrix(training_data)
  testing_data <- as.matrix(testing_data)

  # Run frNN search
  surf_knn <- dbscan::frNN(x = training_data,
                           eps = neighbors_radius,
                           query = testing_data)

  # Early return if no neighbors found
  if (all(lengths(surf_knn$id) == 0)) {
    warning("No neighbors found within the specified radius")
    return(NULL)
  }

  # Use purrr for functional approach with early filtering
  result_list <- purrr::imap(surf_knn$id, function(neighbor_ids, i) {
    if (length(neighbor_ids) == 0) return(NULL)

    n_neighbors <- length(neighbor_ids)
    dists <- surf_knn$dist[[i]]

    # Create data frame for this test point
    data.frame(
      surf_x = training_data[neighbor_ids, 1],
      surf_y = training_data[neighbor_ids, 2],
      surf_z = training_data[neighbor_ids, 3],
      surf_dist = dists,
      x = rep(testing_data[i, 1], n_neighbors),
      y = rep(testing_data[i, 2], n_neighbors),
      z = rep(testing_data[i, 3], n_neighbors)
    )
  })

  # Combine all results
  fr_nn_surf_data <- dplyr::bind_rows(result_list)

  return(fr_nn_surf_data)
}
