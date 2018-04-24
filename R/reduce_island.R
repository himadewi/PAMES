#' Reduce a matrix subset
#'
#' Takes into consideration the eventual presence of NAs
#' @param matrix_subset a matrix
#' @param n minimum required number of sites per island (return NA otherwise)
#' @keywords internal
reduce_island <- function(matrix_subset, n) {
  if (nrow(matrix_subset) < n) {
    beta_values <- NA
  } else {
    # identify sites without any beta-score
    all_NAs_sites <- rowSums(is.na(matrix_subset)) == ncol(matrix_subset)
    if (sum(!all_NAs_sites) < n) {
      beta_values <- NA
    } else {
      beta_values <- apply(matrix_subset, 2, median, na.rm = T)
    }
  }
  return(beta_values)
}
