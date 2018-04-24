#' Remove CpG sites too close to each other
#'
#' Remove sites within 'min_distance' (keep one per 'cluster'), 
#' keeping at most N sites according to their order.
#'
#' @param sites_idx a vector of integers
#' @param N number of sites to retrieve
#' @param min_distance an integer (in basepairs)
#' @param platform_data a data.frame with info about probes location
#' (either \strong{450k} or \strong{27k}).
#'
#' @keywords internal
#'
#' @return a vector of indexes with close sites removed.
cluster_reduction <- function(sites_idx, N, min_distance, platform_data){
  top_idx <- rep(NA, length(sites_idx))
  i <- 1
  n <- 1
  while (n <= N & i <= length(sites_idx)) {
    idx <- sites_idx[i]
    if (!too_close(idx, top_idx[!is.na(top_idx)], min_distance, platform_data)){
      top_idx[n] <- idx
      n <- n + 1
    }
    i <- i+1
  }
  top_idx <- top_idx[!is.na(top_idx)]
  message(sprintf("[%s] %s sites retrieved after 'cluster reduction'.",
    Sys.time(), length(top_idx)))
  return(top_idx)
}

