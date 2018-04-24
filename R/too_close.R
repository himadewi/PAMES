#' Check whether a site is too close to other sites
#'
#' Used in "cluster_redution" function only, given the index of a CpG site and
#' a set of indexes of other sites, check if last added site is less than
#' "min_distance" basepair distant from previously retrieved sites.
#'
#' @param new_idx an index (integer)
#' @param prev_idxs a vector of indexes (integer)
#' @param min_distance an integer. Distance in basepairs
#' @param platform_data a data.frame with info about probes location
#' (either \strong{450k} or \strong{27k}).
#'
#' @keywords internal
#'
#' @return logical
too_close <- function(new_idx, prev_idxs, min_distance, platform_data){
  if (length(prev_idxs) == 0) {
    answer <- FALSE
  } else {
    new_site <- platform_data[new_idx,]
    other_sites <- platform_data[prev_idxs,]
    same_chromosome <- other_sites[["Chromosome"]] == new_site[["Chromosome"]]
    if ("Start" %in% names(platform_data)){
      within_min_distance <- abs(other_sites[["Start"]] - new_site[["Start"]]) < min_distance
    } else {
      within_min_distance <-
        abs(other_sites[["Genomic_Coordinate"]] - new_site[["Genomic_Coordinate"]]) < min_distance
    }
    # pairwise comparision of chromosome location and distance
    answer <- any(same_chromosome & within_min_distance)
  }
  return(ifelse(test = is.na(answer), yes = F, no = answer))
}

