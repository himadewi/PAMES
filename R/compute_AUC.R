#' Compute Area Under Curve
#'
#' This function compute the Area Under Curve used to define the segregation
#' between tumor and normal samples according to their beta-score.
#'
#' @param tumor a matrix of beta scores generated by an Illumina BeadChip.
#' @param control a matrix of beta scores generated by an Illumina BeadChip.
#' @param max_NAs_frac a value between 0 and 1. Sites with a fraction of NAs
#' major than or equal to it are excluded (default = 1; no NAs allowed).
#' @param parallel try to parallelize with a forked-process cluster? (FALSE) 
#' 
#' @return a vector of AUC scores
#'
#' @import parallel 
#' 
#' @export
#' @examples
#' auc_data <- compute_AUC(tumor_toy_data, control_toy_data)
#' auc_data_bs <- compute_AUC(bs_tumor_toy_data, bs_control_toy_data)
compute_AUC <- function(tumor, control, max_NAs_frac=1, parallel=FALSE){

  stopifnot(nrow(tumor) == nrow(control))
  stopifnot(max_NAs_frac >= 0 | max_NAs_frac <= 1)
  stopifnot(identical(rownames(tumor), rownames(control)))

  if (is(tumor, "GenomicRatioSet")) tumor <- getBeta(tumor)
  tumor <- as.matrix(tumor)
  
  if (is(control, "GenomicRatioSet")) control <- getBeta(control)
  control <- as.matrix(control)

  message(sprintf("[%s] Checking per-row NAs fraction...",  Sys.time()))
  auc <- rep(NA, nrow(tumor))
  names(auc) <- rownames(tumor)

  valid_tumor_row_logi <- apply(tumor, 1, function(x) sum(is.na(x)) / length(x) < max_NAs_frac)
  valid_control_row_logi <- apply(control, 1, function(x) sum(is.na(x)) / length(x) < max_NAs_frac)
  valid_row_idx <- which(valid_tumor_row_logi & valid_control_row_logi)

  full_table <- cbind(tumor[valid_row_idx, ], control[valid_row_idx, ])
  state <- c(rep(1, ncol(tumor)), rep(0, ncol(control)))
  message(sprintf("[%s] Computing AUC ", Sys.time()))

  auc_fn <- function(x){
    non_NA_idx <- which(!is.na(x))
    roc <- ROC::rocdemo.sca(truth=state[non_NA_idx], 
                            data=x[non_NA_idx], cutpts=seq(0, 1, .01))
    ROC::AUC(roc)
  }

  if (parallel) { 
    cl <- makeForkCluster() 
    valid_auc <- parApply(cl=cl, full_table, 1, auc_fn)
    stopCluster(cl) 
  } else { 
    valid_auc <- apply(full_table, 1, auc_fn)
  }
  auc[valid_row_idx] <- valid_auc
  message(sprintf("[%s] Done",  Sys.time()))

  # allow using subsets of a platform's probes
  return(auc)
}
