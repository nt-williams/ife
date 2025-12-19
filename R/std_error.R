std_error <- function(self) {
  weighted_eif <- self@eif * self@weights
  if (length(unique(self@id)) == length(weighted_eif)) {
    cluster_means <- weighted_eif
  } else {
    cluster_means <- collapse::fmean(weighted_eif, self@id)
  }
  j <- length(cluster_means)
  sqrt(collapse::fvar(cluster_means) / j)
}
