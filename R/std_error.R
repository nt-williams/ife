std_error <- function(self) {
  weighted_eif <- self@eif * self@weights
  n_clusters <- length(unique(self@id))

  # If only 1 cluster, treat as unclustered (all observations independent)
  if (n_clusters == 1) {
    n <- length(weighted_eif)
    return(sqrt(collapse::fvar(weighted_eif) / n))
  }

  # Multiple clusters: use cluster-robust variance
  if (n_clusters == length(weighted_eif)) {
    cluster_means <- weighted_eif
  } else {
    cluster_means <- collapse::fmean(weighted_eif, self@id)
  }

  sqrt(collapse::fvar(cluster_means) / n_clusters)
}
