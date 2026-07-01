std_error <- function(self) {
  weighted_eif <- self@eif * self@weights
  n_clusters <- length(unique(self@id))
  n <- length(weighted_eif)

  # No clustering, or if only 1 cluster, treat as unclustered (all observations independent)
  if (n_clusters == length(weighted_eif) | n_clusters == 1) {
    return(sqrt(fvar(weighted_eif) / length(weighted_eif)))
  } 
  
  # Multiple clusters: use cluster-robust variance (see doi: 10.1002/sim.9813)
  cluster_sums <- fsum(weighted_eif, self@id)
  cluster_eif <- cluster_sums * n_clusters / n
  sqrt(fvar(cluster_eif) / n_clusters)
}
