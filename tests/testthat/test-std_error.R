describe("std_error with cluster ids", {
  tol <- 1e-8

  # For a sample mean, the EIF is x_i - mean(x).
  # SE = sqrt(var(eif) / n) = sd(x) / sqrt(n) in the unclustered case.

  describe("unclustered (default: each observation is its own cluster)", {
    it("SE of sample mean equals sd(x) / sqrt(n)", {
      set.seed(42)
      x <- rnorm(50, mean = 5, sd = 2)
      obj <- ife(mean(x), x - mean(x))
      expect_equal(obj@std_error, sd(x) / sqrt(length(x)), tolerance = tol)
    })
  })

  describe("single cluster", {
    it("falls back to unclustered formula when all ids are the same", {
      set.seed(42)
      x <- rnorm(50, mean = 5, sd = 2)
      unclustered <- ife(mean(x), x - mean(x))
      one_cluster <- ife(mean(x), x - mean(x), id = rep("A", length(x)))
      expect_equal(one_cluster@std_error, unclustered@std_error, tolerance = tol)
    })
  })

  describe("multiple clusters", {
    it("cluster-robust SE of sample mean is correct with two equal clusters", {
      set.seed(42)
      # 20 obs split into 2 clusters of 10
      x <- rnorm(20, mean = 3, sd = 1)
      id <- rep(c("A", "B"), each = 10)
      eif <- x - mean(x)
      obj <- ife(mean(x), eif, id = id)

      K <- 2L
      n <- 20L
      cluster_sums <- tapply(eif, id, sum)
      cluster_eif <- unname(cluster_sums) * K / n
      expected_se <- sqrt(var(cluster_eif) / K)

      expect_equal(obj@std_error, expected_se, tolerance = tol)
    })

    it("cluster-robust SE of sample mean is correct with unequal cluster sizes", {
      set.seed(42)
      x <- rnorm(30, mean = 0, sd = 1)
      id <- c(rep("A", 5), rep("B", 10), rep("C", 15))
      eif <- x - mean(x)
      obj <- ife(mean(x), eif, id = id)

      K <- 3L
      n <- 30L
      cluster_sums <- tapply(eif, id, sum)
      cluster_eif <- unname(cluster_sums) * K / n
      expected_se <- sqrt(var(cluster_eif) / K)

      expect_equal(obj@std_error, expected_se, tolerance = tol)
    })

    it("cluster-robust SE differs from unclustered SE when there is within-cluster correlation", {
      # Simulate clustered data with strong within-cluster correlation
      set.seed(42)
      K <- 5L
      n_per_cluster <- 10L
      cluster_means <- rnorm(K, 0, 2)
      x <- rep(cluster_means, each = n_per_cluster) + rnorm(K * n_per_cluster, 0, 0.1)
      id <- rep(paste0("C", seq_len(K)), each = n_per_cluster)

      clustered <- ife(mean(x), x - mean(x), id = id)
      unclustered <- ife(mean(x), x - mean(x))

      expect_false(isTRUE(all.equal(clustered@std_error, unclustered@std_error)))
      # With strong within-cluster correlation the cluster-robust SE should be larger
      expect_gt(clustered@std_error, unclustered@std_error)
    })
  })

  describe("clustering with survey weights", {
    it("weighted cluster-robust SE of sample mean is correct", {
      set.seed(42)
      x <- rnorm(20, mean = 5, sd = 2)
      weights <- runif(20, 0.5, 2)
      id <- rep(c("A", "B"), each = 10)
      eif <- x - mean(x)
      obj <- ife(mean(x), eif, weights = weights, id = id)

      K <- 2L
      n <- 20L
      weighted_eif <- eif * weights
      cluster_sums <- tapply(weighted_eif, id, sum)
      cluster_eif <- unname(cluster_sums) * K / n
      expected_se <- sqrt(var(cluster_eif) / K)

      expect_equal(obj@std_error, expected_se, tolerance = tol)
    })
  })

  describe("cluster-robust SE preserved through arithmetic on sample means", {
    # Set up two independent sample means from the same individuals, clustered
    set.seed(7)
    K <- 4L
    n_per_cluster <- 8L
    n <- K * n_per_cluster
    id <- rep(paste0("G", seq_len(K)), each = n_per_cluster)

    x <- rnorm(n, mean = 3)
    y <- rnorm(n, mean = 5)
    eif_x <- x - mean(x)
    eif_y <- y - mean(y)

    obj_x <- ife(mean(x), eif_x, id = id)
    obj_y <- ife(mean(y), eif_y, id = id)

    cluster_robust_se <- function(eif, id) {
      K <- length(unique(id))
      n <- length(eif)
      cluster_sums <- tapply(eif, id, sum)
      cluster_eif <- unname(cluster_sums) * K / n
      sqrt(var(cluster_eif) / K)
    }

    it("difference in means: SE is cluster-robust", {
      z <- obj_x - obj_y
      expect_equal(z@std_error, cluster_robust_se(eif_x - eif_y, id), tolerance = tol)
    })

    it("sum of means: SE is cluster-robust", {
      z <- obj_x + obj_y
      expect_equal(z@std_error, cluster_robust_se(eif_x + eif_y, id), tolerance = tol)
    })

    it("ratio of means: SE is cluster-robust via delta method", {
      z <- obj_x / obj_y
      # Delta method EIF for x/y: eif_x/mean(y) - eif_y*mean(x)/mean(y)^2
      eif_ratio <- eif_x / mean(y) - eif_y * mean(x) / mean(y)^2
      expect_equal(z@std_error, cluster_robust_se(eif_ratio, id), tolerance = tol)
    })

    it("adding a scalar does not change the cluster-robust SE", {
      z <- obj_x + 10
      expect_equal(z@std_error, obj_x@std_error, tolerance = tol)
    })

    it("subtracting a scalar does not change the cluster-robust SE", {
      z <- obj_x - 2
      expect_equal(z@std_error, obj_x@std_error, tolerance = tol)
    })
  })
})
