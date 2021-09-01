run_sim <- function(n = 100, p = 50, nclust = 1, ngroups = 1, rho = .5, mu = 0,
                    clustering_method = "kmeans", k, ...)
{
  if (length(mu) != nclust) warning("Mu must have length nclust")
  Sigma <- Matrix::bdiag(lapply(1:(n/ngroups), function(i){
    I <- (1-rho) * diag(ngroups)
    ones <- rho * rep(1 ,ngroups) %*% t(rep(1, ngroups))
    I + ones
  }))
  cluster_names <- LETTERS[1:nclust]
  cluster_assignment <- sample(cluster_names, size = n, replace = TRUE) # randomly assign each point to a cluster
  mu_vec <- rep(0, n)
  for (cl in 1:length(cluster_names)) # create the mean vector according to cluster assignments
    mu_vec <- mu_vec + ifelse(cluster_assignment == cluster_names[cl],
                              mu[cl],
                              0)
  X <- t(MASS::mvrnorm(n = p, mu = mu_vec, n/nclust,
        Sigma = Sigma))

  if (clustering_method == "kmeans"){
    kmeans.out <- kmeans(X, centers = k)
    cluster_estimate <- kmeans.out$cluster
  }
  if (clustering_method == "hclust")
  {
    dist.mat <- dist(X)
    hclust.out <- hclust(dist.mat, ...)
    cluster_estimate <- cutree(hclust.out, k = k)
  }

  #### calculate the accuracy metric
  Assignment_mat <- model.matrix(~ cluster_assignment - 1)
  D1 <- as.matrix(dist(Assignment_mat, method = "euclidean")) == 0 ## indicates which points have the same true assignment

  Estimate_mat <- model.matrix(~ cluster_estimate - 1)
  D2 <- as.matrix(dist(Estimate_mat, method = "euclidean")) == 0

  shared <- D1 == D2 ## indicates the points that agree on both the true and estimated clusters
  agree_prop <- mean(shared[lower.tri(shared)])

  results <- data.frame(cluster_assignment, cluster_estimate)
  list(X = X, results = results, agree_prop = agree_prop)

}
