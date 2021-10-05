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
  if (clustering_method == "spectral")
  {
    dist.mat <- as.matrix(dist(X))
    sim.mat <- exp(dist.mat^2 / -50) ## gaussian kernel with sigma^2 = 25 (for scaling)
    adj <- apply(sim.mat, 1, function(x){ ## this computes a graph
      adj.vec <- rep(0,n)
      adj.vec[order(x)[1:25]] <- 1 ## I keep the graph relatively dense (if its too sparse we get sparse eigenvectors later)
      return(adj.vec)
    })
    D <- diag(rowSums(adj))
    D_half_inverse <- diag(sapply(diag(D), function(x) ifelse(x > 0, 1 / sqrt(x) , 0))
                           )
    L <- D - adj ## unnormalized laplacian
    L_norm <- D_half_inverse %*% L %*% D_half_inverse
    vectors <- eigen(L_norm)$vectors[,1:k]
    vectors_norm <- apply(vectors, 1, function(x) ifelse(all(x == 0), x, x / (t(x) %*% x)))
    cluster_estimate <- kmeans(vectors_norm, centers = k)$cluster

  }

  #### calculate the accuracy metric
  browser()
  Assignment_mat <- model.matrix(~ cluster_assignment - 1)
  Assignment_dist <- as.matrix(dist(Assignment_mat, method = "euclidean"))
  true_same <- Assignment_dist == 0 ## indicates which points have the same true assignment

  Estimate_mat <- model.matrix(~ cluster_estimate - 1)
  Estimate_dist <- as.matrix(dist(Estimate_mat, method = "euclidean"))
  estimated_same <- Estimate_dist == 0

  shared<- true_same == estimated_same ## indicates the points that agree on both the true and estimated clusters
  ri <- mean(shared[lower.tri(shared)])

  results <- data.frame(cluster_assignment, cluster_estimate)
  list(X = X, results = results, ri = ri)

}
