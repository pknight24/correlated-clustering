run_sim <- function(n = 100, p = 50, nclust = 1, ngroups = 1, rho = .5, mu = 0,
                    method = "kmeans")
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

  if (method = "kmeans")
  {}




}
