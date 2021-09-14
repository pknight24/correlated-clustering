library(tibble)
library(rslurm)
source("run_sim.R")

total_repeats <- 2

run_sim_helper <- function(n, p, nclust, ngroups, rho, mu_step_size, clustering_method, k)
{
  mu <- (0:(nclust - 1)) * mu_step_size ## important: this assumes that the cluster means are all equally far apart. may not be true in real data
  run_sim(n, p, nclust, ngroups, rho, mu, clustering_method, k)
}

n <- c(100, 1000, 5000)
p <- c(50, 500, 1000)
nclust <- c(2, 3, 4)
ngroups <- c(2, 10, 50) ## for better simulations, the number of groups should depend on n
rho <- c(0.2, 0.5, 0.8)
mu_step_size <- c(0.25, 1)
clustering_method <- c("kmeans", "hclust")
#k <- c(2, 3, 4)

params <- tidyr::crossing(n, p, nclust, ngroups, rho, mu_step_size, clustering_method)
params$k <- params$nclust ## we always guess the right number of clusters (for now)
params <- as.data.frame(params)

params_total <- params
for (i in 1:(total_repeats - 1)) params_total <- rbind(params_total, params)

results <- sapply(1:nrow(params_total), function(i){
  run_sim_helper(n = params[i,"n"], p = params[i,"p"], nclust = params[i,"nclust"],
                 ngroups = params[i,"ngroups"], rho = params[i,"rho"],
                 mu_step_size = params[i, "mu_step_size"],
                 clustering_method = params[i,"clustering_method"], k = params[i,"k"])$agree_prop
  })

save(results, file = "results.RData")
