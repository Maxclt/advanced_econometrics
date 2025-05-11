source("CPT.R")
source("expr_functions.R")
library("tidyverse")

nreps <- 3000
nperms <- 1000
m <- 19
dir_dist <- indep_dir_dist

X_dist <- Sys.getenv("Xdist")
eps_dist_name <- Sys.getenv("epsdist")
eps_dist <- dist_map(eps_dist_name)
ratio <- floor(as.numeric(Sys.getenv("ratio")))
ninds <- floor(as.numeric(Sys.getenv("ninds")))
seed <- as.numeric(Sys.getenv("seed"))
set.seed(seed * 2019)

filename <- paste0("../data/power_", X_dist,
                   "_", eps_dist_name, 
                   "_ratio", ratio, "_ninds", ninds,
                   "_seed", seed, ".RData")

X_file <- paste0("../data/mat_", X_dist, "_ratio", ratio,
                 "_ninds", ninds, "_seed", seed, ".RData")
load(X_file)
X <- result$X
ordering1 <- result$ordering1
ordering2 <- result$ordering2

## Set beta = b X dir where dir is a random vector generated
## from dir_dist. Find b at which the power of t/F test is
## roughly 20%
print("Find benchmark beta")
bench_beta <- find_bench_Ftests(X, 1:ninds, eps_dist,
                                nreps = 10000,
                                dir_dist = dir_dist)

## List of beta's to evaluate
beta_list <- bench_beta * (0:5)
## Weight matrix for CPT
if (ninds == 1){
    M <- NULL
} else {
    M <- diag((1:ninds) / ninds) + 1
}


print("Experiment Started")
res <- matrix(NA, 8, 6)
res <- as.data.frame(res)
names(res) <- as.character(0:5)

print("t/F test")
res[1, ] <- fast_Ftests_pow(X, 1:ninds, eps_dist, nreps, beta_list, dir_dist)

print("Permutation t/F test")
res[2, ] <- fast_FPerm_pow(X, 1:ninds, eps_dist, nreps, beta_list, dir_dist, nperms)

print("Freedman-Lane test")
res[3, ] <- fast_FPerm_FL_pow(X, 1:ninds, eps_dist, nreps, beta_list, dir_dist, nperms)

print("weak CPT")
res[4, ] <- fast_CPT_pow(X, 1:ninds, eps_dist, nreps, beta_list, ordering1, m, dir_dist, M)

print("strong CPT")
res[5, ] <- fast_CPT_pow(X, 1:ninds, eps_dist, nreps, beta_list, ordering2, m, dir_dist, M)

print("random CPT")
res[6, ] <- CPT_random_pow(X, 1:ninds, eps_dist, nreps, beta_list, m, dir_dist, M)

print("LAD")
res[7, ] <- LAD_pow(X, 1:ninds, eps_dist, nreps, beta_list, dir_dist)

print("Group Bound")
res[8, ] <- GB_power(X, 1:ninds, eps_dist, 100, beta_list, dir_dist)

res <- res %>% gather(key = "signal", value = "power")
res$signal <- as.numeric(res$signal)
if (ninds == 1){
    res$methods <- c("t test", "Perm. t test", "FL test", "CPT1", "CPT2", "random CPT", "LAD", "Group Bound")
} else {
    res$methods <- c("F test", "Perm. F test", "FL test", "CPT1", "CPT2", "random CPT", "LAD", "Group Bound")
}
res$Xdist <- X_dist
res$epsdist <- eps_dist_name
res$ratio <- ratio
res$ninds <- ninds
res$seed <- seed

save(file = filename, res)
