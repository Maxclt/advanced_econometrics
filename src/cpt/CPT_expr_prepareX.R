source("expr_functions.R")
n <- 1000
m <- 19
rounds1 <- 100
rounds2 <- 900

X_dist <- Sys.getenv("Xdist")
ratio <- floor(as.numeric(Sys.getenv("ratio")))
ninds <- floor(as.numeric(Sys.getenv("ninds")))
seed <- as.numeric(Sys.getenv("seed"))
set.seed(seed)

X_file <- paste0("../data/mat_", X_dist, "_ratio", ratio,
                 "_ninds", ninds, "_seed", seed, ".RData")

p <- ceiling(n / ratio)
X <- gen_X(n, p, X_dist)
if (ninds == 1){
    M <- NULL
} else {
    M <- diag((1:ninds) / ninds) + 1
}

#### Genetic Algorithm
res1 <- find_eta_GA(X, m, testinds = 1:ninds,
                    popSize = 10, rounds = rounds1,
                    M = M)
res2 <- find_eta_GA(X, m, ga_obj = res1$ga_obj,
                    testinds = 1:ninds,
                    popSize = 10, rounds = rounds2,
                    M = M)
result <- list(X = X,
               ordering1 = res1$ordering,
               ordering2 = res2$ordering)
save(file = X_file, result)
