
#### Agglomerate results on power
params <- lapply(1:5, function(i){
    params_filename <- paste0("params_CPT_expr_", i, ".txt")
    read.table(params_filename)
})
params <- do.call(rbind, params)

reslist <- list()
for (i in 1:nrow(params)){
    X_dist <- as.character(params[i, 1])
    eps_dist_name <- as.character(params[i, 2])
    ratio <- as.numeric(params[i, 3])
    ninds <- as.numeric(params[i, 4])
    seed <- as.numeric(params[i, 5])

    filename <- paste0("../data/power_", X_dist,
                       "_", eps_dist_name,
                       "_ratio", ratio, "_ninds", ninds,
                       "_seed", seed, ".RData")
    tmp <- try(load(filename))
    if (class(tmp)[1] != "try-error"){
        reslist[[i]] <- res
    }
}

res <- do.call(rbind, reslist) %>%
    arrange(seed, ratio, ninds, epsdist, Xdist, methods, signal)

save(file = "../data/power_CPT_expr.RData", res)

#### Agglomerate results on GA and SS
params <- read.table("params_GA_SS.txt")

reslist <- list()
for (i in 1:nrow(params)){
    X_dist <- as.character(params[i, 1])
    algo <- as.character(params[i, 2])
    popSize <- as.numeric(params[i, 3])
    seed <- as.numeric(params[i, 4])

    if (algo == "GA"){
        filename <- paste0("../data/GA_", popSize, "_", X_dist, "_", seed, ".RData")
    } else if (algo == "SS"){
        filename <- paste0("../data/SS_", X_dist, "_", seed, ".RData")
    }
    
    tmp <- try(load(filename))
    df <- data.frame(rounds = 1:50, value = result,
                     Xdist = X_dist, algo = algo,
                     popSize = popSize, seed = seed)
    if (class(tmp)[1] != "try-error"){
        reslist[[i]] <- df
    }
}

res <- do.call(rbind, reslist)

save(file = "../data/CPT_GA_SS.RData", res)
