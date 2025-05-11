### Compare the performance of Genetic Algorithm and Naive Stochastic Search
source("CPT.R")
source("expr_functions.R")

X_dist <- Sys.getenv("Xdist")
algo <- Sys.getenv("algo")
popSize <- as.integer(Sys.getenv("popSize"))
seed <- as.numeric(Sys.getenv("seed"))
set.seed(seed)

samples_per_record <- 1000
num_records <- 50
ntestinds <- 1

if (algo == "GA"){
    filename <- paste0("../data/GA_", popSize, "_", X_dist, "_", seed, ".RData")
} else if (algo == "SS"){
    filename <- paste0("../data/SS_", X_dist, "_", seed, ".RData")
}

n <- 1000
p <- 20
m <- 19
X <- gen_X(n, p, X_dist)
result <- rep(0, num_records)

#### GA setup
if (algo == "GA"){
    rounds_per_record <- samples_per_record / popSize
    objective_FUN <- function(ordering){
        solve_optim_eta(X[ordering, ], m, ntestinds)$Ostar
    }    
    ga_obj <- GAPerm(objective_FUN, n, popSize = popSize)
    
    for (i in 1:num_records){
        ga_obj$evolve(rounds_per_record)
        result[i] <- max(ga_obj$bestFit(), na.rm = TRUE)
        save(file = filename, result)
        print(i)
    }
}

#### SS setup
if (algo == "SS"){
    ss_current_max <- 0

    for (i in 1:num_records){
        ss_tmp <- sapply(1:samples_per_record, function(ind){
            ordering <- sample(n, n)
            tmp <- solve_optim_eta(X[ordering, ], m, ntestinds)
            tmp$Ostar
        })
        result[i] <- max(max(ss_tmp), ss_current_max) 
        ss_current_max <- result[i]
        save(file = filename, result)
        print(i)
    }    
}
