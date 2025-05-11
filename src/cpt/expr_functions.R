library("quantreg")
library("hdi")
source("CPT.R")

rt1 <- function(n){
    rt(n, df = 1)
}

rt2 <- function(n){
    rt(n, df = 2)
}

rt3 <- function(n){
    rt(n, df = 3)
}

rpareto <- function(n, df){
    tmp <- runif(n)
    (1 - tmp)^(-1/df)
}

rpareto1 <- function(n){
    rpareto(n, 1)
}

rpareto2 <- function(n){
    rpareto(n, 2)
}

rpareto3 <- function(n){
    rpareto(n, 3)
}

rpareto4 <- function(n){
    rpareto(n, 4)
}

dist_map <- function(dist_name){
    switch(dist_name,
           normal = rnorm,
           t1 = rt1,
           t2 = rt2,
           t3 = rt3,
           pareto1 = rpareto1,
           pareto2 = rpareto2,
           pareto3 = rpareto3,
           pareto4 = rpareto4)
}

gen_X <- function(n, p, X_dist,
                  X_row_rho = 0, X_col_rho = 0){
    if (X_dist %in% c("normal", "t1", "t2", "t3",
                      "pareto1", "pareto2", "pareto3",
                      "pareto4")){
        X_dist <- dist_map(X_dist)
        X <- matrix(X_dist(n * p), nrow = n, ncol = p)
        if (X_row_rho != 0){
            row_factors <- X_dist(n)
            X <- X + row_factors %*% t(rep(1, p)) * X_row_rho
        }
        if (X_col_rho != 0){
            col_factors <- X_dist(p)
            X <- X + rep(1, n) %*% t(col_factors) * X_col_rho
        }
    }  else if (X_dist == "ANOVA1"){
        X <- matrix(0, nrow = n - 3 * p, ncol = p)
        ind <- cbind(1:(n - 3 * p),
                     sample(p, n - 3 * p, replace = TRUE))
        X[ind] <- 1
        id <- diag(rep(1, p))
        X <- rbind(id, id, id, X)
        X <- X[, -ncol(X)]
    } 
    return(X)
}

gen_eps <- function(n, eps_dist, eps_rho = 0){
    eps <- eps_dist(n)
    if (eps_rho != 0){
        eps_factor <- eps_dist(1)
        eps <- eps + eps_factor * eps_rho
    }
    return(eps)
}

indep_dir_dist <- function(nreps, ntestinds = 5){
    vars <- (1:ntestinds) / ntestinds
    vars <- rep(vars, nreps)
    tmp <- rnorm(ntestinds * nreps) * sqrt(vars) + 1
    matrix(tmp, nrow = ntestinds)
}

fast_Ftests_pow <- function(X, testinds, err_dist,
                            nreps, beta,
                            dir_dist = NULL){
    n <- nrow(X)
    p <- ncol(X)
    r <- length(testinds)
    nbeta <- length(beta)
    eps_mat <- matrix(err_dist(n * nreps), nrow = n)
    if (r == 1){
        base_modifier <- resid(lm(X[, testinds] ~ X[, -testinds]))
        base_modifier <- as.numeric(base_modifier)
    } else {
        betadir <- dir_dist(nreps)
        base_modifier <- resid(lm(X[, testinds] ~ X[, -testinds]))
        base_modifier <- base_modifier %*% betadir
    }
    
    resids_full <- resid(lm(eps_mat ~ X))
    rss_full <- as.numeric(colSums(resids_full^2))

    resids_reduce0 <- resid(lm(eps_mat ~ X[, -testinds]))
    rss_reduce_list <- lapply(beta, function(b){
        modifier <- base_modifier * b
        resids_reduce <- resids_reduce0 + modifier
        as.numeric(colSums(resids_reduce^2))
    })
    
    df1 <- r
    df2 <- n - p - 1
    power <- sapply(rss_reduce_list, function(rss_reduce){
        Fstats <- (rss_reduce - rss_full) / rss_full * df2 / df1
        crit_val <- qf(0.95, df1, df2)
        mean(Fstats > crit_val)
    })
    power
}

find_bench_Ftests <- function(X, testinds, err_dist, nreps,
                              dir_dist = NULL,
                              targetpow = 0.2,
                              tol = 0.01,
                              lower = 0, upper = 5){
    beta <- seq(lower, upper, length.out = 100)
    pow <- fast_Ftests_pow(X, testinds, err_dist, nreps, beta, dir_dist)
    pow_diff <- abs(pow - targetpow)
    if (min(pow_diff) < tol){
        ind <- which.min(pow_diff)
        beta[[ind]]
    } else if (max(pow) < targetpow){
        lower <- upper
        upper <- upper * 2
        find_bench_Ftests(X, testinds, err_dist, nreps,
                          dir_dist, targetpow, tol,
                          lower, upper)
    } else {
        lower <- max(beta[pow < targetpow])
        upper <- min(beta[pow > targetpow])
        find_bench_Ftests(X, testinds, err_dist, nreps,
                          dir_dist, targetpow, tol,
                          lower, upper)
    }
}

fast_FPerm_pow <- function(X, testinds, err_dist,
                           nreps, beta,
                           dir_dist = NULL,
                           nperms = 200){
    n <- nrow(X)
    r <- length(testinds)
    eps_mat <- matrix(err_dist(n * nreps), nrow = n)
    if (r == 1){
        base_modifier <- resid(lm(X[, testinds] ~ X[, -testinds]))
        base_modifier <- as.numeric(base_modifier)
        base_signal <- X[, testinds]        
    } else {
        betadir <- dir_dist(nreps)
        base_modifier <- resid(lm(X[, testinds] ~ X[, -testinds]))
        base_modifier <- base_modifier %*% betadir
        base_signal <- X[, testinds] %*% betadir
    }    
    resids_reduce0 <- resid(lm(eps_mat ~ X[, -testinds]))
    resids_full <- resid(lm(eps_mat ~ X))
    rss_full <- as.numeric(colSums(resids_full^2))

    power <- sapply(beta, function(b){
        modifier <- base_modifier * b
        signal <- base_signal * b
        resids_reduce <- resids_reduce0 + modifier
        rss_reduce <- as.numeric(colSums(resids_reduce^2))
        rss_full_perm <- lapply(1:nperms, function(i){
            Xperm <- X
            Xperm[, testinds] <- Xperm[sample(n, n), testinds]
            y_mat <- eps_mat + signal
            resids_full <- resid(lm(y_mat ~ Xperm))
            rss_full <- as.numeric(colSums(resids_full^2))
            rss_full
        })
        rss_full_perm <- do.call(cbind, rss_full_perm)
        rss_full_all <- cbind(rss_full, rss_full_perm)
        Fstats <- (rss_reduce - rss_full_all) / rss_full_all
        pvals <- apply(Fstats, 1, function(x){
            rank(-x)[1] / (1 + nperms)
        })
        mean(pvals <= 0.05 + 1e-10, na.rm = TRUE)
    })
    power
}

fast_FPerm_FL_pow <- function(X, testinds, err_dist,
                              nreps, beta,
                              dir_dist = NULL,
                              nperms = 200){
    n <- nrow(X)
    r <- length(testinds)
    eps_mat <- matrix(err_dist(n * nreps), nrow = n)
    if (r == 1){
        base_modifier <- resid(lm(X[, testinds] ~ X[, -testinds]))
        base_modifier <- as.numeric(base_modifier)
        base_preds <- X[, testinds] - base_modifier
    } else {
        betadir <- dir_dist(nreps)
        base_modifier <- resid(lm(X[, testinds] ~ X[, -testinds]))
        base_preds <- (X[, testinds] - base_modifier) %*% betadir        
        base_modifier <- base_modifier %*% betadir
    }    
    
    mod_reduce <- lm(eps_mat ~ X[, -testinds])
    resids_reduce0 <- resid(mod_reduce)
    preds_reduce0 <- predict(mod_reduce)
    rss_reduce0 <- as.numeric(colSums(resids_reduce0^2))
    resids_full0 <- resid(lm(eps_mat ~ X))
    rss_full0 <- as.numeric(colSums(resids_full0^2))
    
    power <- sapply(beta, function(b){
        resids_modifier <- base_modifier * b
        preds_modifier <- base_preds * b
        resids_reduce <- resids_reduce0 + resids_modifier
        preds_reduce <- preds_reduce0 + preds_modifier
        rss_reduce_modified <- as.numeric(colSums(resids_reduce^2))
        Fstats <- (rss_reduce_modified - rss_full0) / rss_full0
        Fstats_perm <- lapply(1:nperms, function(i){
            err <- resids_reduce[sample(n, n), ]
            y_mat <- preds_reduce + err
            resids_reduce <- resid(lm(y_mat ~ X[, -testinds]))
            rss_reduce <- as.numeric(colSums(resids_reduce^2))
            resids_full <- resid(lm(y_mat ~ X))
            rss_full <- as.numeric(colSums(resids_full^2))
            (rss_reduce - rss_full) / rss_full
        })
        Fstats_perm <- do.call(cbind, Fstats_perm)
        Fstats_all <- cbind(Fstats, Fstats_perm)
        pvals <- apply(Fstats_all, 1, function(x){
            rank(-x)[1] / (1 + nperms)
        })
        mean(pvals <= 0.05 + 1e-10, na.rm = TRUE)
    })
    power
}

LAD_pow <- function(X, testinds, err_dist,
                    nreps, beta,
                    dir_dist = NULL){
    n <- nrow(X)
    p <- ncol(X)
    r <- length(testinds)
    data_names <- c("y", paste0("X", 1:p))
    formula_full <- "y ~ ."
    nontestinds <- setdiff(1:p, testinds)
    formula_reduce <- paste0("y ~ ", paste0("X", nontestinds, collapse = " + "))
    power <- sapply(beta, function(b){
        pvals <- rep(NA, nreps)
        for (i in 1:nreps){
            if (r == 1){
                betastar <- b
            } else {
                betastar <- b * dir_dist(1)
            }
            y <- X[, testinds, drop = FALSE] %*% betastar + err_dist(n)
            dat <- data.frame(y, X)
            names(dat) <- data_names
            formula_full <- as.formula(formula_full)
            mod_full <- try(quantreg::rq(formula_full, data = dat))
            formula_reduce <- as.formula(formula_reduce)
            mod_reduce <- try(quantreg::rq(formula_reduce, data = dat))
            if (class(mod_full)[1] != "try-error" &&
                class(mod_reduce)[1] != "try-error"){
                res <- try(quantreg::anova.rq(mod_full, mod_reduce)$table)
                if (class(res)[1] != "try-error"){
                    pvals[i] <- res[1, 4]
                }
            } else {
                print("Error occurs in LAD!")
            }
        }
        mean(pvals <= 0.05 + 1e-10, na.rm = TRUE)
    })
    return(power)
}

fast_CPT_pow <- function(X, testinds, err_dist,
                         nreps, beta,
                         ordering, m,
                         dir_dist = NULL, M = NULL){
    n <- nrow(X)
    r <- length(testinds)    
    X <- preprocess_X(X, testinds = testinds)$X
    X <- X[ordering, ]
    eta <- solve_optim_eta(X, m, length(testinds), M)$etastar
    Piinds <- left_shift(n, m)
    eta_mat <- apply(Piinds, 2, function(inds){
        eta[inds]
    })
    eps_mat <- matrix(err_dist(n * nreps), nrow = n)
    if (r == 1){
        base_signal <- X[, testinds]
        base_modifier <- as.numeric(t(eta_mat) %*% base_signal)
    } else {
        betadir <- dir_dist(nreps)
        base_signal <- X[, testinds] %*% betadir
        base_modifier <- t(eta_mat) %*% base_signal
    }    
    
    stats0 <- t(eta_mat) %*% eps_mat
    power <- sapply(beta, function(b){
        stats <- stats0 + base_modifier * b
        pvals <- apply(stats, 2, function(x){
            x <- abs(x - median(x))
            rank(-x)[1] / nrow(stats)
        })
        mean(pvals <= 0.05 + 1e-10, na.rm = TRUE)
    })
    power
}

CPT_random_pow <- function(X, testinds, err_dist,
                           nreps, beta,
                           m, dir_dist = NULL, M = NULL){
    n <- nrow(X)
    r <- length(testinds)    
    X <- preprocess_X(X, testinds = testinds)$X
    Piinds <- left_shift(n, m)
    pvals <- matrix(NA, nrow = length(beta), ncol = nreps)
    for (i in 1:nreps){
        if (r == 1){
            base_signal <- X[, testinds, drop = FALSE]
        } else {
            base_signal <- X[, testinds] %*% dir_dist(1)
        }    
        ordering <- sample(n, n)
        X <- X[ordering, ]
        eta <- solve_optim_eta(X, m, length(testinds), M)$etastar
        eta_mat <- apply(Piinds, 2, function(inds){
            eta[inds]
        })
        eps <- err_dist(n)
        stats0 <- as.numeric(t(eta_mat) %*% eps)
        pvals[, i] <- sapply(beta, function(b){
            stats <- stats0 + t(eta_mat) %*% base_signal * b
            stats <- abs(stats - median(stats))
            rank(-stats)[1] / nrow(stats)
        })
    }
    power <- apply(pvals, 1, function(x){
        mean(x <= 0.05 + 1e-10, na.rm = TRUE)
    })
    power
}

GB_power <- function(X, testinds, err_dist,
                     nreps, beta,
                     dir_dist = NULL){
    n <- nrow(X)
    r <- length(testinds)
    power <- sapply(beta, function(b){
        pvals <- rep(NA, nreps)
        for (i in 1:nreps){
            if (r == 1){
                betastar <- b
            } else {
                betastar <- b * dir_dist(1)
            }
            y <- X[, testinds, drop = FALSE] %*% betastar + err_dist(n)
            lb <- try(hdi::groupBound(X, y, group = testinds, silent = TRUE)[1])
            if (class(lb)[1] != "try-error"){
                pvals[i] <- as.numeric(lb == 0)
            } else {
                print("Error occurs in hdi!")
            }

        }
        mean(pvals <= 0.05 + 1e-10, na.rm = TRUE)
    })
    return(power)    
}
