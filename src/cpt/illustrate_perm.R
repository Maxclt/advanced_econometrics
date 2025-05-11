library("combinat")
library("tidyverse")
library("ggplot2")
source("CPT.R")

allperms <- combinat::permn(1:8)

set.seed(1)
X <- matrix(rnorm(8 * 3), nrow = 8)
Ostar_vec_normal <- sapply(allperms, function(inds){
    solve_optim_eta(X[inds, ], 1, 1)$Ostar
})

set.seed(1)
X <- matrix(rt(8 * 3, df = 1), nrow = 8)
Ostar_vec_cauchy <- sapply(allperms, function(inds){
    solve_optim_eta(X[inds, ], 1, 1)$Ostar
})

## Ostars <- data.frame(Ostar_vec_normal,
##                      Ostar_vec_cauchy)
## names(Ostars) <- c("X ~ i.i.d. Normal", "X ~ i.i.d. Cauchy")
Ostar_vec <- data.frame(value = Ostar_vec_normal)
    
plot <- Ostars %>% gather %>%
    ggplot() +
    geom_histogram(aes(x = value, y = ..density..)) +
    ## facet_grid(~ key) +
    ## scale_y_continuous(expand = c(0, 0)) +
    xlab("Values of O* for different ordering") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 12.5),
          strip.text = element_text(size = 15))
ggsave(filename = "../figs/ordering_illustrate.pdf", plot,
       width = 4, height = 3)
