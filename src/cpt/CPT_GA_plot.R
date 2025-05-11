library("ggplot2")
library("dplyr")
load("../data/CPT_GA_SS.RData")

GA_SS_plot <- filter(res, popSize == 10 | algo == "SS",
                     seed == 1) %>%
    select(-popSize) %>%
    spread(Xdist, value) %>%
    mutate(ANOVA1 = ANOVA1 / max(ANOVA1),
           normal = normal / max(normal),
           t1 = t1 / max(t1)) %>%
    gather(Xdist, value, -rounds, -algo, -seed) %>%
    mutate(Xdist = factor(Xdist, levels = c("ANOVA1", "normal", "t1"), labels = c("X ~ ANOVA", "X ~ i.i.d. Normal", "X ~ i.i.d. Cauchy"))) %>%
ggplot(aes(x = rounds, y = value)) +
    geom_line(aes(color = algo, linetype = algo)) +
    facet_grid( ~ Xdist, scales = "free_y") +
    xlab("Round (# Samples / 1000)") +
    ylab("Normalized O*(X)") +
    ## ggtitle("Comparison Between Genetic Algorithm and Naive Stochastic Search (n = 1000)") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          panel.grid = element_blank())

ggsave(filename = "../figs/GA_SS.pdf", GA_SS_plot, width = 7, height = 3)
