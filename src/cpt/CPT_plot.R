library("tidyverse")
library("ggplot2")
load("../data/power_CPT_expr.RData")

res <- res %>% mutate(
    methods = recode(methods,
                     `t test` = "t/F",
                     `F test` = "t/F",
                     `Perm. t test` = "Perm",
                     `Perm. F test` = "Perm",
                     `FL test` = "FL",
                     `CPT1` = "CPTw",
                     `CPT2` = "CPTs",
                     `random CPT` = "CPTr",
                     `Group Bound` = "GB"),
    ratio = factor(ratio, levels = c(25, 30, 40),
                   labels = paste0("n/p = ", c(25, 30, 40))))

size_df <- res %>% filter(signal == 0) %>%
    mutate(methods = factor(methods, levels = c("CPTs", "CPTw", "CPTr", "t/F", "Perm", "FL", "LAD", "GB"),
           labels = paste0("M", 1:8)))
power_df <- res %>% filter(signal != 0) %>%
    mutate(methods = factor(methods,
               levels = c("CPTs", "CPTw", "CPTr", "t/F", "Perm", "FL", "LAD", "GB"),
               labels = c("CPTs", "CPTw", "CPTr", "t/F", "Permutation", "Freedman-Lane", "Least Abs. Dev.", "GroupBound")))
power_df_CPT <- power_df %>%
    filter(methods %in% c("CPTs", "CPTw", "CPTr")) %>%
    spread(methods, power)
power_df_others <- power_df %>%
    filter(!methods %in% c("CPTs", "CPTw", "CPTr"))
power_df <- left_join(power_df_others, power_df_CPT,
                      by = c("signal", "Xdist", "epsdist",
                             "ratio", "ninds", "seed")) %>%
            mutate(CPTw = CPTw / power,
                   CPTs = CPTs / power,
                   CPTr = CPTr / power) %>%
            select(-power, -seed) %>%
            gather(CPT, powratio, -signal, -methods, -Xdist,
                   -epsdist, -ratio, -ninds)##  %>%
            ## filter(methods != "GB")

Xdist_list <- c("ANOVA1", "normal", "t1")
epsdist_list <- c("normal", "t1")
ninds_list <- c(1, 5)
size_exprs <- expand.grid(Xdist_list, ninds_list)
power_exprs <- expand.grid(Xdist_list, epsdist_list, ninds_list)

for (i in 1:nrow(size_exprs)){
    Xdist_label <- as.character(size_exprs[i, 1])
    ninds_label <- as.numeric(size_exprs[i, 2])
    size_plot <- size_df %>%
        filter(Xdist == Xdist_label,
               ninds == ninds_label) %>%
        mutate(epsdist = recode(epsdist, normal = "Normal errors", t1 = "Cauchy errors")) %>%
        ggplot(aes(x = methods, y = power)) +
        geom_boxplot() +
        facet_grid(epsdist ~ ratio) +
        geom_hline(yintercept = 0.05, color = "red") +
        xlab("Methods") + ylab("Type-I error") +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "none",              
              axis.title = element_text(size = 15),
              axis.text = element_text(size = 9),
              strip.text = element_text(size = 15))


    ggsave(filename = paste0("../figs/size_", Xdist_label,
               "_", ninds_label, "_Biometrika.pdf"),
           size_plot, width = 9.5, height = 4.5)
}

for (i in 1:nrow(power_exprs)){
    Xdist_label <- as.character(power_exprs[i, 1])
    epsdist_label <- as.character(power_exprs[i, 2])
    ninds_label <- as.numeric(power_exprs[i, 3])

    power_plot <- power_df %>%
        filter(Xdist == Xdist_label,
               epsdist == epsdist_label,
               ninds == ninds_label) %>%
        group_by(signal, methods, ratio, CPT) %>%
        summarize(low = quantile(powratio, 0.25),
                  high = quantile(powratio, 0.75),
                  med = quantile(powratio, 0.5)) %>%
        ungroup() %>%
        ggplot(aes(x = signal, y = med,
                   color = CPT, linetype = CPT)) +
        geom_point(aes(shape = CPT)) + geom_line() + 
        ## geom_ribbon(aes(ymin = low, ymax = high),
        ##             linetype = 3, alpha = 0.1) +
        ## ggplot(aes(x = signal, y = powratio, color = CPT)) +
        ## geom_smooth(alpha = 0.75) + 
        facet_grid(methods ~ ratio, scales = "free_y") +
        geom_hline(yintercept = 1, color = "black") +
        scale_color_manual(name = "Type of CPT",
                           values = c("CPTs" = "red",
                                      "CPTw" = "blue",
                                      "CPTr" = "#FF8C00")) + 
        scale_linetype_manual(name = "Type of CPT",
                              values = c("CPTs" = "solid",
                                         "CPTw" = "longdash",
                                         "CPTr" = "dotted")) +
        xlab("Relative Signal-to-noise Ratio") +
        ylab("Ratio of power") + 
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "none",
              axis.title = element_text(size = 15),
              axis.text = element_text(size = 10),
              strip.text = element_text(size = 12.5))

    ggsave(filename = paste0("../figs/power_", Xdist_label,
               "_", epsdist_label, "_", ninds_label,
               "_Biometrika.pdf"),
           power_plot, width = 7.5, height = 8.5)
}
