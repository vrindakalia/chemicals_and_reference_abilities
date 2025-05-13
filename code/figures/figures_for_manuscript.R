
source("code/packages/packages_to_load.R")

# Figure 1
source("code/figures/chemical_concentration_boxplot.R")
source("code/figures/chemical_correlation_heatmap.R")

png("figures/Figure1_concentration_and_correlations.png", res = 300, units = "in", h = 18, w = 24)
plot_grid(concentration_boxplot, cor_heatmap, rel_widths = c(0.35, 1), nrow = 1, ncol = 2)
dev.off()

# BKMR results: Figure 2: only global score
source("code/figures/global_score_plots.R")

left <- plot_grid(overall_risk, global_score_cond_pips, nrow = 2, rel_heights = c(0.2, 1),
                  labels = c("A", "B"))

png("figures/Figure2_global_score.png", res = 300, units = "in",
    h = 12, w = 16)
plot_grid(left, response, nrow = 1, rel_widths = c(0.45, 1), labels = c("", "C"))
dev.off()

# BKMR results: Figure 3: PIPs for all outcomes
source("code/figures/dotplot_pips_all_outcomes.R")
png("figures/Figure3_PIPs_all_reference_abilities.png", res = 300, units = "in",
    h = 8, w = 12)
pips_graph
dev.off()
