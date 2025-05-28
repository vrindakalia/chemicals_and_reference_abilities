# Supplemental figures
# 1. BKMR figures for supplement: 
## a. overall risk plots for all RA domains
memory_overall_risk <- ggdraw() +
    draw_image("figures/bkmr/memory_bkmr_overall_risk.png", scale = 1)
reasoning_overall_risk <- ggdraw() +
    draw_image("figures/bkmr/reasoning_bkmr_overall_risk.png", scale = 1) 
speed_overall_risk <- ggdraw() +
    draw_image("figures/bkmr/speed_bkmr_overall_risk.png", scale = 1) 
vocab_overall_risk <- ggdraw() +
    draw_image("figures/bkmr/vocabulary_bkmr_overall_risk.png", scale = 1) 

png("figures/supplemental_overall_risk_all_RAs.png", res = 300, units = "in",
    h = 6, w = 8)
plot_grid(memory_overall_risk,
          reasoning_overall_risk,
          speed_overall_risk,
          vocab_overall_risk, labels = c("A", "B", "C", "D"), nrow = 2)
dev.off()

## b. Traceplots for beta for all five outcomes
global_beta <- ggdraw() +
    draw_image("figures/bkmr/global_score_bkmr_traceplots_beta.png", scale = 1)
memory_beta <- ggdraw() +
    draw_image("figures/bkmr/memory_bkmr_traceplots_beta.png", scale = 1)
reasoning_beta <- ggdraw() +
    draw_image("figures/bkmr/reasoning_bkmr_traceplots_beta.png", scale = 1) 
speed_beta <- ggdraw() +
    draw_image("figures/bkmr/speed_bkmr_traceplots_beta.png", scale = 1) 
vocab_beta <- ggdraw() +
    draw_image("figures/bkmr/vocabulary_bkmr_traceplots_beta.png", scale = 1) 

png("figures/supplemental_beta1_traceplots_all_RAs.png", res = 300, units = "in",
    h = 9, w = 6)
plot_grid(global_beta,
          memory_beta,
          reasoning_beta,
          speed_beta,
          vocab_beta, labels = c("A", "B", "C", "D", "E"), nrow = 5)
dev.off()

## c. Traceplots for sigmasq for all five outcomes
global_sigsq <- ggdraw() +
    draw_image("figures/bkmr/global_score_bkmr_traceplots_sigsq.png", scale = 1)
memory_sigsq <- ggdraw() +
    draw_image("figures/bkmr/memory_bkmr_traceplots_sigsq.png", scale = 1)
reasoning_sigsq <- ggdraw() +
    draw_image("figures/bkmr/reasoning_bkmr_traceplots_sigsq.png", scale = 1) 
speed_sigsq <- ggdraw() +
    draw_image("figures/bkmr/speed_bkmr_traceplots_sigsq.png", scale = 1) 
vocab_sigsq <- ggdraw() +
    draw_image("figures/bkmr/vocabulary_bkmr_traceplots_sigsq.png", scale = 1) 

png("figures/supplemental_sigsq_traceplots_all_RAs.png", res = 300, units = "in",
    h = 9, w = 6)
plot_grid(global_sigsq,
    memory_sigsq,
    reasoning_sigsq,
    speed_sigsq,
    vocab_sigsq, labels = c("A", "B", "C", "D", "E"), nrow = 5)
dev.off()

