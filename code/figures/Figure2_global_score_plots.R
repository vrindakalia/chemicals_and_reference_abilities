source("code/packages/packages_to_load.R")

load(paste0("code/output/bkmr_hierarchical_outcome_", "global_score",".RData"))
load(paste0("code/output/bkmr_hierarchical_outcome_", "global_score","_for_plots.RData"))

# Change labels to better fit plots
chemical_labels_to_change <- c("1,2,3,7,8-Pentachlorodibenzodioxin", "2,3,7,8-Tetrachlorodibenzofuran", "Tri(2-chloroethyl) phosphate",
                               "1,2,4,5-Tetrachlorobenzene")

chemical_groups <- read_tsv("data/chemical_groups.txt") %>% 
    filter(source != "other") %>% 
    arrange(source) %>% 
    select(variable = chemical, source) %>% 
    mutate(change = ifelse(variable %in% chemical_labels_to_change, 1, 0)) %>% 
    mutate(label = case_when(variable == "1,2,3,7,8-Pentachlorodibenzodioxin" ~ "1,2,3,7,8-\nPentachlorodibenzodioxin",
                             variable == "2,3,7,8-Tetrachlorodibenzofuran" ~ "2,3,7,8-\nTetrachlorodibenzofuran",
                             variable == "Tri(2-chloroethyl) phosphate"~ "Tri(2-chloroethyl) \nphosphate",
                             variable == "1,2,4,5-Tetrachlorobenzene"~ "1,2,4,5-\nTetrachlorobenzene",
                             change == 0 ~ variable)) %>% 
    mutate(chemical_plot_labels = factor(label, levels = label)) 


overall_risk <- ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) +  
    geom_hline(yintercept = 00, linetype="dashed", color = "gray")+ 
    geom_pointrange() + 
    scale_y_continuous(name = "Estimate") + 
    scale_x_continuous(name = "Total Chemical Exposure Quantile") + 
    theme_bw()


response <- pred.resp.univar %>% 
    merge(., chemical_groups, by = "variable") %>% 
    arrange(source) %>% 
    mutate(label = case_when(source == "agricultural" ~ "Farming/\nGreen space",
                                   source == "food" ~ "Food",
                                   source == "household dust" ~ "Indoor dust",
                                   source == "industrial" ~ "Industrial use",
                                   source == "legacy" ~ "Legacy",
                                   source == "legacy PCB" ~ "Legacy PCBs")) %>% 
    ggplot(aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se, color = label)) + 
    geom_smooth(stat = "identity") + 
    labs(x = "Log2 Chemical Concentration Z-score", y = "Estimate") + 
    facet_wrap(~ chemical_plot_labels, ncol = 6) +
    geom_hline(yintercept=00, linetype="dashed", color="black") +
    theme_bw()+
    theme(legend.position = "right",
          panel.border = element_rect(colour = "black", size=0.7),
          strip.placement = "outside", 
          strip.background = element_rect(fill = "white")) +
          labs(color = "") +
    scale_color_manual(values = c("#4363d8", "#f58231", "#808000",
                                  "#800000", "#469990", "#000075"))


# Change labels for conditional PIPs plot
pips_labels <- pips %>% 
    mutate(label = case_when(group == 1 ~ "Farming/\n Green space",
                             group == 2 ~ "Food",
                             group == 3 ~ "Indoor dust",
                             group == 4 ~ "Industrial use",
                             group == 5 ~ "Legacy",
                             group == 6 ~ "Legacy PCBs")) %>% 
    mutate(label_gpip = paste0(label, "\n Group PIP = ", round(groupPIP,2))) %>% 
    mutate(outcome = "Global score")


global_score_cond_pips <- pips_labels %>% 
    ggplot(aes(x = condPIP, y = reorder(variable, condPIP), color = factor(label_gpip))) + 
    geom_point() +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black", size=0.7),
          strip.placement = "outside", 
          strip.background = element_rect(fill = "white"),
          strip.text.y = element_text(angle = 0),
          legend.position = "none") +
    facet_grid(rows = vars(label_gpip), scales = "free", space = "free") +
    labs(x = "Conditional Posterior Inclusion Probability (PIP)", y = "") +
    scale_color_manual(values = c("#4363d8", "#f58231", "#808000",
                                  "#800000", "#469990", "#000075")) +
    xlim(0,1)


