# Forest plot of GEE results
gee_results <- read_tsv("code/output/gee_all_outcome.txt")

gee_results <- gee_results %>% 
    mutate(low.ci = (coefficient - 1.96*robust.se)) %>% 
    mutate(up.ci = (coefficient + 1.96*robust.se)) 

chemical_names_key <- read_tsv("data/chemical_names_key.txt")
chemical_groups <- read_tsv("data/chemical_groups.txt")
chemical_names_and_groups <- merge(chemical_names_key, chemical_groups, by.x = "chemical_names_original", by.y = "chemical")

gee_results_with_chemical_data <- merge(gee_results, chemical_names_and_groups, by.x = "chemical", by.y = "chemical_names_clean") %>% 
    mutate(source_updated = factor(source, 
                                   levels = c("agricultural", "food", "household dust", "industrial",
                                              "legacy", "legacy PCB", "other"),
                                   labels = c("Farming/\nGreen space", "Food", "Indoor dust", "Industrial \nuse",
                                              "Legacy", "Legacy PCB", "other"))) %>% 
    mutate(outcome_labels = case_when(outcome == "Memory" ~ "Episodic memory",
                                      outcome == "Reasoning" ~ "Fluid reasoning",
                                      outcome == "Speed" ~ "Perceptual speed",
                                      outcome == "Vocabulary" ~ "Vocabulary",
                                      outcome == "Global score" ~ "Global score"))
gee_results_with_chemical_data$outcome_labels <- factor(gee_results_with_chemical_data$outcome_labels, levels = c("Global score",
                                                                                                                  "Episodic memory",
                                                                                                                  "Fluid reasoning",
                                                                                                                  "Perceptual speed",
                                                                                                                  "Vocabulary"))
png("figures/supplemental_gee_forest_plot.png", res = 300, units = "in",
    h = 10, w = 10)
gee_results_with_chemical_data %>%  
    ggplot(aes(y = chemical_names_original, x = coefficient)) +
    geom_pointrange(aes(xmin = low.ci, xmax = up.ci), color = "grey56") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    theme_bw() +
    labs(x = "Coefficient", y = "", color = "") +
    facet_grid(source_updated~outcome_labels, scales = "free", space = "free") +
    theme(panel.border = element_rect(colour = "black", size=0.7),
          strip.placement = "outside", 
          strip.background = element_rect(fill = "white")) 
dev.off()
