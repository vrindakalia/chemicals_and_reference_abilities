# Chemical concentration distribution

# Create a plot that shows the distribution of chemical 
# concentrations grouped by chemical source 
source("code/packages/packages_to_load.R")

chemicals <- read_tsv("data/chemical_lod_replaced_with_NA.txt") %>% 
    rename(ysad = YSAD)
chemical_group <- read_tsv("data/chemical_groups.txt") %>% 
    mutate(source_updated = factor(source, 
                             levels = c("agricultural", "food", "household dust", "industrial",
                                        "legacy", "legacy PCB", "other"),
                             labels = c("Farming/\nGreen space", "Food", "Indoor dust", "Industrial \nuse",
                                        "Legacy", "Legacy PCB", "other")))
meta_data <- read_tsv("data/outcome_and_meta_data.txt")

chemicals_long <- chemicals %>% 
    gather(key = "chemical", value = "concentration", -ysad) %>% 
    merge(., chemical_group, by = "chemical") %>% 
    merge(., meta_data, by = "ysad") %>% 
    mutate(source_updated = fct_rev(source_updated))

concentration_boxplot <- chemicals_long %>% 
    filter(!is.na(concentration)) %>% 
    ggplot(aes(y = chemical, x = log2(concentration), fill = factor(bl0fu1), color = factor(bl0fu1))) +
    #annotate(geom = "rect", xmin = 0.85, xmax = 0.9, ymin = 0.03, ymax = 0.05, colour = "white", fill = "white") +
    geom_boxplot(alpha = 0.25, width = 0.7) +
    facet_grid(source_updated~., scales = "free", space = "free") +
    theme_bw(base_size = 18) +
    theme(legend.position = "inside",
          legend.position.inside = c(0.87,0.04),
          legend.background = element_rect(fill = NA),
          plot.margin = margin(1.8,-0.015,5.7,0, "cm"),
          panel.spacing = unit(0, "lines"),
          panel.border = element_rect(colour = "black", size=0.7),
          strip.text.y = element_blank() , 
          strip.background = element_blank()) +
    scale_fill_manual(values = c("maroon", "darkgreen")) +
    scale_color_manual(values = c("maroon", "darkgreen")) +
    labs(y = "", x = "Log2(Concentration (ppb))", fill = "Visit", color = "Visit")

png("figures/chemical_concentration_boxplots.png", res = 300, units = "in",
    h = 14, w = 6)
concentration_boxplot
dev.off()
