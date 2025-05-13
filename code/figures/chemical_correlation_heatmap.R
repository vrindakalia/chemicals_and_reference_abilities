#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Correlation plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
source("code/packages/packages_to_load.R")

chem_class <- read_tsv("data/chemical_groups.txt") %>% 
    arrange(source)

chem_class_updated1 <- chem_class %>% 
    select(chemical, source_1 = source) %>% 
    mutate(source_1 = factor(source_1, 
                                     levels = c("agricultural", "food", "household dust", "industrial",
                                                "legacy", "legacy PCB", "other"),
                                     labels = c("Farming/\nGreen space", "Food", "Indoor dust", "Industrial \nuse",
                                                "Legacy", "Legacy PCB", "other")))

chem_class_updated2 <- chem_class %>% 
    select(chemical, source_2 = source) %>% 
    mutate(source_2 = factor(source_2, 
                                     levels = c("agricultural", "food", "household dust", "industrial",
                                                "legacy", "legacy PCB", "other"),
                                     labels = c("Farming/\nGreen space", "Food", "Indoor dust", "Industrial \nuse",
                                                "Legacy", "Legacy PCB", "other")))

# chemical
chemicals <- read_tsv("data/chemicals_replaced_filtered_transformed.txt") %>% 
    select(-ysad)

chem.cor <- cor(chemicals, method = "spear") %>% 
    round(., 2)

p.mat <- cor_pmat(chemicals)

# Adding facet to show group membership
chem.cor.long <- chem.cor %>% 
    as.data.frame(.) %>% 
    rownames_to_column(var = "Var1") %>% 
    gather(key = "Var2", value = "correlation", -Var1) %>% 
    merge(., chem_class_updated1, by.y = "chemical", by.x = "Var1") %>% 
    merge(., chem_class_updated2, by.y = "chemical", by.x = "Var2") %>% 
    mutate(source_2 = fct_rev(source_2))

cor_heatmap <- chem.cor.long %>% 
    ggplot(aes(x = Var1, y = Var2, fill = correlation)) +
    geom_tile(color = "white") +
    theme_bw() +
    labs(x = "", y = "", fill = "Correlation") +
    facet_grid(source_2~source_1, scales = "free", space = "free") +
    scale_fill_gradient2(high = "royalblue3", mid = "white", low = "coral") +
    scale_x_discrete(expand = c(0, 0)) + 
    scale_y_discrete(expand = c(0, 0)) + 
    theme_bw(base_size = 18) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_blank(),
          axis.ticks.y =element_blank(),
          panel.spacing = unit(0, "lines"),
          panel.border = element_rect(colour = "black", size=0.7),
          strip.placement = "outside", 
          strip.background = element_rect(fill = "white"),
          plot.margin = margin(0.25,0,0.25,-0.5, "cm")) 

png("figures/chemical_correlations_by_source.png", res = 300, units = "in",
    h = 19, w = 22)
cor_heatmap
dev.off()


