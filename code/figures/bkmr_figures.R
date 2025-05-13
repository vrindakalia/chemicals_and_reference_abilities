# BKMR output as figures

source("code/packages/packages_to_load.R")
source("code/functions/function_bkmr_plots.R")

# Change labels to better fit plots
chemical_labels_to_change <- c("1,2,3,7,8-Pentachlorodibenzodioxin", "2,3,7,8-Tetrachlorodibenzofuran", "Tri(2-chloroethyl) phosphate")

chemical_groups <- read_tsv("data/chemical_groups.txt") %>% 
    filter(source != "other") %>% 
    arrange(source) %>% 
    select(variable = chemical, source) %>% 
    mutate(change = ifelse(variable %in% chemical_labels_to_change, 1, 0)) %>% 
    mutate(label = case_when(variable == "1,2,3,7,8-Pentachlorodibenzodioxin" ~ "1,2,3,7,8-\nPentachlorodibenzodioxin",
                             variable == "2,3,7,8-Tetrachlorodibenzofuran" ~ "2,3,7,8-\nTetrachlorodibenzofuran",
                             variable == "Tri(2-chloroethyl) phosphate"~ "Tri(2-chloroethyl) \nphosphate",
                             change == 0 ~ variable)) %>% 
    mutate(chemical_plot_labels = factor(label, levels = label)) 


# Generate plots
bkmr_plots(outcome_name = "memory", iter_num = 60000)
bkmr_plots(outcome_name = "vocabulary", iter_num = 60000)
bkmr_plots(outcome_name = "reasoning", iter_num = 60000)
bkmr_plots(outcome_name = "speed", iter_num = 60000)
bkmr_plots(outcome_name = "global_score", iter_num = 60000)

