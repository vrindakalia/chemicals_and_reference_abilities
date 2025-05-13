source("code/packages/packages_to_load.R")
source("code/functions/function_bkmr_plots.R")

# Get PIPs

gcpip_mem <- get_pips(outcome_name = "memory")
gcpip_vocab <- get_pips(outcome_name = "vocabulary")
gcpip_reasoning <- get_pips(outcome_name = "reasoning")
gcpip_speed <- get_pips(outcome_name = "speed")

all_pip <- do.call(rbind, mget(ls(pattern="gcpip_"))) %>% 
    select(label, PIP = condPIP, outcome, groupPIP, variable) %>% 
    mutate(shape = "condPIP")

group_pip <- all_pip %>% 
    select(label, PIP = groupPIP, outcome) %>% 
    distinct(.keep_all = TRUE) %>% 
    mutate(shape = "groupPIP") %>% 
    mutate(variable = "Group")

pip_plot <- all_pip %>% 
    select(label, PIP, outcome, shape, variable) %>% 
    rbind(., group_pip) %>% 
    arrange(desc(shape))

levels(reorder(pip_plot$variable, pip_plot$PIP, median))

pip_plot$variable_fct <- factor(pip_plot$variable, levels = c("PCB 49",                            
                                                              "PCB 18",                            
                                                              "PCB 81",                            
                                                              "PCB 44",                            
                                                              "p,p'-DDD",                          
                                                              "PCB 128",                           
                                                              "PCB 77",                            
                                                              "p,p'-DDE",                          
                                                              "Ethion",                            
                                                              "PCB 33",                            
                                                              "Benz[a]anthracene",                 
                                                              "b-Hexachlorocyclohexane",           
                                                              "Endrin",                            
                                                              "d-Lindane",                         
                                                              "PCB 52",                            
                                                              "Fonofos",                           
                                                              "Chrysene",                          
                                                              "Mirex",                             
                                                              "2,4,5-Trichlorophenol",             
                                                              "2,3,7,8-Tetrachlorodibenzofuran",   
                                                              "Parathion",                         
                                                              "Benzo[b]fluoranthene",              
                                                              "1,2,3,7,8-Pentachlorodibenzodioxin",
                                                              "Pyrene",                            
                                                              "1,2-Dichlorbenzene",                
                                                              "Phenanthrene",                      
                                                              "Fluoranthene",                      
                                                              "Anthracene",                        
                                                              "Isosafrole",                        
                                                              "Tri(2-chloroethyl) phosphate",      
                                                              "cis-Permethrin",                    
                                                              "Diethyl Phthalate",                 
                                                              "1,2,4,5-Tetrachlorobenzene",        
                                                              "Safrole",                           
                                                              "Bromacil",                          
                                                              "DEET",                              
                                                              "PCB 203",                           
                                                              "trans-Permethrin",                  
                                                              "Lindane",                           
                                                              "Etridiazole",                       
                                                              "Malathion",                         
                                                              "1,2,4-Trichlorobenzene",            
                                                              "Acenaphthene",                      
                                                              "Simazine",                          
                                                              "Triadimefon",                       
                                                              "Hexachlorbutadiene",                
                                                              "PCB 60",
                                                              "Group"))    



set.seed(2023)
pips_graph <- pip_plot %>% 
    ggplot(aes(x = PIP, y = variable_fct, color = factor(outcome), shape = factor(shape), alpha = factor(shape))) +
    geom_point(data = filter(pip_plot, shape == "condPIP"), size = 2.5) +
    geom_jitter(data = filter(pip_plot, shape == "groupPIP"), height = 0.25, size = 2.5) +
    facet_wrap(~label, scales = "free_y", nrow = 2) +
    xlim(0,1) +
    labs(color = "Outcome", y = "", shape = "PIP", x = "Posterior Inclusion Probability (PIP)") +
    theme_bw(base_size = 13) +
    theme(legend.position = "bottom",
          panel.border = element_rect(colour = "black", size=0.7),
          strip.placement = "outside", 
          strip.background = element_rect(fill = "white")) +
    guides(alpha = "none") +
    scale_color_manual(values = c("#0B8EF8FF", "orange", "#681A15FF",  "#DC5750FF"),
                       labels = c(c("Memory", "Reasoning", "Speed", "Vocabulary"))) +
    scale_alpha_manual(values = c(0.6, 0.8)) +
    scale_shape_manual(values = c(20, 15), labels = c("Conditional PIP", "Group PIP")) +
    guides(color = guide_legend(ncol = 2),
           shape = guide_legend(ncol = 1))



