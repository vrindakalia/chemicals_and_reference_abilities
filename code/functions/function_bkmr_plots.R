# Script to plot bkmr figures

# Function
bkmr_plots <- function(outcome_name = "", iter_num){
    load(paste0("code/output/bkmr_hierarchical_outcome_", outcome_name,".RData"))
    load(paste0("code/output/bkmr_hierarchical_outcome_", outcome_name,"_for_plots.RData"))
    
    if(outcome_name == "memory"){
        title = "Episodic memory"
    } else if(outcome_name == "reasoning"){
        title = "Fluid reasoning"
    } else if(outcome_name == "speed"){
        title = "Perceptual speed"
    } else if(outcome_name == "vocabulary"){
        title = "Vocabulary"
    } else {
        title = "Global score"
    }
    
    # Remove burn-in
    sel <- seq(5001, iter_num, by=1)
    
    # Trace plot - betas
    png(paste0("figures/bkmr/", outcome_name, "_bkmr_traceplots_beta.png"), 
        res = 300, units = "in", width=6, height=3)
    
    TracePlot(fit = fit, par = "beta", sel=sel, main = paste0(title, ": beta1"))
    
    dev.off()
    
    message("Traceplots betas saved")
    
    # Trace plot - sigma squared
    png(paste0("figures/bkmr/", outcome_name, "_bkmr_traceplots_sigsq.png"), 
        res = 300, units = "in", width=6, height=3)
    
    TracePlot(fit = fit, par = "sigsq.eps", sel=sel, , main = paste0(title, ": sigma sq"))
    
    dev.off()
    
    message("Traceplots sigmasq saved")
    
    # Change labels for conditional PIPs plot
    pips_labels <- pips %>% 
        mutate(label = case_when(group == 1 ~ "Farming/\nGreen space",
                                 group == 2 ~ "Food",
                                 group == 3 ~ "Indoor dust",
                                 group == 4 ~ "Industrial use",
                                 group == 5 ~ "Legacy",
                                 group == 6 ~ "Legacy PCBs")) %>% 
        mutate(label_gpip = paste0(label, "\n Group PIP = ", round(groupPIP,2))) %>% 
        mutate(outcome = outcome_name)
    
    png(paste0("figures/bkmr/", outcome_name, "_bkmr_condPIP.png"), 
        res = 300, units = "in", width=10, height=8)
    
    print(pips_labels %>% 
        ggplot(aes(x = condPIP, y = reorder(variable, condPIP))) +
        geom_point() +
        theme_bw() +
        facet_wrap(~label_gpip, scales = "free") +
        labs(x = "Conditional Posterior Inclusion Probability", y = ""))
    
    dev.off()
    
    message("CondPIPs saved")
    
    # Over all risk plot
    png(paste0("figures/bkmr/", outcome_name, "_bkmr_overall_risk.png"), 
        res = 300, units = "in", width=4, height=3)
    
    print(ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) +  
        geom_hline(yintercept=00, linetype="dashed", color="gray")+ 
        geom_pointrange() + scale_y_continuous(name="estimate") + theme_bw() +
            ggtitle(title))
    
    dev.off()
    
    message("Overall risk plot saved")
    
    # Predicted univariate responses
    png(paste0("figures/bkmr/", outcome_name, "_bkmr_univariate_response.png"), 
        res = 300, units = "in", width=12, height=10)
    
    print(pred.resp.univar %>% 
        merge(., chemical_groups, by = "variable") %>% 
        arrange(source) %>% 
        mutate(label = case_when(source == "agricultural" ~ "Farming/\nGreen space",
                                 source == "food" ~ "Food",
                                 source == "household dust" ~ "Indoor dust",
                                 source == "industrial" ~ "Industrial use",
                                 source == "legacy" ~ "Legacy",
                                 source == "legacy PCB" ~ "Legacy PCBs")) %>% 
        ggplot(aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se, color = label)) + 
        geom_smooth(stat = "identity") + ylab("estimate") + facet_wrap(~ chemical_plot_labels, ncol = 6) +
        geom_hline(yintercept=00, linetype="dashed", color="black") +
        theme_bw()+
        theme(legend.position = "right") +
        labs(color = "") +
        scale_color_manual(values = c("#4363d8", "#f58231", "#808000",
                                      "#800000", "#469990", "#000075")))
    
    dev.off()
    
    message("Univariate response plot saved")
    
    message("Thats all the plots!")
    
}

# Function to get PIPs 
get_pips <- function(outcome_name = ""){
    
    load(paste0("code/output/bkmr_hierarchical_outcome_", outcome_name,"_for_plots.RData"))
    
    pips_labels <- pips %>% 
        mutate(label = case_when(group == 1 ~ "Farming/\nGreen space",
                                 group == 2 ~ "Food",
                                 group == 3 ~ "Indoor dust",
                                 group == 4 ~ "Industrial use",
                                 group == 5 ~ "Legacy",
                                 group == 6 ~ "Legacy PCBs")) %>% 
        mutate(label_gpip = paste0(label, "\n Group PIP = ", round(groupPIP,2))) %>% 
        mutate(outcome = outcome_name)
    
    return(pips_labels)
}