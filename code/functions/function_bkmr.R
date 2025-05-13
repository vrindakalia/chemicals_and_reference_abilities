# Functions to run BKMR and save data for plots

run_bkmr <- function(outcome, covariates, chemicals, ID, chemical_groups, iter_num, outcome_name = ""){
    
    fit <- kmbayes(y = outcome, Z = chemicals, X = covariates, id = ID, iter = iter_num, 
                   verbose=TRUE, varsel=TRUE, groups = chemical_groups) 
    
    save(fit, file = paste0("code/output/bkmr_hierarchical_outcome_", outcome_name,".RData"))
    
    return(fit)
}

bkmr_get_data_for_plots <- function(outcome_name = "", iter_num){
    
    fit_path <- paste0("code/output/bkmr_hierarchical_outcome_", outcome_name,".RData")
    
    load(fit_path)
    
    sel <- seq(5001, iter_num, by=1)
    
    #### create dataframes for ggplot (this takes a little while to run)
    pips <- ExtractPIPs(fit)
    
    pred.resp.univar <- PredictorResponseUnivar(fit, sel = sel, method = "approx")
    
    risks.overall <- OverallRiskSummaries(fit, qs = seq(0.25, 0.75, by = 0.05), 
                                          q.fixed = 0.5, method = "approx",sel = sel)
    
    risks.singvar <- SingVarRiskSummaries(fit, qs.diff = c(0.25, 0.75),
                                          q.fixed = c(0.25, 0.50, 0.75), method = "approx")
    
    risks.int <- SingVarIntSummaries(fit, qs.diff = c(0.25, 0.75), 
                                     qs.fixed = c(0.25, 0.75))
    
    save(pips, pred.resp.univar, risks.overall, risks.singvar, risks.int, 
         file = paste0("code/output/bkmr_hierarchical_outcome_", outcome_name,"_for_plots.RData"))
    
}
