# Run GEE for cognition

run_gee <- function(outcome, chemicals, covariates){
    
    # Get information to set up for loop
    chemical_names <- names(chemicals)
    number_of_chemicals <- length(chemical_names)
    
    # Name of outcome
    outcome_name <- names(outcome)
    
    # List to hold model output
    list_of_results <- vector(length = number_of_chemicals, mode = "list")
    names(list_of_results) <- chemical_names
    
    for(i in chemical_names){
        tmp <- cbind(outcome, chemicals[i], covariates)
        fml <- as.formula(paste(outcome_name, "~", i, "+ age_eval + edu_min + factor(female) + factor(race) + factor(gc_batch) + factor(apoe_e4)"))
        
        # save model output
        model_result <- gee(fml, data = tmp,  id = sub_id, 
                    na.action = na.omit,
                    corstr="exchangeable") 
        
        list_of_results[[i]] <- model_result
    }
    
    return(list_of_results)
}

summarise_gee <- function(list_of_results, outcome_name = ""){
    
    number_of_predictors <- length(list_of_results)
    gee_results <- NULL
    gee_results <- data.frame(matrix(NA, nrow = number_of_predictors, ncol = 5))
    
    for(i in 1:number_of_predictors){
        gee_results[i,1] <- names(list_of_results[i])
        gee_results[i,2] <- coef(list_of_results[[i]])[2]
        #memory_gee_results[i,3] <- coef(memory_gee[[i]])[2]
        gee_results[i,3] <- 2 * pnorm(abs(coef(summary(list_of_results[[i]]))[2,5]), lower.tail = FALSE) 
        gee_results[i,4] <- list_of_results[[i]]$working.correlation[1,2]
        gee_results[i,5] <- coef(summary(list_of_results[[i]]))[2,4]
    }
    
    names(gee_results) <- c("chemical", "coefficient", "pvalue", "working_corr", "robust.se")
    gee_results <- gee_results %>% 
        mutate(fdr = p.adjust(pvalue, method = "BH")) %>% 
        mutate(outcome = outcome_name)
    
    return(gee_results)
}

