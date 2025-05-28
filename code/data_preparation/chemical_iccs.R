#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ICC for chemicals and outcome for baseline and followup data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("code/packages/packages_to_load.R")

# call in the data
# pheno
phenotypic_data <- read_sav("data/CRRANN_Exposome_352_DEMsel.sav") %>% 
    rename(SampleName = SampleName3)

# chemical
chemicals <- read_tsv("data/chemical_lod_replaced_with_NA.txt")

data <- merge(chemicals, phenotypic_data, by = "YSAD")

# Calculate chemical ICCs ----
chemical_names <- names(chemicals[-1])

chemicals_for_icc <- data %>% 
    select(ID = SubID, BL0FU1, all_of(chemical_names)) %>% 
    janitor::clean_names()

chemicals_df_names <- names(chemicals_for_icc)[-c(1:2)]

icc_chemical <- map(chemicals_df_names,
                        function(x){
                            formula_mlm = as.formula(paste0(x,"~ (1|id)"));
                            model_fit = lmer(formula_mlm,data=chemicals_for_icc,
                                             na.action = na.omit);
                            icc = icc(model_fit);
                            return(icc)
                        }) 
 
icc_adjusted <- sapply(icc_chemical,"[[", 1) %>% 
    data.frame() %>% 
    rename(icc_adjusted = 1) %>% 
    mutate(chemical = chemicals_df_names)


    