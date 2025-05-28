# Table 1
# Get outcome and exposure data
meta_data <- read_tsv("data/outcome_and_meta_data.txt")
chemical_data <- read_tsv("data/chemicals_replaced_filtered_transformed.txt")

# Merge the outcome and exposure data; the complete dataset
chemical_and_meta_data <- merge(meta_data, chemical_data, by = "ysad") %>% 
    filter(!is.na(apoe_e4))

# Labels and units for age
label(chemical_and_meta_data$age_eval) <- "Age"
units(chemical_and_meta_data$age_eval) <- "years"
# Labels and units for education
label(chemical_and_meta_data$education) <- "Education"
units(chemical_and_meta_data$education) <- "years"
# Labels for race
chemical_and_meta_data$race <- factor(chemical_and_meta_data$race, levels = c(1,2,3),
                                      labels = c("White", "African American", "Other"))
label(chemical_and_meta_data$race) <- "Race"
# Labels for sex
chemical_and_meta_data$female <- factor(chemical_and_meta_data$female, levels = c(0,1), labels = c("Male", "Female"))
label(chemical_and_meta_data$female) <- "Sex"
# Labels for apoe-e4
chemical_and_meta_data$apoe_e4 <- factor(chemical_and_meta_data$apoe_e4, levels = c(0,1), labels = c("No e4 allele", "At least one e4 allele"))
label(chemical_and_meta_data$apoe_e4 ) <- "APOE-e4 allele"
# Labels for global score
label(chemical_and_meta_data$global_score) <- "Global score"
# Labels for memory
label(chemical_and_meta_data$memory) <- "Episodic memory"
# Labels for vocabulary
label(chemical_and_meta_data$vocab) <- "Vocabulary"
# Labels for processing speed
label(chemical_and_meta_data$speed) <- "Perceptual speed"
# Labels for reasoning
label(chemical_and_meta_data$reasoning) <- "Fluid reasoning"
# Labels for visit number
chemical_and_meta_data$bl0fu1 <- factor(chemical_and_meta_data$bl0fu1, levels = c(0,1), labels = c("Visit 0", "Visit 1"))

# Generate table 1
table1(~age_eval + education + race + female + apoe_e4 + global_score +
                memory + vocab + reasoning + speed | bl0fu1, data = chemical_and_meta_data)

