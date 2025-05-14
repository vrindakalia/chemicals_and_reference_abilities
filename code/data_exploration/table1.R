# Table 1
meta_data <- read_tsv("data/outcome_and_meta_data.txt")
chemical_data <- read_tsv("data/chemicals_replaced_filtered_transformed.txt")

chemical_and_meta_data <- merge(meta_data, chemical_data, by =)

label(chemical_and_meta_data$age_eval) <- "Age"
units(chemical_and_meta_data$age_eval) <- "years"

label(chemical_and_meta_data$education) <- "Education"
units(chemical_and_meta_data$education) <- "years"

chemical_and_meta_data$race <- factor(chemical_and_meta_data$race, levels = c(1,2,3),
                                      labels = c("White", "African American", "Other"))
label(chemical_and_meta_data$race) <- "Race"

chemical_and_meta_data$female <- factor(chemical_and_meta_data$female, levels = c(0,1), labels = c("Male", "Female"))
label(chemical_and_meta_data$female) <- "Sex"

chemical_and_meta_data$apoe_e4 <- factor(chemical_and_meta_data$apoe_e4, levels = c(0,1), labels = c("No e4 allele", "At least one e4 allele"))
label(chemical_and_meta_data$apoe_e4 ) <- "APOE-e4 allele"

label(chemical_and_meta_data$global_score) <- "Global score"
label(chemical_and_meta_data$memory) <- "Memory"
label(chemical_and_meta_data$vocab) <- "Vocabulary"
label(chemical_and_meta_data$speed) <- "Processing speed"
label(chemical_and_meta_data$reasoning) <- "Fluid reasoning"

chemical_and_meta_data$bl0fu1 <- factor(chemical_and_meta_data$bl0fu1, levels = c(0,1), labels = c("Visit 1", "Visit 2"))

table1(~age_eval + education + race + female + apoe_e4 + global_score +
                memory + vocab + reasoning + speed | bl0fu1, data = chemical_and_meta_data)

