#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run GEE models
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("code/functions/function_fit_gee.R")
source("code/packages/packages_to_load.R")

# Call in the data
outcome_meta <- read_tsv("data/outcome_and_meta_data.txt")

chemical_data <- read_tsv("data/chemicals_replaced_filtered_transformed.txt") %>% 
    clean_names(.)
chemical_names_original <- read_tsv("data/chemicals_replaced_filtered_transformed.txt") %>% 
    names(.) %>% .[-1]
chemical_names_clean <- names(chemical_data)[-1]
chemical_names_key <- cbind(chemical_names_original, chemical_names_clean)
chemical_names_key %>% data.frame() %>% write_tsv("data/chemical_names_key.txt")
chemical_meta_data <- read_tsv("data/targeted_assay_meta_data.txt") %>% 
    clean_names(.)

chemical_names <- names(chemical_data)[-1]
covariate_names <- c("age_eval", "female", "edu_min", "race", "gc_batch", "sub_id", "apoe_e4")

# Data combined 
all_data <- merge(outcome_meta, chemical_data, by = "ysad") %>% 
    merge(., chemical_meta_data, by = "ysad")

# Set up to run GEEs
chemicals <- select(all_data, all_of(chemical_names))
covariates <- select(all_data, all_of(covariate_names))

# Outcome = Memory
memory <- select(all_data, memory)
memory_gee <- run_gee(outcome = memory, chemicals = chemicals, covariates = covariates)
memory_gee_results <- summarise_gee(memory_gee, outcome = "Memory")

# Outcome = Speed
speed <- select(all_data, speed)
speed_gee <- run_gee(outcome = speed, chemicals = chemicals, covariates = covariates)
speed_gee_results <- summarise_gee(speed_gee, outcome = "Speed")

# Outcome = Reasoning
reasoning <- select(all_data, reasoning)
reasoning_gee <- run_gee(outcome = reasoning, chemicals = chemicals, covariates = covariates)
reasoning_gee_results <- summarise_gee(reasoning_gee, outcome = "Reasoning")

# Outcome = Vocabulary
vocab <- select(all_data, vocab)
vocab_gee <- run_gee(outcome = vocab, chemicals = chemicals, covariates = covariates)
vocab_gee_results <- summarise_gee(vocab_gee, outcome = "Vocabulary")

# Outcome = Global Score
global_score <- select(all_data, global_score)
global_score_gee <- run_gee(outcome = global_score, chemicals = chemicals, covariates = covariates)
global_score_gee_results <- summarise_gee(global_score_gee, outcome = "Global score")

all_gee_results <- do.call(rbind, mget(ls(pattern="_gee_results")))

all_gee_results %>% write_tsv("code/output/gee_all_outcome.txt")
