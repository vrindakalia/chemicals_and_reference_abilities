#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run BKMR models
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("code/functions/function_bkmr.R")
source("code/packages/packages_to_load.R")

# Call in the data
outcome_meta <- read_tsv("data/outcome_and_meta_data.txt")
chemical_data <- read_tsv("data/chemicals_replaced_filtered_transformed.txt")
chemical_meta_data <- read_tsv("data/targeted_assay_meta_data.txt") %>% 
    clean_names(.)

# Chemical groups information
chemical_groups <- read_tsv("data/chemical_groups.txt") %>% 
    filter(source != "other") %>% 
    arrange(source)

# Data combined 
all_data <- merge(outcome_meta, chemical_data, by = "ysad") %>% 
    merge(., chemical_meta_data, by = "ysad") %>% 
    filter(!is.na(apoe_e4))

# Run models
x <- all_data %>% 
    select(age_eval, education, race, female, gc_batch, apoe_e4) %>% 
    mutate(age_scaled = scale(age_eval, center = T)) %>% 
    mutate(edu_scaled = scale(education, center = T)) %>% 
    mutate(batch_2 = ifelse(gc_batch == 2 , 1, 0)) %>% 
    mutate(batch_3 = ifelse(gc_batch == 3 , 1, 0)) %>% 
    mutate(batch_4 = ifelse(gc_batch == 4 , 1, 0)) %>% 
    mutate(batch_5 = ifelse(gc_batch == 5 , 1, 0)) %>% 
    mutate(batch_6 = ifelse(gc_batch == 6 , 1, 0)) %>% 
    mutate(batch_7 = ifelse(gc_batch == 7 , 1, 0)) %>% 
    mutate(batch_8 = ifelse(gc_batch == 8 , 1, 0)) %>% 
    mutate(race_2 = ifelse(race == 2 , 1, 0)) %>% 
    mutate(race_3 = ifelse(race == 3 , 1, 0)) %>% 
    select(age_scaled, edu_scaled, race_2, race_3, female, contains("batch"), -gc_batch, apoe_e4) %>% 
    map_df(~as.numeric(.x)) %>% 
    as.matrix()

z <- all_data %>% 
    select(all_of(chemical_groups$chemical)) %>% 
    as.matrix()

id <- all_data %>% 
    select(sub_id) %>% 
    mutate_at("sub_id", as.factor) %>% 
    map_df(~as.numeric(.x)) %>% 
    as.matrix()

groups <- as.numeric(as.factor(chemical_groups$source))

num <- 60000

set.seed(1990)

# Memory
y <- all_data$memory

memory_bkmr <- run_bkmr(outcome = y, covariates = x, chemicals = z, ID = id, 
                        chemical_groups = groups, iter_num = num, outcome_name = "memory")

# Speed
y <- all_data$speed
speed_bkmr <- run_bkmr(outcome = y, covariates = x, chemicals = z, ID = id, 
                        chemical_groups = groups, iter_num = num, outcome_name = "speed")

# reasoning
y <- all_data$reasoning
reasoning_bkmr <- run_bkmr(outcome = y, covariates = x, chemicals = z, ID = id, 
                       chemical_groups = groups, iter_num = num, outcome_name = "reasoning")

# vocabulary
y <- all_data$vocab
vocab_bkmr <- run_bkmr(outcome = y, covariates = x, chemicals = z, ID = id, 
                           chemical_groups = groups, iter_num = num, outcome_name = "vocabulary")

# global score
y <- all_data$global_score
global_bkmr <- run_bkmr(outcome = y, covariates = x, chemicals = z, ID = id, 
                       chemical_groups = groups, iter_num = num, outcome_name = "global_score")


# Save data for plots
bkmr_get_data_for_plots(outcome_name = "memory", iter_num = num)
bkmr_get_data_for_plots(outcome_name = "vocabulary", iter_num = num)
bkmr_get_data_for_plots(outcome_name = "reasoning", iter_num = num)
bkmr_get_data_for_plots(outcome_name = "speed", iter_num = num)
bkmr_get_data_for_plots(outcome_name = "global_score", iter_num = num)

