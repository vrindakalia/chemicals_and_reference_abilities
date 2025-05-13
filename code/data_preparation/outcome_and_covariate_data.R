source("code/packages/packages_to_load.R")
source("code/functions/functions.R")

# Call in the meta data and outcome data
meta_data <- read_tsv("data/phenotype_data_complete.txt") 

outcome_data <- read_csv("data/CRRANN_Exposome_254_cognitive change.csv") %>% 
    gather(key = "time", value = "RA", -SubID, -ALL6_flcs, -ALL6_slcs, -ALL6_mlcs, -ALL6_vlcs) %>% 
    mutate(visit = case_when(grepl("lat0", time) ~ 0,
                             grepl("lat1", time) ~ 1)) %>% 
    mutate(domain = case_when(grepl("sl", time) ~ "speed",
                              grepl("vl", time) ~ "vocab",
                              grepl("ml", time) ~ "memory",
                              grepl("fl", time) ~ "reasoning")) %>% 
    select(SubID, domain, visit, RA) %>% 
    spread(key = "domain", value = "RA") %>% 
    rename(BL0FU1 = visit) %>% 
    mutate(global_score = rowMeans(across(.cols = c("memory", "vocab", "speed", "reasoning"))))

variables <- c("YSAD", "SubID", "BL0FU1", "Age", "Education", "ApoeE4", "race01_3g", "Female", "Age_Followup",
               "global_score", "memory", "vocab", "speed", "reasoning")

outcome_meta_data <- merge(meta_data, outcome_data, by = c("SubID", "BL0FU1")) %>% 
    select(all_of(variables)) %>% 
    mutate(age_eval = case_when(BL0FU1 == 0 ~ Age,
                                BL0FU1 == 1 ~ Age_Followup)) %>% 
    mutate(edu_min = Education - 12) %>% 
    janitor::clean_names() %>% 
    rename(race = race01_3g)

outcome_meta_data %>% write_tsv("data/outcome_and_meta_data.txt")

