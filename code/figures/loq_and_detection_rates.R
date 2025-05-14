# Detection rates
source("code/packages/packages_to_load.R")
source("code/functions/functions_cleaning.R")

# To do:

# Call in chemical data ----
# Get the names for columns with chemical concentrations data; data folder is not on github
colnames_chemical_concentrations <- c("Chemicals", "LOQ", as.character(read_excel("data/SternForYuPlasma Targeted GC Analysis Final Data Set June2021.xlsx",
                                                                                  sheet = 1, range = "C5:ML5",col_names = FALSE)))

# Get the chemical concentration data; data folder is not on github
chemical_concentrations_t <- read_excel("data/SternForYuPlasma Targeted GC Analysis Final Data Set June2021.xlsx", skip = 9, col_names = colnames_chemical_concentrations)

# Get names of the chemicals
chemical_names <- chemical_concentrations_t %>% 
    mutate(chem_1 = gsub("α", "a", Chemicals)) %>% 
    mutate(chem_2 = gsub("β", "b", chem_1)) %>% 
    mutate(chem_3 = gsub("δ", "d", chem_2)) %>%
    filter(!grepl("Recovery", Chemicals)) %>% 
    .$chem_3

chemicals_loq <- chemical_concentrations_t %>% 
    select(chemical = Chemicals, LOQ) %>% 
    filter(!grepl("Recovery", chemical)) %>% 
    mutate(chem_1 = gsub("α", "a", chemical)) %>% 
    mutate(chem_2 = gsub("β", "b", chem_1)) %>% 
    mutate(chem_3 = gsub("δ", "d", chem_2)) %>% 
    select(chemical = chem_3, LOQ)

# Transpose the dataframe so observations are in rows and chemical concentration data in columns
chemical_concentrations <- chemical_concentrations_t %>% 
    select(-LOQ) %>% 
    filter(!grepl("Recovery", Chemicals)) %>% 
    mutate(chem_1 = gsub("α", "a", Chemicals)) %>% 
    mutate(chem_2 = gsub("β", "b", chem_1)) %>% 
    mutate(chem_3 = gsub("δ", "d", chem_2)) %>%
    select(chem_3, everything(), -chem_1, -chem_2, -Chemicals) %>% 
    t() %>% 
    as.data.frame() %>% 
    row_to_names(1) %>% 
    rownames_to_column(var = "YSAD")

# Check detection frequency ----
chemicals_detection <- chemical_concentrations %>% 
    select(all_of(chemical_names)) %>% 
    map_df(~codeLODNF(.x))

chemicals_detection$YSAD = chemical_concentrations$YSAD

chemicals_detection_meta_data <- merge(meta_data, chemicals_detection, by = "YSAD")

# Tally detection across all samples ----
detection_tally <- gather(chemicals_detection, key = "chemical", value = "detection") %>%
    group_by(chemical, detection) %>%
    tally %>% 
    spread(detection, n, fill = 0) %>% 
    mutate(proportion_detected = `3`/dim(chemicals_detection)[1]) %>% 
    select(chemical, proportion_detected)

# Detection by visit ----
detection_tally_by_visit <- chemicals_detection_meta_data %>% 
    select(YSAD, BL0FU1, all_of(chemical_names)) %>% 
    gather(., key = "chemical", value = "detection", -YSAD, -BL0FU1) %>%
    group_by(chemical, detection, BL0FU1) %>%
    tally() 

# Detection tally in samples from visit 1
number_of_n_visit0 <- chemicals_detection_meta_data %>% 
    filter(BL0FU1 == 0) %>% 
    dim() %>% .[1]

detection_tally_visit0 <- detection_tally_by_visit %>% 
    filter(BL0FU1 == 0) %>% 
    spread(detection, n, fill = 0) %>% 
    mutate(proportion_detected_visit0 = `3`/number_of_n_visit0) %>% 
    select(chemical, proportion_detected_visit0)

# Detection tally in samples from visit 2
number_of_n_visit1 <- chemicals_detection_meta_data %>% 
    filter(BL0FU1 == 1) %>% 
    dim(.) %>% .[1]

detection_tally_visit1 <- detection_tally_by_visit %>% 
    filter(BL0FU1 == 1) %>% 
    spread(detection, n, fill = 0) %>% 
    mutate(proportion_detected_visit1 = `3`/number_of_n_visit1) %>% 
    select(chemical, proportion_detected_visit1)

tally_both_visits_and_overall <- merge(detection_tally_visit0, detection_tally_visit1, by = "chemical") %>% 
    merge(detection_tally,., by = "chemical")

chemicals_detection_loq <- merge(chemicals_loq, tally_both_visits_and_overall, by = "chemical")

chemicals_detection_loq %>% write_tsv("code/output/all_chemicals_loq_and_detection.txt")
