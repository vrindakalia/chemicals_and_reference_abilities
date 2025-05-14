# chemical descriptors
source("code/packages/packages_to_load.R")

# Meta data
meta_data <- read_tsv("data/outcome_and_meta_data.txt")

# Chemical concentration data
chemicals <- read_tsv("data/chemical_lod_replaced_with_NA.txt") %>% 
    rename(ysad = YSAD)

chemical_names <- chemicals %>% 
    select(-ysad) %>% names(.)

# Chemicals and meta data
chemicals_with_meta_data <- merge(meta_data, chemicals, by = "ysad")

# Chemical group information
chemical_groups <- read_tsv("data/chemical_groups.txt")

# Summarise chemical concentrations (mean, sd, median, and IQR) by visit
chemicals_long <- chemicals_with_meta_data %>% 
    select(ysad, bl0fu1, all_of(chemical_names)) %>% 
    gather(key = "chemical", value = "concentration", -ysad, -bl0fu1) %>% 
    group_by(chemical, bl0fu1) %>% 
    summarise(mean = mean(concentration, na.rm = T)) %>% 
    spread(value = "mean", key = "bl0fu1") %>% 
    rename(mean_visit0 = `0`, mean_visit1 = `1`)

chemicals_median <- chemicals_with_meta_data %>% 
    select(ysad, bl0fu1, all_of(chemical_names)) %>% 
    gather(key = "chemical", value = "concentration", -ysad, -bl0fu1) %>% 
    group_by(chemical, bl0fu1) %>% 
    summarise(median = median(concentration, na.rm = T)) %>% 
    spread(value = "median", key = "bl0fu1") %>% 
    rename(median_visit0 = `0`, median_visit1 = `1`)

chemicals_sd <- chemicals_with_meta_data %>% 
    select(ysad, bl0fu1, all_of(chemical_names)) %>% 
    gather(key = "chemical", value = "concentration", -ysad, -bl0fu1) %>% 
    group_by(chemical, bl0fu1) %>% 
    summarise(sd = sd(concentration, na.rm = T)) %>% 
    spread(value = "sd", key = "bl0fu1") %>% 
    rename(sd_visit0 = `0`, sd_visit1 = `1`)

chemicals_iqr <- chemicals_with_meta_data %>% 
    select(ysad, bl0fu1, all_of(chemical_names)) %>% 
    gather(key = "chemical", value = "concentration", -ysad, -bl0fu1) %>% 
    group_by(chemical, bl0fu1) %>% 
    summarise(iqr = IQR(concentration, na.rm = T)) %>% 
    spread(value = "iqr", key = "bl0fu1") %>% 
    rename(iqr_visit0 = `0`, iqr_visit1 = `1`)

# Geometric mean
chemicals_gm <- chemicals_with_meta_data %>% 
    select(ysad, bl0fu1, all_of(chemical_names)) %>% 
    gather(key = "chemical", value = "concentration", -ysad, -bl0fu1) %>% 
    group_by(chemical, bl0fu1) %>% 
    summarise(gm = exp(mean(log(concentration), na.rm = T))) %>% 
    spread(value = "gm", key = "bl0fu1") %>% 
    rename(gm_visit0 = `0`, gm_visit1 = `1`)

# All relevant descriptors
chemical_descriptors <- merge(chemicals_mean, chemicals_sd, by = "chemical") %>% 
    merge(., chemicals_gm, by = "chemical") %>% 
    merge(., chemicals_median, by = "chemical") %>% 
    merge(., chemicals_iqr, by = "chemical") %>% 
    merge(., chemical_groups, by = "chemical")
    

chemical_descriptors %>% write_tsv("code/output/chemical_descriptors.txt")
