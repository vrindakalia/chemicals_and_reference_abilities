#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clean and prepare the chemical exposure data
# Data are from targeted analysis of serum
# Date edited: 04/24/2025
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("code/packages/packages_to_load.R")
source("code/functions/functions_cleaning.R")

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

# Get the names for columns with meta data and clean the names; data folder is not on github
colnames_targeted_assay_meta_data <- read_excel("data/SternForYuPlasma Targeted GC Analysis Final Data Set June2021.xlsx",
                                               sheet = 1, range = "B4:B8",col_names = c("names")) %>% 
    mutate(names_clean_1 = gsub(":","", names)) %>% 
    mutate(names_clean_2 = gsub("#","", names_clean_1)) %>% 
    mutate(names_clean_3 = gsub("\\(µL\\)","", names_clean_2))

# Get the metadata; data folder is not on github
targeted_assay_meta_data <- read_excel("data/SternForYuPlasma Targeted GC Analysis Final Data Set June2021.xlsx",
                                                    sheet = 1, range = "C4:ML8",col_names = FALSE)
# Transpose the dataset
targeted_assay_meta_data_clean <- targeted_assay_meta_data %>% 
    t() %>% 
    as.data.frame()

# Call in meta data to get visit information 
meta_data <- read_sav("data/CRRANN_Exposome_352_DEMsel.sav") %>% 
    rename(SampleName = SampleName3)

# Assign names to metadata column
names(targeted_assay_meta_data_clean) <- colnames_targeted_assay_meta_data$names_clean_3

targeted_assay_meta_data_clean %>% write_tsv("data/targeted_assay_meta_data.txt")

# Check detection frequency ----
chemicals_detection <- chemical_concentrations %>% 
    select(all_of(chemical_names)) %>% 
    map_df(~codeLODNF(.x))

chemicals_detection$YSAD = chemical_concentrations$YSAD

chemicals_detection_meta_data <- merge(meta_data, chemicals_detection, by = "YSAD")

# Tally detection across all samples ----
detection_tally <- chemicals_detection %>% 
    select(-YSAD) %>% 
    gather(., key = "chemical", value = "detection") %>%
    group_by(chemical, detection) %>%
    tally %>% 
    spread(detection, n, fill = 0) %>% 
    mutate(proportion_detected = `3`/dim(chemicals_detection)[1]) %>% 
    select(chemical, missing = `1`, belowLOD = `2`, detected = `3`, proportion_detected)

detection_tally %>% write_tsv("code/output/detection_all_samples.txt")

# Chemicals detected in at least 50% of the samples ----
detection_in_atl_50perc <- detection_tally %>% 
    filter(proportion_detected >= 0.5)

# Tally detection across all samples, by visit ----
detection_tally_by_visit <- chemicals_detection_meta_data %>% 
    select(Visit, all_of(chemical_names)) %>% 
    gather(., key = "chemical", value = "detection", -Visit) %>%
    group_by(chemical, detection, Visit) %>%
    tally 

v1_n <- chemicals_detection_meta_data %>% 
    filter(Visit == "Baseline") %>% 
    dim(.) %>% 
    .[1]

v2_n <- chemicals_detection_meta_data %>% 
    filter(Visit == "Follow Up 1") %>% 
    dim(.) %>% 
    .[1]

detection_tally_by_visit1 <- detection_tally_by_visit %>% 
    filter(Visit == "Baseline") %>% 
    spread(detection, n, fill = 0) %>% 
    mutate(proportion_detected_v1 = `3`/v1_n) %>% 
    select(chemical, missing = `1`, belowLOD = `2`, detected = `3`, proportion_detected_v1) %>% 
    select(chemical, proportion_detected_v1)

detection_tally_by_visit2 <- detection_tally_by_visit %>% 
    filter(Visit == "Follow Up 1") %>% 
    spread(detection, n, fill = 0) %>% 
    mutate(proportion_detected_v2 = `3`/v2_n) %>% 
    select(chemical, missing = `1`, belowLOD = `2`, detected = `3`, proportion_detected_v2) %>% 
    select(chemical, proportion_detected_v2)

detection_tally_both_visits <- merge(detection_tally_by_visit1, detection_tally_by_visit2, by = "chemical") %>% 
    mutate(included = case_when(chemical %in% detection_in_atl_50perc$chemical ~ 1,
                                !chemical %in% detection_in_atl_50perc$chemical ~ 0))

png("figures/detection_rate_by_visit_colored_by_inclusion.png",
    res = 300, units = "in", h = 4, w = 6)
detection_tally_both_visits %>% 
    ggplot(aes(x = proportion_detected_v1, y = proportion_detected_v2, color = factor(included))) +
    geom_point() +
    geom_abline(intercept = 0.00, slope = 1, linetype = 2, colour = "black", alpha = 0.6) +
    annotate("text", x = 0.9, y = 0.1, label = "r = 0.7") +
    theme_bw() +
    geom_vline(xintercept = 0.5, linetype = "dashed", alpha = 0.7, color = "blue") +
    geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.7, color = "blue") +
    scale_color_manual(values = c("grey66", "orange")) +
    theme(legend.position = "bottom") +
    labs(x = "Detection, baseline", y = "Detection, followup", color = "Included") 
dev.off()
cor.test(detection_tally_both_visits$proportion_detected_v1, detection_tally_both_visits$proportion_detected_v2, method = "pear")

# Replace N/F with NA, and LOD and NA with half minimum value  ----
chemicals_replaced <- chemical_concentrations %>% 
    select(detection_in_atl_50perc$chemical) %>% 
    map_df(~replaceLODzero(.x)) %>% 
    map_df(~replaceNFNA(.x)) %>% 
    modify_if(is.character, as.numeric) %>% 
    map_df(~replacezero(.x)) %>% 
    map_df(~replaceNA(.x))

chemicals_replaced$YSAD <- chemical_concentrations$YSAD

chemicals_replaced %>% 
    select(YSAD, everything()) %>% 
    write_tsv("data/chemical_atl50detect_replaced.txt")

# Filter chemicals based on variability ----
# Calculate average chemical concentration at the two time points
chemicals_replaced_with_meta_data <- merge(chemicals_replaced, meta_data, by = "YSAD") 

chemical_concentration_average <- chemicals_replaced_with_meta_data %>% 
    select(all_of(detection_in_atl_50perc$chemical), SubID, YSAD) %>% 
    gather(key = "chemical", value = "level", -SubID, -YSAD) %>% 
    group_by(chemical, SubID) %>% 
    summarise(avg_conc = mean(level)) %>% 
    ungroup()

# Calculate Coefficient of Quartile Variation (CQV) using average concentration ----
chemicals_cqv <- chemical_concentration_average %>% 
    group_by(chemical) %>% 
    summarise(Q75_all = quantile(avg_conc,0.75), Q25_all = quantile(avg_conc,0.25)) %>% 
    ungroup() %>% 
    mutate(cqv_all = (Q75_all-Q25_all)/(Q75_all+Q25_all))
    
chemicals_cqv %>% 
    ggplot(aes(x = cqv_all, y = reorder(chemical, cqv_all))) +
    geom_point() +
    theme_bw()

chemicals_cqv_gt_06 <- chemicals_cqv %>% 
    filter(cqv_all > 0.6)

# Looking at each time point individually ----
chemical_cqv_0 <- chemicals_replaced_with_meta_data %>% 
    filter(BL0FU1 == 0) %>% 
    select(all_of(detection_in_atl_50perc$chemical)) %>% 
    gather(key = "chemical", value = "level") %>% 
    group_by(chemical) %>% 
    summarise(Q75_0 = quantile(level,0.75), Q25_0 = quantile(level,0.25)) %>% 
    ungroup() %>% 
    mutate(cqv_0 = (Q75_0-Q25_0)/(Q75_0+Q25_0))
    
chemical_cqv_1 <- chemicals_replaced_with_meta_data %>% 
    filter(BL0FU1 == 1) %>% 
    select(all_of(detection_in_atl_50perc$chemical)) %>% 
    gather(key = "chemical", value = "level") %>% 
    group_by(chemical) %>% 
    summarise(Q75_1 = quantile(level,0.75), Q25_1 = quantile(level,0.25)) %>% 
    ungroup() %>% 
    mutate(cqv_1 = (Q75_1-Q25_1)/(Q75_1+Q25_1))

chemicals_cqv_all <- merge(chemical_cqv_0, chemical_cqv_1, by = "chemical") %>% 
    merge(., chemicals_cqv, by = "chemical")

chemicals_cqv_all %>% 
    ggplot(aes(x = cqv_0, y = cqv_1)) +
    geom_point() +
    geom_hline(yintercept = 0.6) +
    geom_vline(xintercept = 0.6) 

cqv_gt_06 <- chemicals_cqv_all %>% 
    filter(cqv_all > 0.6) %>% 
    filter(cqv_0 > 0.6) %>% 
    filter(cqv_1 > 0.6) 

cqv_gt_06 %>% select(chemical) %>% write_tsv("data/chemical_names_filtered.txt")

# Filter based on variability ----
chemicals_replaced_filtered <- chemicals_replaced %>% 
    select("YSAD", cqv_gt_06$chemical) 

chemicals_replaced_filtered_transformed <- chemicals_replaced_filtered %>% 
    select(-YSAD) %>% 
    map_df(~as.numeric(.x)) %>% 
    map_df(~log2(.x)) %>% 
    scale(., center = TRUE, scale = TRUE) %>% 
    as.data.frame()

chemicals_replaced_filtered_transformed$ysad <- chemicals_replaced_filtered$YSAD

# Save filtered and replaced dataframe ----
chemicals_replaced_filtered_transformed %>% 
    select(ysad, everything()) %>% 
    write_tsv("data/chemicals_replaced_filtered_transformed.txt")

# Replace N/F and <LOD with NA (for ICC calculation and boxplots)  ----
chemicals_LOD_replaced_NA <- chemical_concentrations %>% 
    select(detection_in_atl_50perc$chemical) %>% 
    select(cqv_gt_06$chemical) %>% 
    map_df(~replaceLODNA(.x)) %>% 
    map_df(~replaceNFNA(.x)) %>% 
    modify_if(is.character, as.numeric)

chemicals_LOD_replaced_NA$YSAD <- chemical_concentrations$YSAD

chemicals_LOD_replaced_NA %>% 
    select(YSAD, everything()) %>% 
    write_tsv("data/chemical_lod_replaced_with_NA.txt")
