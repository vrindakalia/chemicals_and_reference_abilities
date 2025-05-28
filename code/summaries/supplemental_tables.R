# Supplemental tables
stable1 <- read_tsv("code/output/all_chemicals_loq_and_detection.txt")
stable2 <- read_tsv("code/output/chemical_descriptors.txt")
stable3 <- read_tsv("code/output/chemical_correlations.txt")


stable2 <- stable2 %>% 
    mutate(source_updated = factor(source, 
                                   levels = c("agricultural", "food", "household dust", "industrial",
                                              "legacy", "legacy PCB", "other"),
                                   labels = c("Farming/ Green space", "Food", "Indoor dust", "Industrial use",
                                              "Legacy", "Legacy PCB", "other"))) %>% 
    select(-source)

names(stable1) <- c("Chemical", "Limit of Quantitation", "Proportion of samples with detection (overall)",
                    "Proportion of samples with detection (visit 0)","Proportion of samples with detection (visit 1)")

names(stable2) <- c("Chemical", "Arithmetic mean (visit 0)", "Arithmetic mean (visit 1)",
                    "Standard deviation (visit 0)", "Standard deviation (visit 1)",
                    "Geometric mean (visit 0)", "Geometric mean (visit 1)",
                    "Median (visit 0)", "Median (visit 1)",
                    "Inter-quartile range (visit 0)", "Inter-quartile range (visit 1)",
                    "Assigned chemical source")
names(stable3) <- c("Chemical (x)", "Chemical (y)", "Correlation coefficient", "p-value",
                    "Chemical source (x)", "Chemical source (y)")
cover_sheet <- as.data.frame("This is a summary of the supplemental tables")
names(cover_sheet) <- ""

list_of_datasets <- list("Summary" = cover_sheet, "LOQ and detection rates" = stable1,
                         "Chemical summaries" = stable2, "Chemical correlations" = stable3)

write.xlsx(list_of_datasets, file = "code/output/supplemental_tables.xlsx")

table(stable2$`Assigned chemical source`)
stable2 %>% 
    select(Chemical, contains("Median")) %>% 
    gather(key = "visit", value = "median", -Chemical) %>% 
    arrange(median) %>% 
    slice(1)
