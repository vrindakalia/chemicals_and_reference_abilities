
# the packages needed 

packages_needed <- c("tidyverse", "bkmr", "openxlsx", "cowplot", "table1", "ggcorrplot", "lme4",
                     "haven", "corrplot", "here", "purrr", "readxl", "janitor", "gee", "performance",
                     "cowplot", "table1")

# install only those packages not currently installed
packages_unavailable <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if(length(packages_unavailable)) install.packages(packages_unavailable)

# load packages
lapply(packages_needed, require, character.only = TRUE)
