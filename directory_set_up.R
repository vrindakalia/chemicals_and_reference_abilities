# script to create directories and folders needed for analysis
# last updated: 04/23/3035

# install packages ----
source("code/packages/packages_to_load.R")

# paths to the folders ----
code_folder <- here("code")
figures_output_folder <- here("figures")
figures_bkmr_output_folder <- here("figures","bkmr")
data_preparation_folder <- here("code","data_preparation")
data_exploration_folder <- here("code","data_exploration")
packages_folder <- here("code","packages")
functions_folder <- here("code","functions")
models_folder <- here("code","models")
output_folder <- here("code","output")
figures_folder <- here("code","figures")

# all paths in one vector ----
folder_names <- grep("_folder", names(.GlobalEnv), value = TRUE)

# if the folder does not exist, create the folder ---- 
map_lgl(folder_names, ~ ifelse(!dir.exists(get(.)), dir.create(get(.), recursive = TRUE), FALSE))
