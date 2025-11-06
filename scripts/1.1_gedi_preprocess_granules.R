#Create new directories to correspond to the granule dates of interest

rm(list=ls())

# Load necessary libraries
library(tidyverse)
library(fs)


wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"
setwd(wd)

yearid <- "2019"

parent_folder <- paste0(wd, "/gedi_amz_2B_", yearid)

# Read the list of file paths from the text file
file_paths <- readLines(paste0(parent_folder, "/gedi_allgranules_", yearid, ".txt"))


# Function to extract the date from a single file path
extract_date <- function(file_path) {
  # Extract the year and day of year (DOY) using regular expression
  file_dates <- str_match(file_path, "_(\\d{4})(\\d{3})\\d*_")[,2:3]
  
  # Convert to date
  year <- as.integer(file_dates[1])  # Extracted year
  doy <- as.integer(file_dates[2])   # Extracted day of year
  
  # Convert to a proper date
  date_converted <- as.character(as.Date(doy - 1, origin = paste0(year, "-01-01")))
  return(date_converted)
}

# Apply the function over the list of file paths
file_dates1 <- lapply(file_paths, extract_date)

# Convert to a vector (optional)
file_dates <- unlist(file_dates1)


start_date <- as.Date(paste0(yearid, "-01-01"))
end_date <- max(file_dates)


# Create a function to extract 16-day chunks and save to a new folder and text file
extract_and_save <- function(start_date) {
  end_date <- start_date + 15
  
  # Filter files within the 16-day range
  selected_files <- file_paths[file_dates >= start_date & file_dates <= end_date]
  
  # Create a folder with the start_date as the name
  folder_name <- format(start_date, "%Y.%m.%d")
  proc_name <- "processed"
  
  # Create the directories if they don't exist
  dir_create(fs::path(parent_folder, folder_name))
  dir_create(fs::path(parent_folder, folder_name, proc_name))
  
  # Write selected files to a new text file within the folder
  writeLines(selected_files, fs::path(parent_folder, folder_name, paste0("gedi_", folder_name, ".txt")))
  
}

# Loop through the dates in 16-day intervals
while (start_date <= end_date) {
  extract_and_save(start_date)
  start_date <- start_date + 16
}


