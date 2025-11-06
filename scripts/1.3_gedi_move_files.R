# Move GEDI files to proper destinations after downloading

wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"

yearid <- "2020"

# Set the source folder and destination root
source_folder <- paste0(wd, "/gedi_amz_2B_", yearid, "/all_", yearid, "_gedi_raw")
destination_root <- paste0(wd, "/gedi_amz_2B_", yearid, "/destination_files")

# Create a vector of all .h5 files in the source folder
h5_files <- list.files(path = source_folder, pattern = "\\.h5$", full.names = TRUE)

# helper function to get the destination folder for a given DOY
get_destination_folder <- function(doy) {
  destination_index <- ((doy - 1) %/% 16) + 1 #grouping by 16 days
  destination_folder <- file.path(destination_root, sprintf("doy_%03d", (destination_index - 1) * 16 + 1))
  
  # Create the destination folder if it doesn't exist
  if (!file.exists(destination_folder)) {
    dir.create(destination_folder)
  }
  
  return(destination_folder)
}

# Loop through each file
for (file_path in h5_files) {
  # Extract the DOY from the file name
  doy_str <- substring(basename(file_path), 14, 16)  # Assuming DOY is in positions 14 to 16
  
  file_doy <- as.numeric(doy_str)
  
  destination_folder <- get_destination_folder(file_doy)
  
  file.rename(file_path, file.path(destination_folder, basename(file_path)))
}

# Convert existing folders within destination_root from "doy_***" to "YYYY.MM.DD" format
grouped_folders <- list.dirs(destination_root, recursive = FALSE, full.names = TRUE)
for (group_folder in grouped_folders) {
  # Extract the DOY code
  doy_str <- substring(basename(group_folder), 5, 7)  # Assuming DOY is in positions 5 to 7
  
  doy <- as.numeric(doy_str)
  
  # Convert DOY to "YYYY.MM.DD" format
  base_date <- as.Date("2020-01-01")  # Change the base date accordingly
  date_in_folder <- base_date + as.difftime((doy - 1) * 86400, units = "secs")
  formatted_date <- format(date_in_folder, "%Y.%m.%d")
  
  # Rename the folder to "YYYY.MM.DD" format
  file.rename(group_folder, file.path(destination_root, formatted_date))
}

# Print a message indicating the process is complete
cat("Files and folders moved successfully.\n")


# Set the source and destination subdirectories
source_subdirectory <- paste0(wd, "/gedi_amz_2B_", yearid, "/destination_files")
destination_subdirectory <- paste0(wd, "/gedi_amz_2B_", yearid)

# Get the list of date folders in the source subdirectory
source_date_folders <- list.dirs(source_subdirectory, full.names = TRUE, recursive = FALSE)

# Loop through each date folder in the source subdirectory
for (source_date_folder in source_date_folders) {
  # Extract the date from the folder name
  date_str <- basename(source_date_folder)
  
  # Create the corresponding destination folder path
  destination_date_folder <- file.path(destination_subdirectory, date_str)
  
  # Check if the destination folder exists
  if (file.exists(destination_date_folder)) {
    # Get the list of files in the source date folder
    source_files <- list.files(source_date_folder, full.names = TRUE)
    
    # Move each file to the corresponding destination folder
    file.rename(source_files, file.path(destination_date_folder, basename(source_files)))
  } else {
    cat("Destination folder not found for date:", date_str, "\n")
  }
}

# Print a message indicating the process is complete
cat("Files moved successfully.\n")


