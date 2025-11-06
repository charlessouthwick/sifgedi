
#Download TROPOSIF data
# Data used with permission

# Load necessary library
library(httr)

wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"

# Function to download files for a specific month
download_files <- function(year = 2021, month = "01", timeout = 300) {
  # Ensure the month is formatted as two digits
  month <- sprintf("%02d", as.numeric(month))
  
  # Base URL for the TROPOMI data
  base_url <- paste0("https://ftp.sron.nl/open-access-data-2/TROPOMI/tropomi/sif/v2.1/l2b/", year, "/")
  
  # Create the local folder for storing the downloaded files
  local_folder <- paste0(wd, "/troposif_data/", year)
  if (!dir.exists(local_folder)) {
    dir.create(local_folder, recursive = TRUE)
  }
  
  # URL for the specific month's directory
  month_url <- paste0(base_url, month, "/")
  
  # Get the list of files in the FTP directory for the selected month
  response <- GET(month_url, timeout(timeout))
  if (status_code(response) != 200) {
    message("Failed to access ", month_url)
    return(NULL)
  }
  
  # Extract filenames from the response content
  content_text <- content(response, "text")
  file_pattern <- "TROPOSIF_L2B_2021-[0-9]{2}-[0-9]{2}\\.nc"
  files <- regmatches(content_text, gregexpr(file_pattern, content_text))[[1]]
  
  # Loop over the files and download them
  for (file in files) {
    file_url <- paste0(month_url, file)
    destfile <- paste0(local_folder, "/", file)
    
    # Download the file if it doesn't exist locally
    if (!file.exists(destfile)) {
      tryCatch({
        download.file(file_url, destfile, mode = "wb", timeout = timeout)
        message("Downloaded: ", file)
      }, error = function(e) {
        message("Failed to download: ", file, " - ", e$message)
      })
    } else {
      message("File already exists: ", file)
    }
  }
}

# Example call to download files for a specific month (e.g., January)
download_files(year = 2021, month = "01", timeout = 300)

