
# ------------------------------------------------------------------------------- #
# How to Access the LP DAAC Data Pool with R
# --------------------------------------------------------------------------- #
# Author: LP DAAC, modified by Charles Southwick

rm(list=ls())

# Load necessary packages into R
library(sys)
library(getPass)
library(parallel)
library(httr)
#library(httr2)
# ---------------------------------SET UP ENVIRONMENT--------------------------------------------- #

wd <- "/Users/charlessouthwick/Documents/PhD/sifgedi"

#For downloading GEDI data --------------------------

yearid <- "2021"

#For downloading GEDI Level 2B data ------------------------
#Comment this out if you want to download other data!

#Change this line!!!

# date_dir <- paste0(yearid, ".11.17")
# 
# dl_dir <- paste0(wd, "/gedi_amz_2B_", yearid, "/", date_dir)  # Set dir to download files to
# 
# # Textfile containing links (replace with your text file location)
# files <- readLines(paste0(dl_dir, "/gedi_", date_dir, ".txt"), warn = FALSE)


#For downloading MCD43C4 v061 data --------------------------
#comment this out if you want to download other data!

# raw_dir <- "raw"
# 
# mcd_dir <- paste0(wd, "/mcd43c4_data_", yearid)
# dl_dir <- paste0(mcd_dir, "/", raw_dir)  # Set dir to download files to
# 
# # Textfile containing links (replace with your text file location)
# files <- readLines(paste0(mcd_dir, "/granules", "/mcd43c4_", yearid, "_allgranules", ".txt"), warn = FALSE)


#For downloading MCD19A1CMGO v061 data --------------------------
#comment this out if you want to download other data!
# 
# raw_dir <- "raw"
# 
# mcd_dir <- paste0(wd, "/mcd19a1cmgo_data_", yearid)
# dl_dir <- paste0(mcd_dir, "/", raw_dir)  # Set dir to download files to
# 
# # Textfile containing links (replace with your text file location)
# files <- readLines(paste0(mcd_dir, "/granules", "/mcd19a1cmgo_data_", yearid, "_allgranules", ".txt"), warn = FALSE)

#For downloading MCD19A1CMGL v061 data --------------------------
#comment this out if you want to download other data!

# raw_dir <- "raw"
# 
# mcd_dir <- paste0(wd, "/mcd19a1cmgl_data_", yearid)
# dl_dir <- paste0(mcd_dir, "/", raw_dir)  # Set dir to download files to
# 
# # Textfile containing links (replace with your text file location)
# files <- readLines(paste0(mcd_dir, "/granules", "/mcd19a1cmgl_data_", yearid, "_allgranules", ".txt"), warn = FALSE)


#For downloading MOD15A2H v061 data --------------------------
#comment this out if you want to download other data!

# raw_dir <- "raw"
# 
# mcd15_dir <- paste0(wd, "/mcd15a2h_fpar_data_", yearid)
# dl_dir <- paste0(mcd15_dir, "/", raw_dir)  # Set dir to download files to
# 
# # Textfile containing links (replace with your text file location)
# files <- readLines(paste0(mcd15_dir, "/all_granules_", yearid, ".txt"), warn = FALSE)
# # 

#For downloading PACE L3 8-Day data --------------------------
#comment this out if you want to download other data!

# pace_dir <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi/PACE/pace_vi_data"
# dl_dir <- paste0(pace_dir, "/raw") # Set dir to download files to
# 
# # Textfile containing links (replace with your text file location)
# files <- readLines(paste0(pace_dir, "/PACE_all_8day_l3_4km_VIs.txt"), warn = FALSE)
# 
# 

#For downloading MCD18C2 v062 PAR data --------------------------
#comment this out if you want to download other data!

boxwd <- "/Users/charlessouthwick/Library/CloudStorage/Box-Box/sifgedi"

dl_dir <- paste0(boxwd, "/MCD18C2_par_data")

# Textfile containing links (replace with your text file location)
files <- readLines(paste0(dl_dir, "/alldata_2019thru2021.txt"), warn = FALSE)

## Set working directory and NETRC file ---------------------

setwd(dl_dir)                                          # Set the working dir to the dl_dir
usr <- file.path(Sys.getenv("USERPROFILE"))           # Retrieve home dir (for netrc file)
if (usr == "") {usr = Sys.getenv("HOME")}              # If no user profile exists, use home
netrc <- file.path(usr,'.netrc', fsep = .Platform$file.sep)  # Path to netrc file

# ------------------------------------CREATE .NETRC FILE--------------------- #
# If you already have a .netrc file with your Earthdata Login credentials stored in your home directory, this portion will be skipped. Otherwise you will be prompted for your NASA Earthdata Login Username/Password and a netrc file will be created to store your credentials (in home dir).

#If your password is entered incorrectly, this will delete it.
# Check if .netrc file exists, and if so, delete it to ensure updated credentials
# if (file.exists(netrc)) {
#   file.remove(netrc)  # Remove the old netrc file to replace it with updated credentials
# }

if (file.exists(netrc) == FALSE || grepl("urs.earthdata.nasa.gov", readLines(netrc)) == FALSE) {
  netrc_conn <- file(netrc)
   
   # User will be prompted for NASA Earthdata Login Username and Password below
   writeLines(c("machine urs.earthdata.nasa.gov",
                sprintf("login %s", getPass(msg = "Enter NASA Earthdata Login Username \n (or create an account at urs.earthdata.nasa.gov) :")),
                sprintf("password %s", getPass(msg = "Enter NASA Earthdata Login Password:"))), netrc_conn)
   close(netrc_conn)
 }

# ---------------------------CONNECT TO DATA POOL AND DOWNLOAD FILES---------------- #

# Define the function for downloading a single file
download_file <- function(file_url) {
  filename <- tail(strsplit(file_url, '/')[[1]], n = 1)  # Keep original filename
  
  # Write file to disk (authenticating with netrc) using the current directory/filename
  response <- GET(file_url, write_disk(filename, overwrite = TRUE), progress(),
                  config(netrc = TRUE, netrc_file = netrc), set_cookies("LC" = "cookies"))
  
  # Check to see if file downloaded correctly
  if (response$status_code == 200) {
    sprintf("%s downloaded at %s", filename, dl_dir)
  } else {
    sprintf("%s not downloaded. Verify that your username and password are correct in %s", filename, netrc)
  }
}

results <- lapply(files, download_file)
print(results)


