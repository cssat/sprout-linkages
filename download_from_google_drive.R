# install.packages("googledrive")
# install.packages("dplyr")

# For general purpose
library(tidyverse)
source('helpers.R')

# for pulling files down from Google Drive
library(googledrive)

# for reading files in
library(haven)
library(readxl)

drive_auth(use_oob = FALSE)

rodis_pii_url <- "https://drive.google.com/drive/u/0/folders/1X9Btw83RZR7UG2mIXDEtkY-1nt1r8aQr"

rodis_pii_dribble <- drive_get(rodis_pii_url)

rodis_pii_files <- drive_ls(path = rodis_pii_dribble)

download_multiple(file_dribble = get_file_ids("DeathStat"),
                  sub_dir = "death_stat",
                  overwrite = TRUE
)

download_multiple(file_dribble = get_file_ids("DeathNames"),
                  sub_dir = "death_name",
                  overwrite = TRUE
)

download_multiple(file_dribble = get_file_ids("CW Involvement and Risk for Injury Study"),
                  sub_dir = "famlink",
                  overwrite = FALSE
)


download_multiple(file_dribble = get_file_ids("bir"), 
                  sub_dir = "birth",
                  overwrite = TRUE
)

download_multiple(file_dribble = get_file_ids("chr"), 
                  sub_dir = "chars",
                  overwrite = TRUE
)
