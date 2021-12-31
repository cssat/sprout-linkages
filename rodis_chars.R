######################################################################
#                                                                    #
# Project:  RODIS                                                    #
#                                                                    #
# File: rodis_chars.R                                                #
#                                                                    #
# Purpose: Read the CHARS data and process it #
#                                                                    #
# Procedures:  #
#                                                                    #
# Programmers: Karen Segar                                           #
#                                                                    #
# Dates: 11/6/2021 created                                           #
#                                                                    #
# Notes: We have never used the outpatient data before and have      #
#        decided not to use it for now. Maybe we'll revisit it later.#
#                                                                    #
######################################################################

library(tidyverse)
library(lubridate)
library(haven)
library(readxl)

read_chars_data <- function(pii_type) {
  #create a list of the files from your target directory
  file_list <- list.files(path = paste0(getwd(),
                                        "/raw_data/",
                                        pii_type),
                          full.names = TRUE)
  
  #initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
  dataset <- tibble()
  
  #had to specify columns to get rid of the total column
  for (i in 1:length(file_list)) {
    message(paste0("reading file ", file_list[i]))
    
    temp_data <- read_sas(file_list[i]) %>%
      mutate(source_data = file_list[i])
    names(temp_data) <- toupper(names(temp_data))
    dataset <-
      bind_rows(list(dataset, temp_data)) #for each iteration, bind the new data to the building dataset
  }
  return(dataset)
}

chars_raw <- read_chars_data("chars")
save(chars_raw, file = "chars.Rdata")
#load("chars.Rdata")

chars_inpt_raw <- chars_raw %>% 
  mutate(year = as.numeric(str_extract(source_data,"(?<=chr_r)[[:digit:]]{4}")))

# Normalize diagnosis data, drop empty cells
chars_diag <- chars_raw %>% 
  select(SEQ_NO_ENC, DIAG1:DIAG25,POA1:POA25) %>% 
  pivot_longer(cols = DIAG1:POA25,
               names_to = c(".value","dxnum"),
               names_pattern = "([[:alpha:]]+)([[:digit:]]+)",
               values_drop_na = TRUE) %>% 
  filter(!DIAG == "") %>% 
  mutate(dxnum = as.numeric(dxnum),
         POA = ifelse(POA=="",NA,POA))

# Normalize ecode data, drop empty cells
chars_ecode <- chars_raw %>% 
  select(SEQ_NO_ENC, ECODE1:ECODE10, POAE1:POAE10) %>% 
  pivot_longer(cols = ECODE1:POAE10,
               names_to = c(".value","ecodenum"),
               names_pattern = "([[:alpha:]]+)([[:digit:]]+)",
               values_drop_na = TRUE) %>% 
  filter(!ECODE == "") %>% 
  mutate(ecodenum = as.numeric(ecodenum),
         POAE = ifelse(POAE=="",NA,POAE))

# Normalize procedure data, drop empty cells
chars_proc <- chars_raw %>% 
  select(SEQ_NO_ENC, PROC1:PROC25, PRDATE1:PRDATE25, PRDAY1:PRDAY25) %>% 
  pivot_longer(cols = PROC1:PRDAY25,
               names_to = c(".value","prnum"),
               names_pattern = "([[:alpha:]]+)([[:digit:]]+)",
               values_drop_na = TRUE) %>% 
  filter(!PROC == "") %>% 
  mutate(prnum = as.numeric(prnum))

# We are not using the outpatient files for now, and they weren't in the Google drive download.
#chars_outpt_raw <- read_chars_data("chars_outpt") %>% 
#  mutate(year = as.numeric(str_extract(source_data,"(?<=chro_r)[[:digit:]]{4}")))

