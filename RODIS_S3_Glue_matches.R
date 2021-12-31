######################################################################
#                                                                    #
# Project:  RODIS                                                    #
#                                                                    #
# File: RODIS_S3_Glue_matches.R                                      #
#                                                                    #
# Purpose: Read output files from S3 from job that finds matches in  #
#          birth, death, CHARS, and Famlink files                    #
#                                                                    #
# Procedures: Run Glue job to find matches, currently                #
#             rodis_people_match                                     #
#                                                                    #
# Programmers: Karen Segar                                           #
#                                                                    #
# Dates: 12/24/2021 created                                          #
#                                                                    #
######################################################################

library(aws.s3)
library(data.table)
library(tidyverse)
library(arrow)

# Run these lines one at a time (interactive mode)
access_key_id <- readline("Enter your AWS access key ID (20-character code): ")
secret_access_key <- readline("Enter your AWS secret access key: ")

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = access_key_id,
  "AWS_SECRET_ACCESS_KEY" = secret_access_key,
  "AWS_DEFAULT_REGION" = "us-west-2")

rodis_pii_files <- data.table::rbindlist(get_bucket(bucket = "rodis-pii", max = Inf))

run_num <- "1640772953488"
file_list <- pull(data.table::rbindlist(get_bucket(bucket = "rodis-pii")) %>% 
  filter(str_detect(Key,run_num) & Owner == "partnersforourchildren"),
  Key)

# Read set of output files from Glue job by run number
read_s3_data <- function(run_num) {
  #create a list of the files from your target directory
  file_list <- pull(data.table::rbindlist(get_bucket(bucket = "rodis-pii",
                                                     max = Inf)) %>% 
                      filter(str_detect(Key,run_num) & Owner == "partnersforourchildren"),
                    Key)
  
  # column types that need to be specified so read_csv doesn't guess wrong
    col_spec <- cols(id_conglomerate = col_character(),
                     id_source_record = col_character(),
                     tx_record_relation = col_character(),
                     tx_last_name = col_character(),
                     tx_maiden_name = col_character(),
                     tx_first_name = col_character(),
                     tx_middle_name = col_character(),
                     tx_suffix_name = col_character(),
                     tx_reported_sex_or_gender = col_character(),
                     match_id = col_double()
    )

  #initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
  dataset <- tibble()
  
  for (i in 1:length(file_list)) {
    message(paste0("reading file ", file_list[i]))
    
    temp_data <- s3read_using(FUN = read_csv,
                              col_types = col_spec,
                              object = paste0("s3://rodis-pii/",file_list[i])) %>%
      mutate(source_data = file_list[i])
    dataset <-
      bind_rows(list(dataset, temp_data)) #for each iteration, bind the new data to the building dataset
  }
  return(dataset)
}

run_1640772953488 <- read_s3_data(run_num = "1640772953488")

matches <- run_1640772953488 %>% 
  select(match_id) %>% 
  group_by(match_id) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n))

match_over_15 <- run_1640772953488 %>%
  inner_join(matches, by = "match_id") %>% 
  filter(n > 15) %>% 
  arrange(match_id)
View(match_over_15)

match10 <- run_1640772953488 %>%
  inner_join(matches, by = "match_id") %>% 
  filter(n == 10) %>% 
  arrange(match_id)
View(match10)
