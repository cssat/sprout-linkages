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

# Currently reads in data exported from CA_ODS.rodis.berd

library(aws.s3)
library(data.table)
library(tidyverse)
library(arrow)

# Run these lines one at a time (interactive mode)
# access_key_id <- readline("Enter your AWS access key ID (20-character code): ")
# secret_access_key <- readline("Enter your AWS secret access key: ")

# Sys.setenv(
#   "AWS_ACCESS_KEY_ID" = access_key_id,
#   "AWS_SECRET_ACCESS_KEY" = secret_access_key,
#   "AWS_DEFAULT_REGION" = "us-west-2")

# Or read your credential file
aws_credentials <- read_csv("H:/RODIS/CSSAT/AWS/ksegar_aws_credentials.csv")
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = aws_credentials$`Access key ID`,
  "AWS_SECRET_ACCESS_KEY" = aws_credentials$`Secret access key`,
  "AWS_DEFAULT_REGION" = "us-west-2")

rodis_pii_files <- data.table::rbindlist(get_bucket(bucket = "rodis-pii", max = Inf))
View(rodis_pii_files %>% filter(str_detect(Key,"output_data/run") == TRUE) %>% arrange(desc(LastModified)))
# 
# run_num <- rodis_pii_files %>% 
#   filter(str_detect(Key,"output_data/run") == TRUE) %>% 
#   mutate(current_run_date = max(LastModified)) %>% 
#   filter(LastModified == current_run_date) %>% 
#   mutate(run_num = str_extract(Key,"(?<=run-)[[:digit:]]+(?=-part)")) %>% 
#   group_by(run_num) %>% 
#   count() %>% 
#   pull(run_num)
# file_list <- pull(data.table::rbindlist(get_bucket(bucket = "rodis-pii")) %>% 
#   filter(str_detect(Key,run_num) & Owner == "partnersforourchildren"),
#   Key)

# Read set of output files from Glue job by run number, or default to most recent
read_s3_data <- function(run_num = data.table::rbindlist(get_bucket(bucket = "rodis-pii", max = Inf)) %>% 
                           filter(str_detect(Key,"output_data/run") == TRUE) %>% 
                           mutate(current_run_date = max(LastModified)) %>% 
                           filter(LastModified == current_run_date) %>% 
                           mutate(run_num = str_extract(Key,"(?<=run-)[[:digit:]]+(?=-part)")) %>% 
                           group_by(run_num) %>% 
                           count() %>% 
                           pull(run_num)) {
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

run_latest <- read_s3_data(run_num = "1640930597366")
# This takes several minutes to read all 480 files. Save the data in case R crashes again.
save(run_latest,file = "H:/RODIS/CSSAT/data_working/glue_run_1640930597366.Rdata")
load("H:/RODIS/CSSAT/data_working/glue_run_1640930597366.Rdata")

set_stats <- run_latest %>% 
  select(match_id, tx_record_relation) %>% 
  mutate(n = 1) %>% 
  pivot_wider(names_from = tx_record_relation,
              values_from = n,
              values_fn = sum,
              values_fill = 0) %>% 
  mutate(chars_pct = patient / (patient + death + famlink + child + mother + father),
         n = patient + death + famlink + child + mother + father)

matches_latest <- run_latest %>% 
  select(match_id) %>% 
  group_by(match_id) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n))
table(matches_latest$n)

match_over_20 <- matches_latest %>% 
  filter(n > 20) %>% 
  left_join(run_latest, by = "match_id")
#View(match_over_20)

# CHARS records with truncated names explain the groups with the highest n.
# How common is this?
chars_pct_over_20 <- match_over_20 %>% 
  select(match_id, tx_record_relation) %>% 
  mutate(n = 1) %>% 
  pivot_wider(names_from = tx_record_relation,
              values_from = n,
              values_fn = sum,
              values_fill = 0) %>% 
  mutate(chars_pct = patient / (patient + death + famlink + child + mother + father),
         n = patient + death + famlink + child + mother + father)
# A lot of them, but some of the high n groups have other records

# We're not interested in any groups without birth or Famlink records
not_all_chars_over_20 <- chars_pct_over_20 %>% 
  filter(chars_pct < 1 & (famlink + child + mother + father > 0)) %>% 
  inner_join(run_latest, by = "match_id")
#View(not_all_chars_over_20)

# I'm not seeing records from CHARS with truncated names in the high-n sets
# with matches to birth records or Famlink.
# Are they unmatched, or in CHARS/death-only sets?
chars_truncated <- run_latest %>% 
  filter(tx_record_relation == "patient" & nchar(tx_last_name) == 2 &
           nchar(tx_first_name) == 2)
chars_truncated_chars_pct <- matches_latest %>% 
  filter(match_id %in% chars_truncated$match_id) %>% 
  inner_join(run_latest, by = "match_id") %>%
  select(match_id, tx_record_relation) %>% 
  mutate(n = 1) %>% 
  pivot_wider(names_from = tx_record_relation,
              values_from = n,
              values_fn = sum,
              values_fill = 0) %>% 
  mutate(chars_pct = patient / (patient + death + famlink + child + mother + father),
         n = patient + death + famlink + child + mother + father)
chars_truncated_detail <- matches_latest %>% 
  filter(match_id %in% chars_truncated$match_id) %>%
  inner_join(chars_truncated_chars_pct, by = "match_id") %>% 
  inner_join(run_latest, by = "match_id") %>% 
  arrange(match_id, id_conglomerate)
# OK, they are in low-n sets. But I see another problem: sets with more than
# one child record.
set_multiple_births <- set_stats %>% 
  filter(child > 1) %>% 
  inner_join(run_latest, by = "match_id")
# Twins are getting lumped together, even opposite sex. Let's see if the 
# run at .9 precision does this better.

run_latest_9 <- read_s3_data("1641022561001")
# This takes several minutes to read all 480 files. Save the data in case R crashes again.
save(run_latest_9,file = "H:/RODIS/CSSAT/data_working/glue_run_1641022561001.Rdata")
load("H:/RODIS/CSSAT/data_working/glue_run_1641022561001.Rdata")

set_stats_9 <- run_latest_9 %>% 
  select(match_id, tx_record_relation) %>% 
  mutate(n = 1) %>% 
  pivot_wider(names_from = tx_record_relation,
              values_from = n,
              values_fn = sum,
              values_fill = 0) %>% 
  mutate(chars_pct = patient / (patient + death + famlink + child + mother + father),
         n = patient + death + famlink + child + mother + father)

matches_latest_9 <- run_latest_9 %>% 
  select(match_id) %>% 
  group_by(match_id) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n))
table(matches_latest_9$n)

chars_truncated_9 <- run_latest_9 %>% 
  filter(tx_record_relation == "patient" & nchar(tx_last_name) == 2 &
           nchar(tx_first_name) == 2)
chars_truncated_detail_9 <- matches_latest_9 %>% 
  filter(match_id %in% chars_truncated_9$match_id) %>%
  inner_join(set_stats_9, by = "match_id") %>% 
  inner_join(run_latest_9, by = "match_id") %>% 
  arrange(match_id, id_conglomerate)
# I'm not seeing a bunch of twins in the matches anymore. Is the problem solved?
table(chars_truncated_detail_9$child)
# Not entirely
View(chars_truncated_detail_9 %>% filter(child > 1))
# There are two match sets here, one with twins, and the other with two
# births on the same day that could match to two CHARS births, but are
# obviously not the same person because they have a different mother.
# This makes me wonder what is happening with CHARS records for twins
# with the same truncated name in the years of name truncation.

# Same-sex twins with similar names
twins <- run_latest_9 %>% 
  filter(tx_record_relation == "child") %>% 
  group_by(tx_reported_sex_or_gender, tx_last_2, tx_first_2, dt_birth_yr, dt_birth_mo, dt_birth_da,
           tx_mom_last_name, tx_mom_first_name) %>% 
  mutate(same_day_births = n()) %>% 
  ungroup() %>% 
  filter(same_day_births > 1) %>% 
  arrange(tx_reported_sex_or_gender, tx_last_2, tx_first_2, dt_birth_yr, dt_birth_mo, dt_birth_da,
          tx_mom_last_name, tx_mom_first_name)

# There may be something wrong with this query. See the next one instead.
# twinsish_9 <- run_latest_9 %>% 
#   filter(id_conglomerate %in% chars_truncated_9$id_conglomerate) %>% 
#   inner_join(set_stats_9, by = "match_id") %>% 
#   filter(child > 0) %>% 
#   mutate(tx_reported_sex_or_gender = ifelse(tx_reported_sex_or_gender == "" |
#                                               is.na(tx_reported_sex_or_gender),
#                                             "N",
#                                             tx_reported_sex_or_gender)) %>% 
#   group_by(tx_reported_sex_or_gender, tx_last_name, tx_first_name, dt_birth_yr, dt_birth_mo, dt_birth_da) %>% 
#   mutate(n_birth = n()) %>% 
#   ungroup() %>% 
#   filter(n_birth > 1) %>% 
#   bind_rows(twins) %>% 
#   arrange(tx_reported_sex_or_gender, tx_last_2, tx_first_2, dt_birth_yr, dt_birth_mo, dt_birth_da, match_id) %>% 
#   select(match_id,id_conglomerate,tx_reported_sex_or_gender,tx_last_name,tx_first_name, dt_birth_yr, 
#          dt_birth_mo, dt_birth_da, everything())
# Looked through the first 1000 records and didn't see any CHARS records with the same DOB as twins
# with names that might match. Let's start with the birth record twins and look for CHARS matches
# by truncated name, DOB, and gender.
twins_chars <- twins %>% 
  left_join(run_latest_9 %>% 
              filter(tx_record_relation == "patient" & nchar(tx_last_name) == 2 &
                       nchar(tx_first_name) == 2),
            by = c("tx_reported_sex_or_gender", "tx_last_2", "tx_first_2", 
                   "dt_birth_yr", "dt_birth_mo", "dt_birth_da")) %>% 
  arrange(dt_birth_yr, dt_birth_mo, dt_birth_da, tx_reported_sex_or_gender,
          tx_last_2, tx_first_2) %>% 
  select(match_id.x, match_id.y, everything()) %>% 
  filter(match_id.x == match_id.y)
# The only set that matched a truncated CHARS record to a twin is the one that lumped
# the twins' birth records together. Otherwise, the CHARS records are their own match set.

# Can we rely on birth and CHARS records to be in the same order for twins?
twin_order <- run_latest_9 %>% 
  filter(match_id %in% twins$match_id & dt_birth_yr > 2008) %>% 
  arrange(dt_birth_yr, dt_birth_mo, dt_birth_da, tx_last_name, tx_record_relation, id_source_record) %>% 
  select(match_id, everything())

# What do twin matches look like in old BERD table?
# Need to run this to generate birth_berd.
source("rodis_berd.R")

# This code is from birth_berd_check.R.
berd_bcert <- read_sas("../data_working/berd_bc_uni.sas7bdat") %>% 
  mutate(bcert = ifelse(bcertnum>0,
                        (file_year * 1000000) + bcertnum,
                        -1 * ((file_year * 1000000) - bcertnum)))

compare <- birth_berd %>% 
  select(bcert:sga) %>% 
  full_join(berd_bcert %>% select(bcert,bc_uni,fdid),
            by = "bcert")
# end birth_berd_check.R excerpt.

old_berd_dx <- read_csv("../data_working/old_rodis_berd_dx_5.csv",
                        na = "NULL")

compare_twins <- compare %>% 
  group_by(birthyr,birthmo,birthdy,mombiryr,mombirmo,mombirdy) %>% 
  mutate(birth_tot = n()) %>% 
  ungroup() %>% 
  filter(birth_tot > 1)

# We only care about twins who can't be differentiated by truncated names.
twin_dx <- compare_twins %>% 
  left_join(old_berd_dx, by = "bc_uni") %>% 
  mutate(id_source_record = as.character(bcert)) %>% 
  left_join(run_latest_9, by = "id_source_record") %>% 
  filter(tx_record_relation == "child") %>%
  group_by(tx_reported_sex_or_gender,dt_birth_yr, dt_birth_mo, dt_birth_da, tx_last_2, tx_first_2) %>% 
  mutate(trunc_name = n()) %>% 
  filter(trunc_name > 1) %>% 
  arrange(birthyr,birthmo,birthdy,mombiryr,mombirmo,mombirdy)
View(twin_dx %>% select(bcert, birthyr,birthmo,birthdy,mombiryr,mombirmo,mombirdy,
     cbdiag1:cbdiag5, cmdiag1:cmdiag5))

# Compare unmatched records at .9 precision to those at .8. Are their matches at
# .8 good or bad? Use as training data.
singletons_9 <- run_latest_9 %>% 
  inner_join(set_stats_9, by = "match_id") %>% 
  filter(n == 1) %>% 
  mutate(unmatched_9 = 1)
singletons_8 <- run_latest %>% 
  inner_join(set_stats, by = "match_id") %>% 
  filter(n == 1) %>% 
  mutate(unmatched_8 = 1)

# Look at matches found for unmatched records at .9 precision when
# precision is lowered to .8
singletons_match_8 <- singletons_9 %>% 
  select(id_conglomerate) %>%
  mutate(singleton = 1) %>% 
  filter(!id_conglomerate %in% singletons_8$id_conglomerate) %>% 
  inner_join(run_latest, by = "id_conglomerate")
# CHARS records from years with name truncation are still winding up
# off by themselves, even at .8 precision. Add some more to the training
# data.
singleton_groups <- run_latest %>% 
  filter(match_id %in% singletons_match_8$match_id) %>% 
  mutate(singleton = ifelse(id_conglomerate %in% singletons_match_8$id_conglomerate,
                            1,0)) %>% 
  select(match_id,singleton,everything()) %>% 
  arrange(match_id,dt_birth_yr,dt_birth_mo,dt_birth_da,tx_last_name)
singleton_groups_additional_chars <- singleton_groups %>% 
  select(dt_birth_yr,dt_birth_mo,dt_birth_da,tx_first_2,tx_last_2) %>% 
  distinct() %>% 
  rename(tx_first_name = tx_first_2,
         tx_last_name = tx_last_2) %>% 
  inner_join(run_latest, 
             by = c("dt_birth_yr","dt_birth_mo","dt_birth_da","tx_first_name","tx_last_name")) %>% 
  filter(!id_conglomerate %in% singleton_groups$id_conglomerate &
           dt_birth_yr >= 1999)
write_csv(bind_rows(singleton_groups,singleton_groups_additional_chars) %>% 
                      arrange(dt_birth_yr,dt_birth_mo,dt_birth_da,tx_last_name,tx_first_name,match_id) %>% 
            filter(dt_birth_yr >= 1999),
          "H:/RODIS/CSSAT/data_working/rodis_people_new_matches_precision_8.csv",
          na="")
table(singleton_groups$singleton)


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

match10 <- matches %>% 
  filter(n == 10) %>% 
  arrange(match_id) %>% 
  mutate(match10_num = row_number()) %>% 
  inner_join(run_1640772953488, by = "match_id") 
View(match10)

# Let's make some labels out of these sets of 10.

labels_1_20 <- match10 %>% 
  filter(match10_num <= 20) %>% 
  select(id_conglomerate:dt_cdob_da)
write_csv(labels_1_20,"../AWS/rodis_people_glue/run_1640772953488_10set_1_20.csv",
          na = "")