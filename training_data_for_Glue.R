######################################################################
#                                                                    #
# Project:  RODIS                                                    #
#                                                                    #
# File: training_data_for_Glue.R                                     #
#                                                                    #
# Purpose: Read spreadsheet with notes on fuzzy matches from Link    #
#          Plus and retrieve data to copy into label file for Glue   #
#                                                                    #
# Procedures: The spreadsheet with the matches was produced manually #
#             and isn't an immediate product of any of the scripts   #
#             in the sprout-linkages project. Edit the file location #
#             accordingly. Run prep_for_glue.R to generate needed    #
#             data.
#                                                                    #
# Programmers: Karen Segar                                           #
#                                                                    #
# Dates: 4/12/2021 created                                           #
#                                                                    #
######################################################################

library(tidyverse)
library(readxl)

# Get match sets from notes on issues with Glue runs.
match_sets_for_training <- read_xlsx("H:/RODIS/CSSAT/data_working/rodis_people_precision_8_notes.xlsx",
                                     col_types = c("text","text","text")) %>% 
  mutate(run = "1640930597366") %>% 
  bind_rows(read_xlsx("H:/RODIS/CSSAT/data_working/rodis_people_precision_9_notes.xlsx",
                      col_types = c("text","text","text"))) %>% 
  mutate(run = ifelse(is.na(run),
                  "1641022561001",
                  run))

load("H:/RODIS/CSSAT/data_working/glue_run_1640930597366.Rdata")
training_8 <- run_latest %>% 
  filter(match_id %in% match_sets_for_training$match_id[match_sets_for_training$run == "1640930597366"])

load("H:/RODIS/CSSAT/data_working/glue_run_1641022561001.Rdata")
training_9 <- run_latest_9 %>% 
  filter(match_id %in% match_sets_for_training$match_id[match_sets_for_training$run == "1641022561001"])

write_csv(bind_rows(training_8,training_9),
          "H:/RODIS/CSSAT/data_working/training_20220111.csv",
          na = "")
# Manually assign labelling set IDs and labels, then add the file to exported labels and join to
# new rodis_people_glue file with new columns.
  
# Labels for new transform working with admission/discharge dates and 
# dx from CHARS.
labels <- read_csv("H:/RODIS/CSSAT/AWS/rodis_people_glue/tfm-2cddc271329f9b656404632009a9f69ef4fb961d_labels_2022-01-12_00_17_57_c042df6e-c93b-45f1-88f3-402399cf10dc.csv",
                   col_types = cols(tx_suffix_name = col_character(),
                                    dt_mom_dob_mo = col_integer(),
                                    dt_mom_dob_da = col_integer(),
                                    dt_dad_dob_mo = col_integer(),
                                    dt_dad_dob_da = col_integer(),
                                    dt_birth_mo = col_integer(),
                                    dt_birth_da = col_integer(),
                                    dt_cdob_mo = col_integer(),
                                    dt_cdob_da = col_integer(),
                                    tx_reported_sex_or_gender = col_character())) %>% 
  bind_rows(read_csv("H:/RODIS/CSSAT/data_working/rodis_people_new_matches_precision_8.csv",
                     col_types = cols(tx_suffix_name = col_character(),
                                      dt_mom_dob_mo = col_integer(),
                                      dt_mom_dob_da = col_integer(),
                                      dt_dad_dob_mo = col_integer(),
                                      dt_dad_dob_da = col_integer(),
                                      dt_birth_mo = col_integer(),
                                      dt_birth_da = col_integer(),
                                      dt_cdob_mo = col_integer(),
                                      dt_cdob_da = col_integer(),
                                      tx_reported_sex_or_gender = col_character()))) %>% 
  bind_rows(read_csv("H:/RODIS/CSSAT/data_working/training_20220111.csv",
                   col_types = cols(tx_suffix_name = col_character(),
                                    dt_mom_dob_mo = col_integer(),
                                    dt_mom_dob_da = col_integer(),
                                    dt_dad_dob_mo = col_integer(),
                                    dt_dad_dob_da = col_integer(),
                                    dt_birth_mo = col_integer(),
                                    dt_birth_da = col_integer(),
                                    dt_cdob_mo = col_integer(),
                                    dt_cdob_da = col_integer(),
                                    tx_reported_sex_or_gender = col_character()))) %>%
  bind_rows(read_csv("H:/RODIS/CSSAT/AWS/rodis_people_glue/tfm-2cddc271329f9b656404632009a9f69ef4fb961d_labels_2022-01-21_03_59_07_375a65ee-beb8-4379-bf96-8a7e0510d910.csv",
                     col_types = cols(tx_suffix_name = col_character(),
                                      dt_mom_dob_mo = col_integer(),
                                      dt_mom_dob_da = col_integer(),
                                      dt_dad_dob_mo = col_integer(),
                                      dt_dad_dob_da = col_integer(),
                                      dt_birth_mo = col_integer(),
                                      dt_birth_da = col_integer(),
                                      dt_cdob_mo = col_integer(),
                                      dt_cdob_da = col_integer(),
                                      tx_reported_sex_or_gender = col_character()))) %>% 
  filter(!is.na(labeling_set_id))

load("rodis_people_glue.Rdata")

# There is some duplication in the labels file (two exports from Glue with the same labels).
# Select distinct labelling sets.
label_rodis_people_glue <- labels %>% 
  distinct(labeling_set_id, label, id_conglomerate) %>% 
  inner_join(rodis_people_glue, by = "id_conglomerate")
write_csv(label_rodis_people_glue,"../AWS/rodis_people_glue/rodis_people_labels_20220122.csv",na="")
write_csv(label_rodis_people_glue %>% 
            filter(tx_record_relation == "patient") %>% 
            group_by(labeling_set_id) %>% 
            mutate(labeling_set_n = n()) %>% 
            filter(labeling_set_n > 1) %>% 
            ungroup() %>% 
            select(-labeling_set_n),
          "../AWS/rodis_people_glue/rodis_chars_labels_20220122.csv",na="")

# 1/22/2022 don't need anything beyond here anymore
# Get labels exported from Glue
labels <- read_csv("H:/RODIS/CSSAT/AWS/rodis_people_glue/tfm-2cddc271329f9b656404632009a9f69ef4fb961d_labels_2021-12-10_04_39_26_9fcb5dfa-2452-44ef-89f7-cff2195ef087.csv",
                   col_types = cols(tx_suffix_name = col_character(),
                                    dt_mom_dob_mo = col_integer(),
                                    dt_mom_dob_da = col_integer(),
                                    dt_dad_dob_mo = col_integer(),
                                    dt_dad_dob_da = col_integer(),
                                    dt_birth_mo = col_integer(),
                                    dt_birth_da = col_integer(),
                                    dt_cdob_mo = col_integer(),
                                    dt_cdob_da = col_integer(),
                                    tx_reported_sex_or_gender = col_character())) %>% 
  bind_rows(read_csv("H:/RODIS/CSSAT/AWS/rodis_people_glue/CHARS_1999_Famlink_link_plus_training.csv",
                     col_types = cols(tx_suffix_name = col_character(),
                                      dt_mom_dob_mo = col_integer(),
                                      dt_mom_dob_da = col_integer(),
                                      dt_dad_dob_mo = col_integer(),
                                      dt_dad_dob_da = col_integer(),
                                      dt_birth_mo = col_integer(),
                                      dt_birth_da = col_integer(),
                                      dt_cdob_mo = col_integer(),
                                      dt_cdob_da = col_integer(),
                                      tx_reported_sex_or_gender = col_character())))
#load("rodis_people_glue.Rdata")
label_rodis_people_glue <- labels %>% 
  select(-id_source_record:-dt_cdob_da) %>% 
  inner_join(rodis_people_glue, by = "id_conglomerate")
write_csv(label_rodis_people_glue,"rodis_people_labels_20211209.csv",na="")

# 12/9/2021: don't need to do this again, code above supersedes this
labels <- read_csv("H:/RODIS/CSSAT/AWS/rodis_people_glue/tfm-2cddc271329f9b656404632009a9f69ef4fb961d_labels_2021-12-02_21_47_04_5e691af1-8f16-47d3-9f7a-7435fc51d19d.csv",
                   col_types = cols(tx_suffix_name = col_character(),
                                    dt_mom_dob_mo = col_integer(),
                                    dt_mom_dob_da = col_integer(),
                                    dt_dad_dob_mo = col_integer(),
                                    dt_dad_dob_da = col_integer(),
                                    dt_birth_mo = col_integer(),
                                    dt_birth_da = col_integer(),
                                    dt_cdob_mo = col_integer(),
                                    dt_cdob_da = col_integer(),
                                    tx_reported_sex_or_gender = col_character())) %>% 
  bind_rows(read_csv("H:/RODIS/CSSAT/AWS/rodis_people_glue/tfm-2cddc271329f9b656404632009a9f69ef4fb961d_labels_2021-12-03_02_58_30_daf215d6-b9f6-480c-83cf-2d2fa08648ed.csv",
                     col_types = cols(tx_suffix_name = col_character(),
                                      dt_mom_dob_mo = col_integer(),
                                      dt_mom_dob_da = col_integer(),
                                      dt_dad_dob_mo = col_integer(),
                                      dt_dad_dob_da = col_integer(),
                                      dt_birth_mo = col_integer(),
                                      dt_birth_da = col_integer(),
                                      dt_cdob_mo = col_integer(),
                                      dt_cdob_da = col_integer(),
                                      tx_reported_sex_or_gender = col_character())))
load("rodis_people_glue.Rdata")
label_rodis_people_glue <- labels %>% 
  select(-id_source_record:-dt_cdob_da) %>% 
  inner_join(rodis_people_glue, by = "id_conglomerate")
write_csv(label_rodis_people_glue,"rodis_people_labels_20211205.csv",na="")

# 9/21/2021: don't need to do this again, code above supersedes this
labels <- read_csv("H:/RODIS/CSSAT/AWS/mother_daughter_dadinfo/tfm-f9f58acf2126b48b8dd4278900954b8d4340d5f2_labels_2021-09-21_18_24_15_41ab9297-9d00-43b8-b0f8-463a2de097a6.csv",
                              col_types = cols(tx_suffix_name = col_character(),
                                               tx_reported_sex_or_gender = col_character())) %>% 
  bind_rows(read_csv("H:/RODIS/CSSAT/AWS/father_son/tfm-ed4001818b3f44ac0cc80ec7125246f6a51939a2_labels_2021-09-21_18_34_38_8a631f14-b8aa-488e-9515-fb9abe8cbd97.csv",
                                   col_types = cols(tx_suffix_name = col_character(),
                                                    tx_reported_sex_or_gender = col_character())))
#load("rodis_people_glue.Rdata")
label_rodis_people_glue <- labels %>% 
  select(-id_source_record:-dt_cdob) %>% 
  inner_join(rodis_people_glue, by = "id_conglomerate")
write_csv(label_rodis_people_glue,"birth_labels.csv",na="")

######################################################################
# The code below documents training data created prior to 9/21/2021. #
# These data have been consolidated above and the code below does    #
# not need to be run anymore.                                        #
######################################################################

matches_mom_daughter <- read_xlsx("H:/RODIS/CSSAT/data_working/possible_training_cases_for_glue.xlsx")

load(paste0(getwd(),"/rodis_people_mother_daughter.Rdata"))

matches_long_mom_daughter <- matches_mom_daughter %>% 
  select(file,`Link ID`,real_match,id_conglomerate1,id_conglomerate2) %>% 
  pivot_longer(cols=starts_with("id"),names_to = "id_which",
               values_to = "id_conglomerate") %>% 
  inner_join(rodis_people_mother_daughter,by="id_conglomerate")

matches_dad_son <- read_xlsx("H:/RODIS/CSSAT/data_working/father_son_training_cases.xlsx")

load(paste0(getwd(),"/rodis_people_father_son.Rdata"))


# This was the first training file
#write_csv(matches_long,"link_plus_training_data.csv",na="")

# Second training file
#write_csv(matches_long %>% 
#            filter(file=="birth_female_2002_2004_dedupe_cutoff_1.txt") %>% 
#            select(-file),
#          "link_plus_training_data_2.csv",na="")

# Third training file
#write_csv(matches_long %>% 
#            filter(file=="birth_female_2015_2017_dedupe_cutoff_1.txt") %>% 
#            select(-file),
#          "link_plus_training_data_3.csv",na="")

# Training file from "match" sets from first attempt to run a job 
# using the trained transform.
female_birth_job1 <- read_csv("H:/RODIS/CSSAT/AWS/tfm-6350252b5b2da904d4dd8aeeb977675795839ff4_round_1_failures.csv",
                              col_types = cols(tx_suffix_name = col_character(),
                                               tx_reported_sex_or_gender = col_character())) %>% 
  filter(!is.na(labeling_set_id)) %>% 
  select(labeling_set_id:tx_reported_sex_or_gender)
#write_csv(female_birth_job1,
#          "tfm-6350252b5b2da904d4dd8aeeb977675795839ff4_round_1_failures_labelled.csv",
#          na="")

# Training file from "match" sets from run #3 after tuning the job to .9 precision-recall trade-off.
female_birth_job3 <- read_csv("H:/RODIS/CSSAT/AWS/tfm-6350252b5b2da904d4dd8aeeb977675795839ff4_round_3_failures_2.csv",
                              col_types = cols(tx_suffix_name = col_character(),
                                               tx_reported_sex_or_gender = col_character())) %>% 
  filter(!is.na(labeling_set_id)) %>% 
  select(labeling_set_id:tx_reported_sex_or_gender)
write_csv(female_birth_job3,
          "tfm-6350252b5b2da904d4dd8aeeb977675795839ff4_round_3_failures_2_labelled.csv",
          na="")

female_birth_job3 <- read_csv("H:/RODIS/CSSAT/AWS/tfm-6350252b5b2da904d4dd8aeeb977675795839ff4_round_4_failures.csv",
                              col_types = cols(tx_suffix_name = col_character(),
                                               tx_reported_sex_or_gender = col_character())) %>% 
  filter(!is.na(labeling_set_id)) %>% 
  select(labeling_set_id:tx_reported_sex_or_gender)
write_csv(female_birth_job3,
          "tfm-6350252b5b2da904d4dd8aeeb977675795839ff4_round_4_failures_labelled.csv",
          na="")


############################################################################
# Male births and fathers

matches_long_dad_son <- matches_dad_son %>% 
  select(file,`Link ID`,real_match,id_conglomerate1,id_conglomerate2) %>% 
  pivot_longer(cols=starts_with("id"),names_to = "id_which",
               values_to = "id_conglomerate") %>% 
  inner_join(rodis_people_father_son,by="id_conglomerate")

write_csv(matches_long_dad_son,"father_son_link_plus_training_data.csv",na="")

male_birth_job1 <- read_csv("H:/RODIS/CSSAT/AWS/father_son/tfm-ed4001818b3f44ac0cc80ec7125246f6a51939a2_round_1_failures.csv",
                              col_types = cols(tx_suffix_name = col_character(),
                                               tx_reported_sex_or_gender = col_character())) %>% 
  filter(!is.na(labeling_set_id)) %>% 
  select(labeling_set_id:tx_reported_sex_or_gender)
write_csv(male_birth_job1,
          "H:/RODIS/CSSAT/AWS/father_son/tfm-ed4001818b3f44ac0cc80ec7125246f6a51939a2_round_1_failures_labelled.csv",
          na="")


