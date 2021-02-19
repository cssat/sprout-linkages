# install.packages("googledrive")
# install.packages("dplyr")

# For general purpose
library(tidyverse)
source('~/rodis/helpers.R')

# for pulling files down from Google Drive
# and writing them back
library(googledrive)

# for reading files in
library(haven)
library(readxl)


# create people files blocked on gender 
# for subsequent linkages on AWS glue

children <- read_linkage_data(pii_type = "birth") %>%
  mutate(
    id_source_record = CERT_NUM,
    tx_record_relation = "child",
    id_death_certificate = ifelse(DEATH_CERT_NUM_OOS != "000000", DEATH_CERT_NUM_OOS, 
                                  ifelse(DEATH_CERT_NUM != "0000000000", DEATH_CERT_NUM, NA
                                  )),
    tx_last_name = PERSON_LAST_NAME,
    tx_maiden_name = NA,
    tx_first_name = PERSON_FIRST_NAME,
    tx_middle_name = PERSON_MIDDLE_NAME,
    tx_suffix_name = PERSON_SUFFIX,
    dt_birth = PERSON_DOB,
    dt_death = DATE_OF_DEATH,
    tx_zip_code = RES_ZIP_CODE,
    tx_reported_sex_or_gender = SEX,
    fl_any_hispanic = ifelse(PERSON_HISP_CALC %in% c("0","9") | PERSON_RACE_CALC %in% c("C"), 0, 1),
    fl_any_african_american = ifelse(PERSON_RACE_CALC %in% c("2"), 1, 0),
    fl_any_american_indian = ifelse(PERSON_RACE_CALC %in% c("3"), 1, 0),
    fl_any_asian = ifelse(PERSON_RACE_CALC %in% c("5", "D", "E", "G"), 1, 0),
    fl_any_pac_island = ifelse(PERSON_RACE_CALC %in% c("A", "B", "F", "H", "7"), 1, 0),
    fl_any_white = ifelse(PERSON_RACE_CALC %in% c("1"), 0, 1),
    tx_dad_last_name = FA_LAST_NAME,
    tx_dad_first_name = FA_FIRST_NAME,
    tx_dad_middle_name = FA_MIDDLE_NAME,
    tx_dad_suffix_name = FA_SUFFIX,
    tx_dad_dob = FA_BIOLOGICAL_DOB,
    fl_any_hispanic_dad = ifelse(FA_HISPANIC %in% c("0","9") | FA_RACE %in% c("C"), 0, 1),
    fl_any_african_american_dad = ifelse(FA_RACE %in% c("2"), 1, 0),
    fl_any_american_indian_dad = ifelse(FA_RACE %in% c("3"), 1, 0),
    fl_any_asian_dad = ifelse(FA_RACE %in% c("5", "D", "E", "G"), 1, 0),
    fl_any_pac_island_dad = ifelse(FA_RACE %in% c("A", "B", "F", "H", "7"), 1, 0),
    fl_any_white_dad = ifelse(FA_RACE %in% c("1"), 1, 0),
    tx_mom_last_name = MO_LEGAL_NAME,
    tx_mom_maiden_name = MO_MAIDEN_NAME,
    tx_mom_first_name = MO_FIRST_NAME,
    tx_mom_middle_name = MO_MIDDLE_NAME,
    tx_mom_dob = MO_BIOLOGICAL_DOB,
    fl_any_hispanic_mom = ifelse(MO_HISPANIC %in% c("0","9") | MO_RACE %in% c("C"), 0, 1),
    fl_any_african_american_mom = ifelse(MO_RACE %in% c("2"), 1, 0),
    fl_any_american_indian_mom = ifelse(MO_RACE %in% c("3"), 1, 0),
    fl_any_asian_mom = ifelse(MO_RACE %in% c("5", "D", "E", "G"), 1, 0),
    fl_any_pac_island_mom = ifelse(MO_RACE %in% c("A", "B", "F", "H", "7"), 1, 0),
    fl_any_white_mom = ifelse(MO_RACE %in% c("1"), 1, 0),  
    fl_child_death_cert_oos = ifelse(DEATH_CERT_NUM_OOS != "000000", 1, 0),
    dt_youngest_maternal_sibling_birth = DATE_LAST_BIRTH,
    fl_child_death_in_hospital = ifelse(PERSON_ALIVE_AT_DISCHARGE == "N", 1, 0)
  ) %>%
  select(
    id_source_record,
    source_data,
    tx_record_relation,
    id_death_certificate,
    tx_last_name,
    tx_maiden_name,
    tx_first_name,
    tx_middle_name,
    tx_suffix_name,
    fl_any_hispanic,
    fl_any_african_american,
    fl_any_american_indian,
    fl_any_asian,
    fl_any_pac_island,
    fl_any_white,   
    dt_birth,
    dt_death,
    tx_zip_code,
    tx_reported_sex_or_gender,
    tx_dad_last_name,
    tx_dad_first_name,
    tx_dad_middle_name,
    tx_dad_suffix_name,
    fl_any_hispanic_dad,
    fl_any_african_american_dad,
    fl_any_american_indian_dad,
    fl_any_asian_dad,
    fl_any_pac_island_dad,
    fl_any_white_dad,       
    tx_dad_dob,
    tx_mom_last_name,
    tx_mom_maiden_name,
    tx_mom_first_name,
    tx_mom_middle_name, 
    fl_any_hispanic_mom,
    fl_any_african_american_mom,
    fl_any_american_indian_mom,
    fl_any_asian_mom,
    fl_any_pac_island_mom,
    fl_any_white_mom,      
    tx_mom_dob,
    fl_child_death_cert_oos,
    dt_youngest_maternal_sibling_birth,
    fl_child_death_in_hospital
  )

mothers <- children %>%
  mutate(
    tx_record_relation = "mother",
    id_death_certificate = NA,
    tx_last_name = tx_mom_last_name,
    tx_maiden_name = tx_mom_maiden_name,
    tx_first_name = tx_mom_first_name,
    tx_middle_name = tx_mom_middle_name,
    tx_suffix_name = NA,
    dt_birth = tx_mom_dob,
    dt_death = NA,
    tx_reported_sex_or_gender = "F",
    fl_any_hispanic = fl_any_hispanic_mom,
    fl_any_african_american = fl_any_african_american_mom,
    fl_any_american_indian = fl_any_american_indian_mom,
    fl_any_asian = fl_any_asian_mom,
    fl_any_pac_island = fl_any_pac_island_mom,
    fl_any_white = fl_any_white_mom,
    tx_dad_last_name = NA,
    tx_dad_first_name = NA,
    tx_dad_middle_name = NA,
    tx_dad_suffix_name = NA,
    tx_dad_dob = NA,
    fl_any_hispanic_dad = NA,
    fl_any_african_american_dad = NA,
    fl_any_american_indian_dad = NA,
    fl_any_asian_dad = NA,
    fl_any_pac_island_dad = NA,
    fl_any_white_dad = NA,
    tx_mom_last_name = NA,
    tx_mom_maiden_name = NA,
    tx_mom_first_name = NA,
    tx_mom_middle_name = NA,
    tx_mom_dob = NA,
    fl_any_hispanic_mom = NA,
    fl_any_african_american_mom = NA,
    fl_any_american_indian_mom = NA,
    fl_any_asian_mom = NA,
    fl_any_pac_island_mom = NA,
    fl_any_white_mom = NA,
    fl_child_death_cert_oos = NA,
    dt_youngest_maternal_sibling_birth = NA,
    fl_child_death_in_hospital = NA
  ) 

fathers <- children %>%
  mutate(
    tx_record_relation = "father",
    id_death_certificate = NA,
    tx_last_name = tx_dad_last_name,
    tx_maiden_name = NA,
    tx_first_name = tx_dad_first_name,
    tx_middle_name = tx_dad_middle_name,
    tx_suffix_name = NA,
    dt_birth = tx_dad_dob,
    dt_death = NA,
    tx_reported_sex_or_gender = "M",
    fl_any_hispanic = fl_any_hispanic_dad,
    fl_any_african_american = fl_any_african_american_dad,
    fl_any_american_indian = fl_any_american_indian_dad,
    fl_any_asian = fl_any_asian_dad,
    fl_any_pac_island = fl_any_pac_island_dad,
    fl_any_white = fl_any_white_dad,
    tx_dad_last_name = NA,
    tx_dad_first_name = NA,
    tx_dad_middle_name = NA,
    tx_dad_suffix_name = NA,
    tx_dad_dob = NA,
    fl_any_hispanic_dad = NA,
    fl_any_african_american_dad = NA,
    fl_any_american_indian_dad = NA,
    fl_any_asian_dad = NA,
    fl_any_pac_island_dad = NA,
    fl_any_white_dad = NA,
    tx_mom_last_name = NA,
    tx_mom_maiden_name = NA,
    tx_mom_first_name = NA,
    tx_mom_middle_name = NA,
    tx_mom_dob = NA,
    fl_any_hispanic_mom = NA,
    fl_any_african_american_mom = NA,
    fl_any_american_indian_mom = NA,
    fl_any_asian_mom = NA,
    fl_any_pac_island_mom = NA,
    fl_any_white_mom = NA,
    fl_child_death_cert_oos = NA,
    dt_youngest_maternal_sibling_birth = NA,
    fl_child_death_in_hospital = NA
  ) 

patients <- read_linkage_data(pii_type = "chars") %>%
  mutate(
    id_source_record = SEQ_NO_ENC,
    tx_record_relation = "patient",
    id_death_certificate = NA,
    tx_last_name = LASTNAME,
    tx_maiden_name = NA,
    tx_first_name = FIRSTNAME,
    tx_middle_name = MINAME,
    tx_suffix_name = NA,
    dt_birth = as.character(format(DOB, "%Y%m%d")),
    dt_death = NA,
    tx_zip_code = ZIPCODE,
    tx_reported_sex_or_gender = SEX,
    fl_any_hispanic = ifelse(HISPANIC %in% c("Y"), 1, 0),
    fl_any_african_american = ifelse(RACE_BLK %in% c("Y"), 1, 0),
    fl_any_american_indian = ifelse(RACE_AMI %in% c("Y"), 1, 0),
    fl_any_asian = ifelse(RACE_AMI %in% c("Y"), 1, 0),
    fl_any_pac_island = ifelse(RACE_HAW %in% c("Y"), 1, 0),
    fl_any_white = ifelse(RACE_WHT %in% c("Y"), 1, 0),
    tx_dad_last_name = NA,
    tx_dad_first_name = NA,
    tx_dad_middle_name = NA,
    tx_dad_suffix_name = NA,
    tx_dad_dob = NA,
    fl_any_hispanic_dad = NA,
    fl_any_african_american_dad = NA,
    fl_any_american_indian_dad = NA,
    fl_any_asian_dad = NA,
    fl_any_pac_island_dad = NA,
    fl_any_white_dad = NA,
    tx_mom_last_name = NA,
    tx_mom_maiden_name = NA,
    tx_mom_first_name = NA,
    tx_mom_middle_name = NA,
    tx_mom_dob = NA,
    fl_any_hispanic_mom = NA,
    fl_any_african_american_mom = NA,
    fl_any_american_indian_mom = NA,
    fl_any_asian_mom = NA,
    fl_any_pac_island_mom = NA,
    fl_any_white_mom = NA,
    fl_child_death_cert_oos = NA,
    dt_youngest_maternal_sibling_birth = NA,
    fl_child_death_in_hospital = NA
  )  %>%
  select(
    id_source_record,
    source_data,
    tx_record_relation,
    id_death_certificate,
    tx_last_name,
    tx_maiden_name,
    tx_first_name,
    tx_middle_name,
    tx_suffix_name,
    fl_any_hispanic,
    fl_any_african_american,
    fl_any_american_indian,
    fl_any_asian,
    fl_any_pac_island,
    fl_any_white,   
    dt_birth,
    dt_death,
    tx_zip_code,
    tx_reported_sex_or_gender,
    tx_dad_last_name,
    tx_dad_first_name,
    tx_dad_middle_name,
    tx_dad_suffix_name,
    fl_any_hispanic_dad,
    fl_any_african_american_dad,
    fl_any_american_indian_dad,
    fl_any_asian_dad,
    fl_any_pac_island_dad,
    fl_any_white_dad,       
    tx_dad_dob,
    tx_mom_last_name,
    tx_mom_maiden_name,
    tx_mom_first_name,
    tx_mom_middle_name, 
    fl_any_hispanic_mom,
    fl_any_african_american_mom,
    fl_any_american_indian_mom,
    fl_any_asian_mom,
    fl_any_pac_island_mom,
    fl_any_white_mom,      
    tx_mom_dob,
    fl_child_death_cert_oos,
    dt_youngest_maternal_sibling_birth,
    fl_child_death_in_hospital
  )

rodis_people <- bind_rows(
  children,
  mothers,
  fathers,
  patients
)

rodis_people_father_son <- rodis_people %>%
  filter(
    tx_record_relation != "patient", 
    tx_reported_sex_or_gender %in% c("M", "U"),
    (fl_child_death_in_hospital == 1 & tx_record_relation == "child") == FALSE
  ) %>% 
  mutate(
    id_conglomerate = paste0(id_source_record, tx_record_relation)
  ) %>% 
  select(
    id_conglomerate,
    tx_last_name,
    tx_maiden_name,
    tx_first_name,
    tx_middle_name,
    tx_suffix_name,
    fl_any_hispanic,
    fl_any_african_american,
    fl_any_american_indian,
    fl_any_asian,
    fl_any_pac_island,
    fl_any_white,   
    dt_birth,
    tx_zip_code,
    tx_reported_sex_or_gender
  ) 


rodis_people_mother_daughter <- rodis_people %>%
  filter(
    tx_record_relation != "patient", 
    tx_reported_sex_or_gender %in% c("F", "U"),
    (fl_child_death_in_hospital == 1 & tx_record_relation == "child") == FALSE
  ) %>% 
  mutate(
    id_conglomerate = paste0(id_source_record, tx_record_relation)
  ) %>% 
  select(
    id_conglomerate,
    tx_last_name,
    tx_maiden_name,
    tx_first_name,
    tx_middle_name,
    tx_suffix_name,
    fl_any_hispanic,
    fl_any_african_american,
    fl_any_american_indian,
    fl_any_asian,
    fl_any_pac_island,
    fl_any_white,   
    dt_birth,
    tx_zip_code,
    tx_reported_sex_or_gender
  ) 

# Write data back to Google for linkage work on AWS Glue
# Demo Comment
write_csv(rodis_people_mother_daughter, "rodis_people_mother_daughter.csv")

drive_upload(
  media = "rodis_people_mother_daughter.csv",
  name = "rodis_people_mother_daughter.csv",
  path = rodis_pii_dribble
)

write_csv(rodis_people_father_son, "rodis_people_father_son.csv")

drive_upload(
  media = "rodis_people_father_son.csv",
  name = "rodis_people_father_son.csv",
  path = rodis_pii_dribble
)
