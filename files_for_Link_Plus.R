######################################################################
#                                                                    #
# Project:  RODIS                                                    #
#                                                                    #
# File: files_for_Link_Plus.R                                        #
#                                                                    #
# Purpose: Make files small enough for Link Plus to handle in order  #
#          to find fuzzy matches that can be used to train transform #
#          in AWS Glue.                                              #
#                                                                    #
# Procedures: Run prep_for_glue.R first to generate needed data.     #
#             Don't run this whole script, just the beginning. The   #
#             rest is documentation of files already produced.       #
#                                                                    #
# Programmers: Karen Segar                                           #
#                                                                    #
# Dates: 4/12/2021 created                                           #
#                                                                    #
######################################################################

library(tidyverse)

setwd("U:/RODIS/CSSAT/sprout-linkages/")

load("./rodis_people_glue.Rdata")

# Comma-separated file for Link Plus
# Need a fake record at the top of the file so Link Plus can guess the data types of
# columns that have a lot of missing data, like maiden name.
fake_record <- data.frame(id_conglomerate = "1998900001mother",
                          id_source_record = "1998900001",
                          tx_record_relation = "mother",
                          tx_last_name = "FAKELASTNAME",
                          tx_maiden_name = "FAKEMAIDENNAME",
                          tx_first_name = "FAKEFIRSTNAME",
                          tx_middle_name = "FAKEMIDDLENAME",
                          tx_suffix_name = "JR",
                          tx_reported_sex_or_gender = "U",
                          tx_mom_last_name = "FAKELASTNAME",
                          tx_mom_maiden_name = "FAKEMAIDENNAME",
                          tx_mom_first_name = "FAKEFIRSTNAME",
                          tx_mom_middle_name = "FAKEMIDDLENAME",
                          dt_mom_dob_yr = "1998",
                          dt_mom_dob_mo = "01",
                          dt_mom_dob_da = "01",
                          tx_dad_last_name = "FAKELASTNAME",
                          tx_dad_first_name = "FAKEFIRSTNAME",
                          tx_dad_middle_name = "FAKEMIDDLENAME",
                          tx_dad_suffix_name = "JR",
                          dt_dad_dob_yr = "1998",
                          dt_dad_dob_mo = "01",
                          dt_dad_dob_da = "01",
                          dt_youngest_maternal_sibling_birth = "199801",
                          tx_last_2 = "AA",
                          tx_first_2 = "BB",
                          tx_middle_2 = "CC",
                          dt_cdob_yr = "1998",
                          dt_cdob_mo = "01",
                          dt_cdob_da = "01")

# 12/26/2021 File below didn't work, try just 2007
rodis_people_2007 <- rodis_people_glue %>% 
  filter((tx_record_relation == "famlink" |
            substr(id_conglomerate,1,4) == "2007") &
           !is.na(dt_birth_yr))
linkplus_input <- fake_record %>% 
  bind_rows(rodis_people_2007) %>% 
  mutate(block_sex_last2_first2 = paste0(tx_reported_sex_or_gender,tx_last_2,
                                         tx_first_2))
write_csv(linkplus_input, 
          "../data_working/rodis_people_2007.csv", na="", eol = "\r\n")


# 12/24/2021
# All Famlink and 2007-2008 birth, death, and CHARS.
# Try excluding records with missing DOB.
rodis_people_2007_2008 <- rodis_people_glue %>% 
  filter((tx_record_relation == "famlink" |
           substr(id_conglomerate,1,4) %in% c("2007","2008")) &
           !is.na(dt_birth_yr))
linkplus_input <- fake_record %>% 
  bind_rows(rodis_people_2007_2008) %>% 
  mutate(block_sex_last2_first2 = paste0(tx_reported_sex_or_gender,tx_last_2,
                                         tx_first_2))
write_csv(linkplus_input, 
          "../data_working/rodis_people_2007_2008.csv", na="", eol = "\r\n")


# All Famlink and 2006-2008 birth, death, and CHARS.
# Link Plus terminated while trying to match this file.
rodis_people_2006_2008 <- rodis_people_glue %>% 
  filter(tx_record_relation == "famlink" |
           substr(id_conglomerate,1,4) %in% c("2006","2007","2008"))
linkplus_input <- fake_record %>% 
  bind_rows(rodis_people_2006_2008) %>% 
  mutate(block_sex_last2_first2 = paste0(tx_reported_sex_or_gender,tx_last_2,
                                         tx_first_2))
write_csv(linkplus_input, 
          "../data_working/rodis_people_2006_2008.csv", na="", eol = "\r\n")

# 1999 death records and all Famlink records
death_1999_Famlink <- rodis_people_glue %>% 
  filter((tx_record_relation == "death" &
            substr(id_conglomerate,1,4) == "1999") |
           tx_record_relation == "famlink")

linkplus_input <- fake_record %>% 
  bind_rows(death_1999_Famlink) %>% 
  mutate(block_sex_last2_first2 = paste0(tx_reported_sex_or_gender,tx_last_2,
                                         tx_first_2))

write_csv(linkplus_input, 
          "../data_working/death_1999_Famlink.csv", na="", eol = "\r\n")

# 1999 birth records and all Famlink records
birth_1999_Famlink <- rodis_people_glue %>% 
  filter((tx_record_relation %in% c("child","mother","father") &
           substr(id_conglomerate,1,4) == "1999") |
             tx_record_relation == "famlink")

linkplus_input <- fake_record %>% 
  bind_rows(birth_1999_Famlink) %>% 
  mutate(block_sex_last2_first2 = paste0(tx_reported_sex_or_gender,tx_last_2,
                                         tx_first_2))

write_csv(linkplus_input, 
          "../data_working/birth_1999_Famlink.csv", na="", eol = "\r\n")

##########################################################################
# Don't run the rest of this
# 1999 CHARS data with name truncation and Famlink records
death_CHARS_1999 <- rodis_people_glue %>% 
  filter(tx_record_relation %in% c("patient","death") &
            substr(id_conglomerate,1,4) == "1999")

linkplus_input <- fake_record %>% 
  bind_rows(death_CHARS_1999) %>% 
  mutate(block_sex_last2_first2 = paste0(tx_reported_sex_or_gender,tx_last_2,
                                         tx_first_2))

write_csv(linkplus_input, 
          "../data_working/death_CHARS_1999.csv", na="", eol = "\r\n")

# 1999 CHARS data with name truncation and Famlink records
CHARS_1999_Famlink <- rodis_people_glue %>% 
  filter((tx_record_relation == "patient" &
           substr(id_conglomerate,1,4) == "1999") |
           tx_record_relation == "famlink")

linkplus_input <- fake_record %>% 
  bind_rows(CHARS_1999_Famlink) %>% 
  mutate(block_sex_last2_first2 = paste0(tx_reported_sex_or_gender,tx_last_2,
                                         tx_first_2))

write_csv(linkplus_input, 
          "../data_working/CHARS_1999_Famlink.csv", na="", eol = "\r\n")

# 1999 birth and CHARS data with name truncation
birth_CHARS_1999 <- rodis_people_glue %>% 
  filter(tx_record_relation %in% c("child","mother","patient") &
           substr(id_conglomerate,1,4) == "1999")

linkplus_input <- fake_record %>% 
  bind_rows(birth_CHARS_1999) %>% 
  mutate(block_sex_last2_first2 = paste0(tx_reported_sex_or_gender,tx_last_2,
                                         tx_first_2))

write_csv(linkplus_input, 
          "../data_working/birth_CHARS_1999_v4.csv", na="", eol = "\r\n")



######################################################################
# What follows is older code used in tests on just the birth data

load(paste0(getwd(),"/rodis_people_mother_daughter.Rdata"))
load(paste0(getwd(),"/rodis_people_father_son.Rdata"))

# Comma-separated file for Link Plus
# Need a fake record at the top of the file so Link Plus can guess the data types of
# columns that have a lot of missing data, like maiden name.
fake_record <- data.frame(id_conglomerate = "1998900001mother",
                              id_source_record = "1998900001",
                              tx_record_relation = "mother",
                           tx_last_name = "FAKELASTNAME",
                           tx_maiden_name = "FAKEMAIDENNAME",
                           tx_first_name = "FAKEFIRSTNAME",
                           tx_middle_name = "FAKEMIDDLENAME",
                           tx_suffix_name = "JR",
                           dt_birth = "19000101",
                           tx_reported_sex_or_gender = "U")

write_csv(fake_record %>% bind_rows(rodis_people_mother_daughter) %>% 
          filter(as.numeric(substr(id_conglomerate,1,4)) %in% 
                   c(1998,2002,2003,2004)), 
         "rodis_people_mother_daughter_2002_2004.csv", na="", eol = "\r\n")
write_csv(fake_record %>% bind_rows(rodis_people_mother_daughter) %>% 
            filter(as.numeric(substr(id_conglomerate,1,4)) %in% 
                     c(1998,2005,2006,2007)), 
          "rodis_people_mother_daughter_2005_2007.csv", na="", eol = "\r\n")
write_csv(fake_record %>% bind_rows(rodis_people_mother_daughter) %>% 
            filter(as.numeric(substr(id_conglomerate,1,4)) %in% 
                     c(1998,2008,2009,2010)), 
          "rodis_people_mother_daughter_2008_2010.csv", na="", eol = "\r\n")
write_csv(fake_record %>% bind_rows(rodis_people_mother_daughter) %>% 
            filter(as.numeric(substr(id_conglomerate,1,4)) %in% 
                     c(1998,2011,2012,2013,2014)), 
          "rodis_people_mother_daughter_2011_2014.csv", na="", eol = "\r\n")
write_csv(fake_record %>% bind_rows(rodis_people_mother_daughter) %>% 
            filter(as.numeric(substr(id_conglomerate,1,4)) %in% 
                     c(1998,2015,2016,2017)), 
          "rodis_people_mother_daughter_2015_2017.csv", na="", eol = "\r\n")

write_csv(fake_record %>% bind_rows(rodis_people_father_son) %>% 
            filter(as.numeric(substr(id_conglomerate,1,4)) %in% 
                     c(1998,1999,2000,2001)), 
          "rodis_people_father_son_1999_2001.csv", na="", eol = "\r\n")
write_csv(fake_record %>% bind_rows(rodis_people_father_son) %>% 
            filter(as.numeric(substr(id_conglomerate,1,4)) %in% 
                     c(1998,2002,2003,2004)), 
          "rodis_people_father_son_2002_2004.csv", na="", eol = "\r\n")
write_csv(fake_record %>% bind_rows(rodis_people_father_son) %>% 
            filter(as.numeric(substr(id_conglomerate,1,4)) %in% 
                     c(1998,2005,2006,2007)), 
          "rodis_people_father_son_2005_2007.csv", na="", eol = "\r\n")
write_csv(fake_record %>% bind_rows(rodis_people_father_son) %>% 
            filter(as.numeric(substr(id_conglomerate,1,4)) %in% 
                     c(1998,2008,2009,2010)), 
          "rodis_people_father_son_2008_2010.csv", na="", eol = "\r\n")
write_csv(fake_record %>% bind_rows(rodis_people_father_son) %>% 
            filter(as.numeric(substr(id_conglomerate,1,4)) %in% 
                     c(1998,2011,2012,2013,2014)), 
          "rodis_people_father_son_2011_2014.csv", na="", eol = "\r\n")
write_csv(fake_record %>% bind_rows(rodis_people_father_son) %>% 
            filter(as.numeric(substr(id_conglomerate,1,4)) %in% 
                     c(1998,2015,2016,2017)), 
          "rodis_people_father_son_2015_2017.csv", na="", eol = "\r\n")

# 7/24/2021 father/son files aren't running in LinkPlus. Try deleting records
# where father's first name is "NONENAMED".
father_son_1999_2001 <- read_csv("rodis_people_father_son_1999_2001.csv",
                                 col_types = "ccccccccc") %>% 
  filter(tx_first_name != "NONENAMED")
write_csv(father_son_1999_2001,
          "rodis_people_father_son_1999_2001_named.csv", 
          na="", eol = "\r\n")
