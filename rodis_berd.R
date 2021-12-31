######################################################################
#                                                                    #
# Project:  RODIS                                                    #
#                                                                    #
# File: rodis_berd.R                                                 #
#                                                                    #
# Purpose: Create variables in rodis.berd table to the extent        #
#          possible with the data requested for this round of RODIS  #
#                                                                    #
# Procedures:  #
#                                                                    #
# Programmers: Karen Segar                                           #
#                                                                    #
# Dates: 8/29/2021 created                                           #
#                                                                    #
######################################################################

library(tidyverse)
library(lubridate)
library(haven)
library(readxl)

read_berd_data <- function(pii_type) {
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
      dataset <-
        bind_rows(list(dataset, temp_data)) #for each iteration, bind the new data to the building dataset
    }
    return(dataset)
}

# Get code lists from data dictionary
tribe <- read_excel("documentation/422-161-BirthStatisticalDictionaryCrosswalks_2017.xlsx",
                    sheet = "Tribal Reservation Codes")
state <- read_excel("documentation/422-161-BirthStatisticalDictionaryCrosswalks_2017.xlsx",
                    sheet = "NCHS State Codes") %>% 
  filter(!str_detect(`NCHS STATE CODES`,"Present")) %>% 
  separate(`NCHS STATE CODES`,into = c("nchs_state","state"),sep = " - ") %>% 
  mutate(state_upper = toupper(state))

birth_raw <- read_berd_data("birth") %>% 
    mutate(year = as.numeric(str_extract(source_data,"(?<=bir)[[:digit:]]{4}")))
save(birth_raw,file="birth_raw.Rdata")
load("birth_raw.Rdata")

# From kotel.sas:
# prevs - number of prenatal care visits
#       premo=  'month prenatal care visits began'
# sx=   'sx of infant'
# gest=  'gestational age in weeks'
# bgrams= 'birth weight in grams'
# uexpvis= 'unadjusted expected prenatal care visits'
# expvis=  'expected prenatal care visits'
# evratio= 'expected visit ratio (observed/expected)'
# evindex= 'expected visit index (received pnc service)'
# moindex4='month prenatal care initiation index'
# indexsum='two factor summary index'
# gestimp= 'gestational age imputation marker'
# nopnc=   'no prenatal care received'

#         howlgm = Mother_Months_at_Residence, --- variable accidentally not requested

birth_berd_2017 <- birth_raw %>% 
  filter(as.numeric(str_extract(source_data,"(?<=bir)[[:digit:]]{4}")) > 2016) %>% 
  rename(`Tribe Literal` = Mother_Res_Tribal_Reservation,
         state_upper = Mother_Birthplace_State) %>% 
  mutate(`Tribe Literal` = recode(`Tribe Literal`,
                "COLVILLE RESERVATION" = "COLVILLE CONFEDERATED TRIBES",
                "CONFEDERATED TRIBES-COLVILLE" = "COLVILLE CONFEDERATED TRIBES",
                "CONFEDERATED TRIBES-COLVILLE RESERVATION" = "COLVILLE CONFEDERATED TRIBES",
                "CONFEDERATED TRIBES COLVILLE" = "COLVILLE CONFEDERATED TRIBES",
                "TRIBES COLVILLE" = "COLVILLE CONFEDERATED TRIBES",
                "CHEHALIS CONFEDERATED" = "CHEHALIS",
                "LOWER ELWHA" = "LOWER ELWHA KLALLAM",
                "LUMMI NATION" = "LUMMI",
                "SAUK-SUIATTLE" = "SAUK SUIATTLE",
                "SPOKANE TRIBE" = "SPOKANE"),
         state_upper = recode(state_upper,
                              "NORTHERN MARIANA ISLANDS" = "MARIANAS")) %>% 
  left_join(tribe, by = "Tribe Literal") %>%
  left_join(state, by = "state_upper") %>% 
  rename(trib_res = `Tribal Reservation Code`,
         Mother_Birthplace_State = state_upper,
         mombirst = nchs_state) %>% 
  mutate(year = as.numeric(str_extract(source_data,"(?<=bir)[[:digit:]]{4}")),
         birthyr = as.numeric(Date_of_Birth_Year),
         birthmo = as.numeric(Date_of_Birth_Month),
         birthdy = as.numeric(Date_of_Birth_Day),
         trib_res = ifelse(`Tribe Literal` %in% c("UNK","UNKNOWN"),
                           999,
                           trib_res),
         momle8ed = Mother_Educ_8th_Grade_or_Less,
         dadle8ed = Father_Educ_8th_Grade_or_Less,
         otherout = Other_Preg_Outcomes,
         birplace = Facility_Type,
         birfacil = as.numeric(Facility),
         bcountry = ifelse(is.na(as.numeric(Birthplace_Country_WA_Code)),
                           0, as.numeric(Birthplace_Country_WA_Code)),
         bres = as.numeric(Birthplace_County_City_WA_Code),
         diedyr = year(Date_of_Death),
         diedmo = month(Date_of_Death),
         dieddy = day(Date_of_Death),
         partype = Parent_Type,
         dadbirst = as.numeric(Father_Birthplace_State_NCHS_Cd),
         dcountry = as.numeric(Father_Birthplace_Cntry_WA_Code),
         dadbiryr = Father_Date_of_Birth_Year,
         dadbirmo = Father_Date_of_Birth_Month,
         dadbirdy = Father_Date_of_Birth_Day,
         mombirst = as.numeric(mombirst),
         mcountry = as.numeric(Mother_Birthplace_Cntry_WA_Code),
         mombiryr = Mother_Date_of_Birth_Year,
         mombirmo = Mother_Date_of_Birth_Month,
         mombirdy = Mother_Date_of_Birth_Day,
         momres = as.numeric(Mother_Residence_City_WA_Code),
         howlgy = Mother_Years_at_Residence,
         chisp = as.numeric(Child_Calculated_Ethnicity),
         dadhisp = Father_Hispanic,
         momhisp = Mother_Hispanic,
         dadedu = case_when(Father_Education == 1 & Father_Educ_8th_Grade_or_Less < 9 ~ Father_Educ_8th_Grade_or_Less,
                            Father_Education == 1 & Father_Educ_8th_Grade_or_Less == 9 ~ 99,
                            Father_Education == 2 ~ 10,
                            Father_Education == 3 ~ 12,
                            Father_Education == 4 ~ 13,
                            Father_Education == 5 ~ 14,
                            Father_Education == 6 ~ 16,
                            Father_Education == 7 ~ 18,
                            Father_Education == 8 ~ 20,
                            Father_Education == 9 ~ 99),
         momedu = case_when(Mother_Education == 1 & Mother_Educ_8th_Grade_or_Less < 9 ~ Mother_Educ_8th_Grade_or_Less,
                            Mother_Education == 1 & Mother_Educ_8th_Grade_or_Less == 9 ~ 99,
                            Mother_Education == 2 ~ 10,
                            Mother_Education == 3 ~ 12,
                            Mother_Education == 4 ~ 13,
                            Mother_Education == 5 ~ 14,
                            Mother_Education == 6 ~ 16,
                            Mother_Education == 7 ~ 18,
                            Mother_Education == 8 ~ 20,
                            Mother_Education == 9 ~ 99),
         maristat = case_when(Mother_Marital_Status %in% c("1","2","3") ~ 1,
                              Mother_Marital_Status %in% c("4","5","6") ~ 2,
                              Mother_Marital_Status %in% c("8","U") ~ 9),
         lbnl = Prior_Live_Births_Living,
         lbnd = Prior_Live_Births_Deceased,
         lstlivyr = Last_Live_Birth_Year,
         lstlivmo = Last_Live_Birth_Month,
         lstfetyr = Other_Preg_Outcomes_Year,
         lstfetmo = Other_Preg_Outcomes_Month,
         delivpay = recode(Source_of_Payment, `4` = 6, `6` = 4),
         gestest = Gestation_Estimate,
         prenatvs = Number_Prenatal_Visits,
         numatbir = Plurality,
         order = Birth_Order,
         birgrams = Birth_Weight_Grams,
         pripreg = ifelse(Prior_Live_Births_Living < 99 & Prior_Live_Births_Deceased < 99 &
                            Other_Preg_Outcomes < 99,
                          Prior_Live_Births_Living + Prior_Live_Births_Deceased + Other_Preg_Outcomes,
                          NA),
         momrace = recode(Mother_Race_Calculation,
                          "1" = 1,
                          "2" = 2,
                          "3" = 3,
                          "4" = 4,
                          "5" = 5,
                          "7" = 6,
                          "A" = 7,
                          "B" = 8,
                          "C" = 9,
                          "6" = 10,
                          "D" = 11,
                          "E" = 12,
                          "F" = 13,
                          "G" = 14,
                          "H" = 15,
                          "8" = 98,
                          "9" = 99),
         dadrace = recode(Father_Race_Calculation,
                          "1" = 1,
                          "2" = 2,
                          "3" = 3,
                          "4" = 4,
                          "5" = 5,
                          "7" = 6,
                          "A" = 7,
                          "B" = 8,
                          "C" = 9,
                          "6" = 10,
                          "D" = 11,
                          "E" = 12,
                          "F" = 13,
                          "G" = 14,
                          "H" = 15,
                          "8" = 98,
                          "9" = 99),
         kidrace = recode(Child_Calculated_Race,
                          "1" = 1,
                          "2" = 2,
                          "3" = 3,
                          "4" = 4,
                          "5" = 5,
                          "7" = 6,
                          "A" = 7,
                          "B" = 8,
                          "C" = 9,
                          "6" = 10,
                          "D" = 11,
                          "E" = 12,
                          "F" = 13,
                          "G" = 14,
                          "H" = 15,
                          "8" = 98,
                          "9" = 99),
         mcnty = as.numeric(Mother_Residence_County_WA_Code),
         sex = recode(Sex, 
                      "M" = 1,
                      "F" = 2),
         momage = Mother_Calculated_Age,
         dadage = Father_Calculated_Age,
         momedu03 = Mother_Education,
         dadedu03 = Father_Education,
         wic = recode(WIC,
                      "Y" = 1,
                      "N" = 0,
                      "U" = 9),
         dadocc = as.numeric(Father_Occupation_Milham),
         momocc = as.numeric(Mother_Occupation_Milham),
         prenatmo = Month_Prenatal_Care_Began,
         gestlen = Date_of_Birth - Date_Last_Menses,
         ddays = as.numeric(Date_of_Death - Date_of_Birth),
         PARITY = ifelse(Prior_Live_Births_Living < 99 & Prior_Live_Births_Deceased < 99,
                         Prior_Live_Births_Living + Prior_Live_Births_Deceased,
                         NA),
         moindex4 = ifelse(Number_Prenatal_Visits >= 0 & Number_Prenatal_Visits <= 90,
                           case_when(Month_Prenatal_Care_Began >= 1 & Month_Prenatal_Care_Began <=2 ~ 4,
                                     Month_Prenatal_Care_Began >= 3 & Month_Prenatal_Care_Began <=4 ~ 3,
                                     Month_Prenatal_Care_Began >= 5 & Month_Prenatal_Care_Began <=6 ~ 2,
                                     (Month_Prenatal_Care_Began >= 7 & Month_Prenatal_Care_Began <=9) |
                                       Month_Prenatal_Care_Began == 0 |
                                       (Number_Prenatal_Visits == 0 & is.na(Month_Prenatal_Care_Began)) ~ 1),
                           0),
         gestlen = ifelse(Calculated_Gestation < 99,
                          Calculated_Gestation,
                          NA),
         gestest = ifelse(Gestation_Estimate < 99,
                          Gestation_Estimate,
                          NA),
         gest1 = ifelse(round(gestlen/7,1) >= 18 & round(gestlen/7,1) <= 50,
                       round(gestlen/7,1),
                       ifelse(gestest >= 18 & gestest <= 50,
                              gestest,
                              NA)),
         premo = ifelse(!is.na(gest1) & ifelse(Month_Prenatal_Care_Began <= 9,
                                              Month_Prenatal_Care_Began,
                                              NA) <= gest1/4,
                        ifelse(Month_Prenatal_Care_Began <= 9,
                        Month_Prenatal_Care_Began,
                        NA),
                        NA),
         prevs = ifelse(Number_Prenatal_Visits < 90,
                        Number_Prenatal_Visits,
                        NA),
         premo = ifelse((prevs == 0 & premo >= 1) | (premo == 0 & prevs >= 1),
                        NA,
                        premo),
         prevs = ifelse((prevs == 0 & premo >= 1) | (premo == 0 & prevs >= 1),
                        NA,
                        prevs),
         bgrams = ifelse(birgrams >= 400 & birgrams <= 6000,
                         birgrams,
                         NA),
         gest1 = ifelse(is.na(gest1),
                        case_when(sex == 1 & 530 <= bgrams & bgrams <= 608 ~ 22,
                                  sex == 1 & 609 <= bgrams & bgrams <= 698 ~ 23,
                                  sex == 1 & 699 <= bgrams & bgrams <= 799 ~ 24,
                                  sex == 1 & 800 <= bgrams & bgrams <= 912 ~ 25,
                                  sex == 1 & 913 <= bgrams & bgrams <= 1040 ~ 26,
                                  sex == 1 & 1041 <= bgrams & bgrams <= 1183 ~ 27,
                                  sex == 1 & 1184 <= bgrams & bgrams <= 1342 ~ 28,
                                  sex == 1 & 1343 <= bgrams & bgrams <= 1536 ~ 29,
                                  sex == 1 & 1537 <= bgrams & bgrams <= 1751 ~ 30,
                                  sex == 1 & 1752 <= bgrams & bgrams <= 1978 ~ 31,
                                  sex == 1 & 1979 <= bgrams & bgrams <= 2219 ~ 32,
                                  sex == 1 & 2220 <= bgrams & bgrams <= 2458 ~ 33,
                                  sex == 1 & 2459 <= bgrams & bgrams <= 2693 ~ 34,
                                  sex == 1 & 2694 <= bgrams & bgrams <= 2909 ~ 35,
                                  sex == 1 & 2910 <= bgrams & bgrams <= 3111 ~ 36,
                                  sex == 1 & 3112 <= bgrams & bgrams <= 3291 ~ 37,
                                  sex == 1 & 3292 <= bgrams & bgrams <= 3433 ~ 38,
                                  sex == 1 & 3434 <= bgrams & bgrams <= 3533 ~ 39,
                                  sex == 1 & 3534 <= bgrams & bgrams <= 6000 ~ 40,
                                  sex == 2 & 496 <= bgrams & bgrams <= 568 ~ 22,
                                  sex == 2 & 569 <= bgrams & bgrams <= 650 ~ 23,
                                  sex == 2 & 651 <= bgrams & bgrams <= 744 ~ 24,
                                  sex == 2 & 745 <= bgrams & bgrams <= 849 ~ 25,
                                  sex == 2 & 850 <= bgrams & bgrams <= 968 ~ 26,
                                  sex == 2 & 969 <= bgrams & bgrams <= 1101 ~ 27,
                                  sex == 2 & 1102 <= bgrams & bgrams <= 1251 ~ 28,
                                  sex == 2 & 1252 <= bgrams & bgrams <= 1429 ~ 29,
                                  sex == 2 & 1430 <= bgrams & bgrams <= 1636 ~ 30,
                                  sex == 2 & 1637 <= bgrams & bgrams <= 1860 ~ 31,
                                  sex == 2 & 1861 <= bgrams & bgrams <= 2089 ~ 32,
                                  sex == 2 & 2090 <= bgrams & bgrams <= 2328 ~ 33,
                                  sex == 2 & 2329 <= bgrams & bgrams <= 2561 ~ 34,
                                  sex == 2 & 2562 <= bgrams & bgrams <= 2787 ~ 35,
                                  sex == 2 & 2788 <= bgrams & bgrams <= 2991 ~ 36,
                                  sex == 2 & 2992 <= bgrams & bgrams <= 3160 ~ 37,
                                  sex == 2 & 3161 <= bgrams & bgrams <= 3293 ~ 38,
                                  sex == 2 & 3294 <= bgrams & bgrams <= 3388 ~ 39,
                                  sex == 2 & 3389 <= bgrams & bgrams <= 6000 ~ 40),
                        gest1),
         uexpvis = case_when(gest1 >= 35 ~ gest1 - 26,
                             gest1 == 34 ~ 9,
                             gest1 >= 32 ~ 8,
                             gest1 >= 30 ~ 7,
                             gest1 >= 26 ~ 6,
                             gest1 >= 22 ~ 5,
                             gest1 >= 18 ~ 4,
                             gest1 >= 14 ~ 3,
                             gest1 >= 10 ~ 2,
                             gest1 >= 6 ~ 1,
                             gest1 >= 0 ~ 0),
         expvis = case_when(is.na(premo) | premo == 0 ~ uexpvis,
                            premo == 10 ~ uexpvis - 17,
                            premo == 9 ~ uexpvis - 13,
                            premo == 8 ~ uexpvis - 9,
                            premo == 7 ~ uexpvis - 7,
                            premo == 6 ~ uexpvis - 6,
                            premo == 5 ~ uexpvis - 5,
                            premo == 4 ~ uexpvis - 3,
                            premo == 3 ~ uexpvis - 2,
                            premo == 2 ~ uexpvis - 1,
                            premo == 1 ~ uexpvis),
         expvis = ifelse(expvis <= 0,
                         1,
                         expvis),
         evratio = (prevs/expvis) * 100,
         evindex = case_when(is.na(evratio) | is.na(premo) ~ 0,
                             evratio > 109.99 ~ 4,
                             evratio > 79.99 ~ 3,
                             evratio > 49.99 ~ 2,
                             evratio <= 49.99 ~ 1),
         indexsum = ifelse(evindex == 0 | moindex4 == 0,
                  NA,
                  ifelse(evindex == 1 | (moindex4 <= 2 & moindex4 >= 1),
                         1,
                         ifelse(evindex == 3 & moindex4 <= 4 & moindex4 >= 3,
                                3,
                                ifelse(evindex == 4 & moindex4 <= 4 & moindex4 >= 3,
                                       4,
                                       NA)))),
         gest2 = ifelse(Gestation_Estimate >= 10 & Gestation_Estimate <= 60,
                       Gestation_Estimate,
                       ifelse(Calculated_Gestation < 99,
                              round(Calculated_Gestation/7,1),
                              NA)),
         sga = case_when(birgrams >= 326 & birgrams < 736 & 
                           gest2 >= 22 & gest2 < 23 & sex == 1 ~ 2,
                         birgrams < 326 & 
                           gest2 >= 22 & gest2 < 23 & sex == 1 ~ 1,
                         birgrams >= 736 & 
                           gest2 >= 22 & gest2 < 23 & sex == 1 ~ 3,
                         birgrams >= 376 & birgrams < 852 & 
                           gest2 >= 23 & gest2 < 24 & sex == 1 ~ 2,
                         birgrams < 376 &  
                           gest2 >= 23 & gest2 < 24 & sex == 1 ~ 1,
                         birgrams >= 852 &  
                           gest2 >= 23 & gest2 < 24 & sex == 1 ~ 3,
                         birgrams >= 433 & birgrams < 982 & 
                           gest2 >= 24 & gest2 < 25 & sex == 1 ~ 2,
                         birgrams < 433 &  
                           gest2 >= 24 & gest2 < 25 & sex == 1 ~ 1,
                         birgrams >= 982 &  
                           gest2 >= 24 & gest2 < 25 & sex == 1 ~ 3,
                         birgrams >= 499 & birgrams < 1127 & 
                           gest2 >= 25 & gest2 < 26 & sex == 1 ~ 2,
                         birgrams < 499 &  
                           gest2 >= 25 & gest2 < 26 & sex == 1 ~ 1,
                         birgrams >= 1127 &  
                           gest2 >= 25 & gest2 < 26 & sex == 1 ~ 3,
                         birgrams >= 574 & birgrams < 1288 & 
                           gest2 >= 26 & gest2 < 27 & sex == 1 ~ 2,
                         birgrams < 574 &  
                           gest2 >= 26 & gest2 < 27 & sex == 1 ~ 1,
                         birgrams >= 1288 &  
                           gest2 >= 26 & gest2 < 27 & sex == 1 ~ 3,
                         birgrams >= 662 & birgrams < 1466 & 
                           gest2 >= 27 & gest2 < 28 & sex == 1 ~ 2,
                         birgrams < 662 &  
                           gest2 >= 27 & gest2 < 28 & sex == 1 ~ 1,
                         birgrams >= 1466 &  
                           gest2 >= 27 & gest2 < 28 & sex == 1 ~ 3,
                         birgrams >= 762 & birgrams < 1661 & 
                           gest2 >= 28 & gest2 < 29 & sex == 1 ~ 2,
                         birgrams < 762 &  
                           gest2 >= 28 & gest2 < 29 & sex == 1 ~ 1,
                         birgrams >= 1661 &  
                           gest2 >= 28 & gest2 < 29 & sex == 1 ~ 3,
                         birgrams >= 878 & birgrams < 1873 & 
                           gest2 >= 29 & gest2 < 30 & sex == 1 ~ 2,
                         birgrams < 878 &  
                           gest2 >= 29 & gest2 < 30 & sex == 1 ~ 1,
                         birgrams >= 1873 &  
                           gest2 >= 29 & gest2 < 30 & sex == 1 ~ 3,
                         birgrams >= 1007 & birgrams < 2159 & 
                           gest2 >= 30 & gest2 < 31 & sex == 1 ~ 2,
                         birgrams < 1007 &  
                           gest2 >= 30 & gest2 < 31 & sex == 1 ~ 1,
                         birgrams >= 2159 &  
                           gest2 >= 30 & gest2 < 31 & sex == 1 ~ 3,
                         birgrams >= 1159 & birgrams < 2439 & 
                           gest2 >= 31 & gest2 < 32 & sex == 1 ~ 2,
                         birgrams < 1159 &  
                           gest2 >= 31 & gest2 < 32 & sex == 1 ~ 1,
                         birgrams >= 2439 &  
                           gest2 >= 31 & gest2 < 32 & sex == 1 ~ 3,
                         birgrams >= 1348 & birgrams < 2727 & 
                           gest2 >= 32 & gest2 < 33 & sex == 1 ~ 2,
                         birgrams < 1348 &  
                           gest2 >= 32 & gest2 < 33 & sex == 1 ~ 1,
                         birgrams >= 2727 &  
                           gest2 >= 32 & gest2 < 33 & sex == 1 ~ 3,
                         birgrams >= 1561 & birgrams < 2972 & 
                           gest2 >= 33 & gest2 < 34 & sex == 1 ~ 2,
                         birgrams < 1561 &  
                           gest2 >= 33 & gest2 < 34 & sex == 1 ~ 1,
                         birgrams >= 2972 &  
                           gest2 >= 33 & gest2 < 34 & sex == 1 ~ 3,
                         birgrams >= 1787 & birgrams < 3205 & 
                           gest2 >= 34 & gest2 < 35 & sex == 1 ~ 2,
                         birgrams < 1787 &  
                           gest2 >= 34 & gest2 < 35 & sex == 1 ~ 1,
                         birgrams >= 3205 &  
                           gest2 >= 34 & gest2 < 35 & sex == 1 ~ 3,
                         birgrams >= 2030 & birgrams < 3415 & 
                           gest2 >= 35 & gest2 < 36 & sex == 1 ~ 2,
                         birgrams < 2030 &  
                           gest2 >= 35 & gest2 < 36 & sex == 1 ~ 1,
                         birgrams >= 3415 &  
                           gest2 >= 35 & gest2 < 36 & sex == 1 ~ 3,
                         birgrams >= 2278 & birgrams < 3591 & 
                           gest2 >= 36 & gest2 < 37 & sex == 1 ~ 2,
                         birgrams < 2278 &  
                           gest2 >= 36 & gest2 < 37 & sex == 1 ~ 1,
                         birgrams >= 3591 &  
                           gest2 >= 36 & gest2 < 37 & sex == 1 ~ 3,
                         birgrams >= 2499 & birgrams < 3765 & 
                           gest2 >= 37 & gest2 < 38 & sex == 1 ~ 2,
                         birgrams < 2499 &  
                           gest2 >= 37 & gest2 < 38 & sex == 1 ~ 1,
                         birgrams >= 3765 &  
                           gest2 >= 37 & gest2 < 38 & sex == 1 ~ 3,
                         birgrams >= 2696 & birgrams < 3931 & 
                           gest2 >= 38 & gest2 < 39 & sex == 1 ~ 2,
                         birgrams < 2696 &  
                           gest2 >= 38 & gest2 < 39 & sex == 1 ~ 1,
                         birgrams >= 3931 &  
                           gest2 >= 38 & gest2 < 39 & sex == 1 ~ 3,
                         birgrams >= 2849 & birgrams < 4064 & 
                           gest2 >= 39 & gest2 < 40 & sex == 1 ~ 2,
                         birgrams < 2849 &  
                           gest2 >= 39 & gest2 < 40 & sex == 1 ~ 1,
                         birgrams >= 4064 &  
                           gest2 >= 39 & gest2 < 40 & sex == 1 ~ 3,
                         birgrams >= 2944 & birgrams < 4154 & 
                           gest2 >= 40 & gest2 < 41 & sex == 1 ~ 2,
                         birgrams < 2944 &  
                           gest2 >= 40 & gest2 < 41 & sex == 1 ~ 1,
                         birgrams >= 4154 &  
                           gest2 >= 40 & gest2 < 41 & sex == 1 ~ 3,
                         birgrams >= 3018 & birgrams < 4214 & 
                           gest2 >= 41 & gest2 < 42 & sex == 1 ~ 2,
                         birgrams < 3018 &  
                           gest2 >= 41 & gest2 < 42 & sex == 1 ~ 1,
                         birgrams >= 4214 &  
                           gest2 >= 41 & gest2 < 42 & sex == 1 ~ 3,
                         birgrams >= 3086 & birgrams < 4330 & 
                           gest2 >= 42 & gest2 < 50 & sex == 1 ~ 2,
                         birgrams < 3086 &  
                           gest2 >= 42 & gest2 < 50 & sex == 1 ~ 1,
                         birgrams >= 4330 &  
                           gest2 >= 42 & gest2 < 50 & sex == 1 ~ 3,
                         birgrams >= 314 & birgrams < 755 & 
                           gest2 >= 22 & gest2 < 23 & sex == 2 ~ 2,
                         birgrams < 314 & 
                           gest2 >= 22 & gest2 < 23 & sex == 2 ~ 1,
                         birgrams >= 755 & 
                           gest2 >= 22 & gest2 < 23 & sex == 2 ~ 3,
                         birgrams >= 354 & birgrams < 869 & 
                           gest2 >= 23 & gest2 < 24 & sex == 2 ~ 2,
                         birgrams < 354 &  
                           gest2 >= 23 & gest2 < 24 & sex == 2 ~ 1,
                         birgrams >= 869 &  
                           gest2 >= 23 & gest2 < 24 & sex == 2 ~ 3,
                         birgrams >= 400 & birgrams < 996 & 
                           gest2 >= 24 & gest2 < 25 & sex == 2 ~ 2,
                         birgrams < 400 &  
                           gest2 >= 24 & gest2 < 25 & sex == 2 ~ 1,
                         birgrams >= 996 &  
                           gest2 >= 24 & gest2 < 25 & sex == 2 ~ 3,
                         birgrams >= 454 & birgrams < 1136 & 
                           gest2 >= 25 & gest2 < 26 & sex == 2 ~ 2,
                         birgrams < 454 &  
                           gest2 >= 25 & gest2 < 26 & sex == 2 ~ 1,
                         birgrams >= 1136 &  
                           gest2 >= 25 & gest2 < 26 & sex == 2 ~ 3,
                         birgrams >= 518 & birgrams < 1290 & 
                           gest2 >= 26 & gest2 < 27 & sex == 2 ~ 2,
                         birgrams < 518 &  
                           gest2 >= 26 & gest2 < 27 & sex == 2 ~ 1,
                         birgrams >= 1290 &  
                           gest2 >= 26 & gest2 < 27 & sex == 2 ~ 3,
                         birgrams >= 591 & birgrams < 1460 & 
                           gest2 >= 27 & gest2 < 28 & sex == 2 ~ 2,
                         birgrams < 591 &  
                           gest2 >= 27 & gest2 < 28 & sex == 2 ~ 1,
                         birgrams >= 1460 &  
                           gest2 >= 27 & gest2 < 28 & sex == 2 ~ 3,
                         birgrams >= 678 & birgrams < 1645 & 
                           gest2 >= 28 & gest2 < 29 & sex == 2 ~ 2,
                         birgrams < 678 &  
                           gest2 >= 28 & gest2 < 29 & sex == 2 ~ 1,
                         birgrams >= 1645 &  
                           gest2 >= 28 & gest2 < 29 & sex == 2 ~ 3,
                         birgrams >= 780 & birgrams < 1845 & 
                           gest2 >= 29 & gest2 < 30 & sex == 2 ~ 2,
                         birgrams < 780 &  
                           gest2 >= 29 & gest2 < 30 & sex == 2 ~ 1,
                         birgrams >= 1845 &  
                           gest2 >= 29 & gest2 < 30 & sex == 2 ~ 3,
                         birgrams >= 902 & birgrams < 2113 & 
                           gest2 >= 30 & gest2 < 31 & sex == 2 ~ 2,
                         birgrams < 902 &  
                           gest2 >= 30 & gest2 < 31 & sex == 2 ~ 1,
                         birgrams >= 2113 &  
                           gest2 >= 30 & gest2 < 31 & sex == 2 ~ 3,
                         birgrams >= 1041 & birgrams < 2364 & 
                           gest2 >= 31 & gest2 < 32 & sex == 2 ~ 2,
                         birgrams < 1041 &  
                           gest2 >= 31 & gest2 < 32 & sex == 2 ~ 1,
                         birgrams >= 2364 &  
                           gest2 >= 31 & gest2 < 32 & sex == 2 ~ 3,
                         birgrams >= 1219 & birgrams < 2619 & 
                           gest2 >= 32 & gest2 < 33 & sex == 2 ~ 2,
                         birgrams < 1219 &  
                           gest2 >= 32 & gest2 < 33 & sex == 2 ~ 1,
                         birgrams >= 2619 &  
                           gest2 >= 32 & gest2 < 33 & sex == 2 ~ 3,
                         birgrams >= 1436 & birgrams < 2847 & 
                           gest2 >= 33 & gest2 < 34 & sex == 2 ~ 2,
                         birgrams < 1436 &  
                           gest2 >= 33 & gest2 < 34 & sex == 2 ~ 1,
                         birgrams >= 2847 &  
                           gest2 >= 33 & gest2 < 34 & sex == 2 ~ 3,
                         birgrams >= 1668 & birgrams < 3058 & 
                           gest2 >= 34 & gest2 < 35 & sex == 2 ~ 2,
                         birgrams < 1668 &  
                           gest2 >= 34 & gest2 < 35 & sex == 2 ~ 1,
                         birgrams >= 3058 &  
                           gest2 >= 34 & gest2 < 35 & sex == 2 ~ 3,
                         birgrams >= 1918 & birgrams < 3250 & 
                           gest2 >= 35 & gest2 < 36 & sex == 2 ~ 2,
                         birgrams < 1918 &  
                           gest2 >= 35 & gest2 < 36 & sex == 2 ~ 1,
                         birgrams >= 3250 &  
                           gest2 >= 35 & gest2 < 36 & sex == 2 ~ 3,
                         birgrams >= 2169 & birgrams < 3450 & 
                           gest2 >= 36 & gest2 < 37 & sex == 2 ~ 2,
                         birgrams < 2169 &  
                           gest2 >= 36 & gest2 < 37 & sex == 2 ~ 1,
                         birgrams >= 3450 &  
                           gest2 >= 36 & gest2 < 37 & sex == 2 ~ 3,
                         birgrams >= 2410 & birgrams < 3646 & 
                           gest2 >= 37 & gest2 < 38 & sex == 2 ~ 2,
                         birgrams < 2410 &  
                           gest2 >= 37 & gest2 < 38 & sex == 2 ~ 1,
                         birgrams >= 3646 &  
                           gest2 >= 37 & gest2 < 38 & sex == 2 ~ 3,
                         birgrams >= 2587 & birgrams < 3802 & 
                           gest2 >= 38 & gest2 < 39 & sex == 2 ~ 2,
                         birgrams < 2587 &  
                           gest2 >= 38 & gest2 < 39 & sex == 2 ~ 1,
                         birgrams >= 3802 &  
                           gest2 >= 38 & gest2 < 39 & sex == 2 ~ 3,
                         birgrams >= 2730 & birgrams < 3923 & 
                           gest2 >= 39 & gest2 < 40 & sex == 2 ~ 2,
                         birgrams < 2730 &  
                           gest2 >= 39 & gest2 < 40 & sex == 2 ~ 1,
                         birgrams >= 3923 &  
                           gest2 >= 39 & gest2 < 40 & sex == 2 ~ 3,
                         birgrams >= 2817 & birgrams < 4005 & 
                           gest2 >= 40 & gest2 < 41 & sex == 2 ~ 2,
                         birgrams < 2817 &  
                           gest2 >= 40 & gest2 < 41 & sex == 2 ~ 1,
                         birgrams >= 4005 &  
                           gest2 >= 40 & gest2 < 41 & sex == 2 ~ 3,
                         birgrams >= 2873 & birgrams < 4040 & 
                           gest2 >= 41 & gest2 < 42 & sex == 2 ~ 2,
                         birgrams < 2873 &  
                           gest2 >= 41 & gest2 < 42 & sex == 2 ~ 1,
                         birgrams >= 4040 &  
                           gest2 >= 41 & gest2 < 42 & sex == 2 ~ 3,
                         birgrams >= 2936 & birgrams < 4136 & 
                           gest2 >= 42 & gest2 < 50 & sex == 2 ~ 2,
                         birgrams < 2936 &  
                           gest2 >= 42 & gest2 < 50 & sex == 2 ~ 1,
                         birgrams >= 4136 &  
                           gest2 >= 42 & gest2 < 50 & sex == 2 ~ 3),
                 gestlen = Date_of_Birth - Date_Last_Menses
         )

#Process birth files with the Bedrock naming schema
birth_berd_bedrock <- birth_raw %>% 
           filter(as.numeric(str_extract(source_data,"(?<=bir)[[:digit:]]{4}")) <= 2016) %>% 
           mutate(year = as.numeric(str_extract(source_data,"(?<=bir)[[:digit:]]{4}")),
                  birthyr = as.numeric(substr(PERSON_DOB,1,4)),
                  birthmo = as.numeric(substr(PERSON_DOB,5,6)),
                  birthdy = as.numeric(substr(PERSON_DOB,7,8)),
                  trib_res = ifelse(year >= 2003,
                                    as.numeric(MO_RES_TRIBAL_RESV_CODE),
                                    NA),
                  momle8ed = as.numeric(MO_EDUC_LE_8TH),
                  dadle8ed = as.numeric(FA_EDUC_LE_8TH),
                  otherout = ifelse(year >= 2003,
                                    as.numeric(OTHER_PREG_OUTCOMES),
                                    NA),
                  birplace = as.numeric(EVENT_PLACE_TYPE),
                  birfacil = as.numeric(EVENT_FACILITY_CODE),
                  bcountry = 0,
                  bres = as.numeric(BIRTH_WA_CNTY_CITY_CODE),
                  diedyr = as.numeric(substr(DATE_OF_DEATH,1,4)),
                  diedmo = as.numeric(substr(DATE_OF_DEATH,5,6)),
                  dieddy = as.numeric(substr(DATE_OF_DEATH,7,8)),
                  partype = PARENT_TYPE,
                  dadbirst = as.numeric(FA_BIRTH_FED_STATE),
                  dcountry = as.numeric(FA_BIRTH_COUNTRY),
                  dadbiryr = as.numeric(substr(FA_BIOLOGICAL_DOB,1,4)),
                  dadbirmo = as.numeric(substr(FA_BIOLOGICAL_DOB,5,6)),
                  dadbirdy = as.numeric(substr(FA_BIOLOGICAL_DOB,7,8)),
                  mombirst = as.numeric(MO_BIRTH_FED_STATE),
                  mcountry = as.numeric(MO_BIRTH_COUNTRY),
                  mombiryr = as.numeric(substr(MO_BIOLOGICAL_DOB,1,4)),
                  mombirmo = as.numeric(substr(MO_BIOLOGICAL_DOB,5,6)),
                  mombirdy = as.numeric(substr(MO_BIOLOGICAL_DOB,7,8)),
                  momres = ifelse(RES_WA_CNTY_CITY_CODE != "0000",
                                  as.numeric(RES_WA_CNTY_CITY_CODE),
                                  NA),
                  howlgy = as.numeric(substr(RES_LENGTH_OF,1,2)),
                  howlgm = as.numeric(substr(RES_LENGTH_OF,3,4)),
                  chisp = as.numeric(PERSON_HISP_CALC),
                  dadhisp = as.numeric(FA_HISPANIC),
                  momhisp = as.numeric(MO_HISPANIC),
                  dadedu = ifelse(year<=2002,
                                  case_when(as.numeric(FA_EDUC_YRS) == 0 ~ 0,
                                            as.numeric(FA_EDUC_YRS) == 1 ~ 1,
                                            as.numeric(FA_EDUC_YRS) == 2 ~ 2,
                                            as.numeric(FA_EDUC_YRS) == 3 ~ 3,
                                            as.numeric(FA_EDUC_YRS) == 4 ~ 4,
                                            as.numeric(FA_EDUC_YRS) == 5 ~ 5,
                                            as.numeric(FA_EDUC_YRS) == 6 ~ 6,
                                            as.numeric(FA_EDUC_YRS) == 7 ~ 7,
                                            as.numeric(FA_EDUC_YRS) == 8 ~ 8,
                                            as.numeric(FA_EDUC_YRS) == 9 ~ 9,
                                            as.numeric(FA_EDUC_YRS) == 10 ~ 10,
                                            as.numeric(FA_EDUC_YRS) == 11 ~ 11,
                                            as.numeric(FA_EDUC_YRS) == 12 ~ 12,
                                            as.numeric(FA_EDUC_YRS) == 13 ~ 13,
                                            as.numeric(FA_EDUC_YRS) == 14 ~ 14,
                                            as.numeric(FA_EDUC_YRS) == 15 ~ 14,
                                            as.numeric(FA_EDUC_YRS) == 16 ~ 16,
                                            as.numeric(FA_EDUC_YRS) == 17 ~ 18,
                                            as.numeric(FA_EDUC_YRS) == 98 ~ 98,
                                            as.numeric(FA_EDUC_YRS) == 99 ~ 99
                                  ),
                                  case_when(as.numeric(FA_EDUC_YRS) == 1 &
                                              as.numeric(FA_EDUC_LE_8TH) < 9
                                            ~ as.numeric(FA_EDUC_LE_8TH),
                                            as.numeric(FA_EDUC_YRS) == 1 &
                                              as.numeric(FA_EDUC_LE_8TH) == 9
                                            ~ 99,                                            
                                            as.numeric(FA_EDUC_YRS) == 2 ~ 10,
                                            as.numeric(FA_EDUC_YRS) == 3 ~ 12,
                                            as.numeric(FA_EDUC_YRS) == 4 ~ 13,
                                            as.numeric(FA_EDUC_YRS) == 5 ~ 14,
                                            as.numeric(FA_EDUC_YRS) == 6 ~ 16,
                                            as.numeric(FA_EDUC_YRS) == 7 ~ 18,
                                            as.numeric(FA_EDUC_YRS) == 8 ~ 20,
                                            as.numeric(FA_EDUC_YRS) == 9 ~ 99
                                  )),
                  momedu = ifelse(year<=2002,
                                  case_when(as.numeric(MO_EDUC_YRS) == 0 ~ 0,
                                            as.numeric(MO_EDUC_YRS) == 1 ~ 1,
                                            as.numeric(MO_EDUC_YRS) == 2 ~ 2,
                                            as.numeric(MO_EDUC_YRS) == 3 ~ 3,
                                            as.numeric(MO_EDUC_YRS) == 4 ~ 4,
                                            as.numeric(MO_EDUC_YRS) == 5 ~ 5,
                                            as.numeric(MO_EDUC_YRS) == 6 ~ 6,
                                            as.numeric(MO_EDUC_YRS) == 7 ~ 7,
                                            as.numeric(MO_EDUC_YRS) == 8 ~ 8,
                                            as.numeric(MO_EDUC_YRS) == 9 ~ 9,
                                            as.numeric(MO_EDUC_YRS) == 10 ~ 10,
                                            as.numeric(MO_EDUC_YRS) == 11 ~ 11,
                                            as.numeric(MO_EDUC_YRS) == 12 ~ 12,
                                            as.numeric(MO_EDUC_YRS) == 13 ~ 13,
                                            as.numeric(MO_EDUC_YRS) == 14 ~ 14,
                                            as.numeric(MO_EDUC_YRS) == 15 ~ 14,
                                            as.numeric(MO_EDUC_YRS) == 16 ~ 16,
                                            as.numeric(MO_EDUC_YRS) == 17 ~ 18,
                                            as.numeric(MO_EDUC_YRS) == 98 ~ 98,
                                            as.numeric(MO_EDUC_YRS) == 99 ~ 99
                                  ),
                                  case_when(as.numeric(MO_EDUC_YRS) == 1 &
                                              as.numeric(MO_EDUC_LE_8TH) < 9
                                            ~ as.numeric(MO_EDUC_LE_8TH),
                                            as.numeric(MO_EDUC_YRS) == 1 &
                                              as.numeric(MO_EDUC_LE_8TH) == 9
                                            ~ 99,                                            
                                            as.numeric(MO_EDUC_YRS) == 2 ~ 10,
                                            as.numeric(MO_EDUC_YRS) == 3 ~ 12,
                                            as.numeric(MO_EDUC_YRS) == 4 ~ 13,
                                            as.numeric(MO_EDUC_YRS) == 5 ~ 14,
                                            as.numeric(MO_EDUC_YRS) == 6 ~ 16,
                                            as.numeric(MO_EDUC_YRS) == 7 ~ 18,
                                            as.numeric(MO_EDUC_YRS) == 8 ~ 20,
                                            as.numeric(MO_EDUC_YRS) == 9 ~ 99
                                  )),
                  maristat = case_when(MO_MARRIED == "Y" ~ 1,
                                       MO_MARRIED == "N" ~ 2,
                                       MO_MARRIED == "U" ~ 9),
                  lbnl = as.numeric(PRIOR_NOW_LIVING),
                  lbnd = as.numeric(PRIOR_DEAD),
                  lstlivyr = as.numeric(substr(DATE_LAST_BIRTH,1,4)),
                  lstlivmo = as.numeric(substr(DATE_LAST_BIRTH,5,6)),
                  last_spontan_year = ifelse(as.numeric(substr(DATE_LAST_SPONTAN,1,4)) < 8000,
                                             as.numeric(substr(DATE_LAST_SPONTAN,1,4)),
                                             NA),
                  last_induced_year = ifelse(as.numeric(substr(DATE_LAST_INDUCED,1,4)) < 8000,
                                             as.numeric(substr(DATE_LAST_INDUCED,1,4)),
                                             NA),
                  lstfetyr = ifelse(year < 2003,
                                    ifelse(is.na(last_spontan_year),
                                           last_induced_year,
                                           ifelse(is.na(last_induced_year),
                                                  last_spontan_year,
                                                  max(last_spontan_year,last_induced_year))),
                                    as.numeric(substr(DATE_OTHER_PREG_OUTCOMES,1,4))),
                  last_spontan_month = ifelse(as.numeric(substr(DATE_LAST_SPONTAN,1,4)) < 8000,
                                             as.numeric(substr(DATE_LAST_SPONTAN,5,6)),
                                             NA),
                  last_induced_month = ifelse(as.numeric(substr(DATE_LAST_INDUCED,1,4)) < 8000,
                                             as.numeric(substr(DATE_LAST_INDUCED,5,6)),
                                             NA),
                  lstfetmo = case_when(year < 2003 & is.na(last_spontan_year) ~ last_induced_month,
                                      year < 2003 & is.na(last_induced_year) ~ last_spontan_month,
                                      year < 2003 & last_spontan_year == last_induced_year & 
                                        last_spontan_month > 12 ~ last_induced_month,
                                      year < 2003 & last_spontan_year == last_induced_year & 
                                        last_induced_month > 12 ~ last_spontan_month,
                                      year < 2003 & last_spontan_year == last_induced_year & 
                                        last_spontan_month <= 12 & last_induced_month <= 12 
                                      ~ max(last_spontan_month, last_induced_month),
                                      year < 2003 & last_spontan_year > last_induced_year
                                      ~ last_induced_month,
                                      year < 2003 & last_spontan_year < last_induced_year
                                      ~ last_spontan_month,
                                      year >= 2003 ~ as.numeric(substr(DATE_OTHER_PREG_OUTCOMES,5,6))),
                  delivpay = case_when(SOURCE_OF_PAYMENT == "1" ~ 1,
                                       SOURCE_OF_PAYMENT == "2" ~ 2,
                                       SOURCE_OF_PAYMENT == "3" ~ 3,
                                       SOURCE_OF_PAYMENT == "4" ~ 4,
                                       SOURCE_OF_PAYMENT == "5" ~ 5,
                                       SOURCE_OF_PAYMENT == "6" ~ 6,
                                       SOURCE_OF_PAYMENT == "7" ~ 7,
                                       SOURCE_OF_PAYMENT == "8" ~ 8,
                                       SOURCE_OF_PAYMENT == "9" ~ 9,
                                       SOURCE_OF_PAYMENT == "U" ~ 9),
                  gestest = as.numeric(EST_GEST_PERIOD),
                  prenatvs = as.numeric(NUM_PRENATAL_VISITS),
                  numatbir = as.numeric(NUM_AT_BIRTH),
                  order = as.numeric(BIRTH_ORDER),
                  birgrams = as.numeric(BIRTH_WEIGHT_GRAMS),
                  num_other_preg_outcomes = ifelse(year < 2003,
                                                   ifelse(is.numeric(SPONTAN_20WKS_OR_GT) < 88 & 
                                                            is.numeric(SPONTAN_LT_20WKS) < 88 &
                                                            is.numeric(NUM_INDUCED) < 88,
                                                          is.numeric(SPONTAN_20WKS_OR_GT) +
                                                            is.numeric(SPONTAN_LT_20WKS) +
                                                            is.numeric(NUM_INDUCED)),
                                                   ifelse(is.numeric(OTHER_PREG_OUTCOMES) < 88,
                                                          is.numeric(OTHER_PREG_OUTCOMES),
                                                          NA)),
                  pripreg = ifelse(as.numeric(PRIOR_NOW_LIVING) < 88 & as.numeric(PRIOR_DEAD) < 88 &
                                     as.numeric(OTHER_PREG_OUTCOMES) < 88,
                                   as.numeric(PRIOR_NOW_LIVING) + as.numeric(PRIOR_DEAD)
                                   + as.numeric(OTHER_PREG_OUTCOMES),
                                   NA),
                  benefit = as.numeric(paste0(BENEFIT_PARTICIPATION1,BENEFIT_PARTICIPATION2)),
                  momrace = case_when(MO_RACE == "1" ~ 1,
                                   MO_RACE == "2" ~ 2,
                                   MO_RACE == "3" ~ 3,
                                   MO_RACE == "4" ~ 4,
                                   MO_RACE == "5" ~ 5,
                                   MO_RACE == "7" ~ 6,
                                   MO_RACE == "A" ~ 7,
                                   MO_RACE == "B" ~ 8,
                                   MO_RACE == "C" ~ 9,
                                   MO_RACE == "6" ~ 10,
                                   MO_RACE == "D" ~ 11,
                                   MO_RACE == "E" ~ 12,
                                   MO_RACE == "F" ~ 13,
                                   MO_RACE == "G" ~ 14,
                                   MO_RACE == "H" ~ 15,
                                   MO_RACE == "8" ~ 98,
                                   MO_RACE == "9" ~ 99),
                  dadrace = case_when(FA_RACE == "1" ~ 1,
                                   FA_RACE == "2" ~ 2,
                                   FA_RACE == "3" ~ 3,
                                   FA_RACE == "4" ~ 4,
                                   FA_RACE == "5" ~ 5,
                                   FA_RACE == "7" ~ 6,
                                   FA_RACE == "A" ~ 7,
                                   FA_RACE == "B" ~ 8,
                                   FA_RACE == "C" ~ 9,
                                   FA_RACE == "6" ~ 10,
                                   FA_RACE == "D" ~ 11,
                                   FA_RACE == "E" ~ 12,
                                   FA_RACE == "F" ~ 13,
                                   FA_RACE == "G" ~ 14,
                                   FA_RACE == "H" ~ 15,
                                   FA_RACE == "8" ~ 98,
                                   FA_RACE == "9" ~ 99),
                  kidrace = case_when(PERSON_RACE_CALC == "1" ~ 1,
                                      PERSON_RACE_CALC == "2" ~ 2,
                                      PERSON_RACE_CALC == "3" ~ 3,
                                      PERSON_RACE_CALC == "4" ~ 4,
                                      PERSON_RACE_CALC == "5" ~ 5,
                                      PERSON_RACE_CALC == "7" ~ 6,
                                      PERSON_RACE_CALC == "A" ~ 7,
                                      PERSON_RACE_CALC == "B" ~ 8,
                                      PERSON_RACE_CALC == "C" ~ 9,
                                      PERSON_RACE_CALC == "6" ~ 10,
                                      PERSON_RACE_CALC == "D" ~ 11,
                                      PERSON_RACE_CALC == "E" ~ 12,
                                      PERSON_RACE_CALC == "F" ~ 13,
                                      PERSON_RACE_CALC == "G" ~ 14,
                                      PERSON_RACE_CALC == "H" ~ 15,
                                      PERSON_RACE_CALC == "8" ~ 98,
                                      PERSON_RACE_CALC == "9" ~ 99),
                  mcnty = ifelse(RES_WA_CNTY_CITY_CODE != "0000",
                                 as.numeric(substr(RES_WA_CNTY_CITY_CODE,1,2)),
                                 NA),
                  sex = case_when(SEX == "M" ~ 1,
                                  SEX == "F" ~ 2,
                                  SEX == "U" ~ 9),
                  momage = as.numeric(MO_BIOLOGICAL_AGE),
                  dadage = as.numeric(FA_BIOLOGICAL_AGE),
                  momedu03 = as.numeric(MO_EDUC_YRS),
                  dadedu03 = as.numeric(FA_EDUC_YRS),
                  wic = case_when(WIC_BENEFITS == "N" ~ 0,
                                  WIC_BENEFITS == "Y" ~ 1,
                                  WIC_BENEFITS == "U" ~ 9),
                  prenatmo = as.numeric(MONTH_PRENATAL_CARE_BGN),
                  gestlen = ymd(PERSON_DOB) - ymd(DATE_OF_LAST_MENSES),
                  dadocc = as.numeric(FA_OCCUP_MILHAM),
                  momocc = as.numeric(MO_OCCUP_MILHAM),
                  ddays = as.numeric(ymd(DATE_OF_DEATH) - ymd(PERSON_DOB)),
                  GRAV = ifelse(as.numeric(PRIOR_NOW_LIVING) < 88 & 
                                  as.numeric(PRIOR_DEAD) < 88 & 
                                  as.numeric(SPONTAN_20WKS_OR_GT) < 88 & 
                                  as.numeric(SPONTAN_LT_20WKS) < 88,
                                as.numeric(PRIOR_NOW_LIVING) +
                                  as.numeric(PRIOR_DEAD) +
                                  as.numeric(SPONTAN_20WKS_OR_GT) +
                                  as.numeric(SPONTAN_LT_20WKS),
                                NA),
                  PARITY = ifelse(as.numeric(PRIOR_NOW_LIVING) < 88 & 
                                    as.numeric(PRIOR_DEAD) < 88,
                                  as.numeric(PRIOR_NOW_LIVING) +
                                    as.numeric(PRIOR_DEAD),
                                  NA),
                  moindex4 = ifelse(as.numeric(NUM_PRENATAL_VISITS) >= 0 & as.numeric(NUM_PRENATAL_VISITS) <= 90,
                                    case_when(as.numeric(MONTH_PRENATAL_CARE_BGN) >= 1 & as.numeric(MONTH_PRENATAL_CARE_BGN) <=2 ~ 4,
                                              as.numeric(MONTH_PRENATAL_CARE_BGN) >= 3 & as.numeric(MONTH_PRENATAL_CARE_BGN) <=4 ~ 3,
                                              as.numeric(MONTH_PRENATAL_CARE_BGN) >= 5 & as.numeric(MONTH_PRENATAL_CARE_BGN) <=6 ~ 2,
                                              (as.numeric(MONTH_PRENATAL_CARE_BGN) >= 7 & as.numeric(MONTH_PRENATAL_CARE_BGN) <=9) |
                                                as.numeric(MONTH_PRENATAL_CARE_BGN) == 0 |
                                                (Number_Prenatal_Visits == 0 & is.na(as.numeric(MONTH_PRENATAL_CARE_BGN))) ~ 1),
                                    0),
                  # We don't have calculated gestation period for 1999-2015, so
                  # clinical estimate is standing in here and elsewhere. See 2017
                  # variable construction for the way this should be if we get
                  # CALC_GEST_PERIOD.
                  gestlen = ifelse(as.numeric(EST_GEST_PERIOD) < 99,
                                   as.numeric(EST_GEST_PERIOD),
                                   NA),
                  gestest = ifelse(as.numeric(EST_GEST_PERIOD) < 99,
                                   as.numeric(EST_GEST_PERIOD),
                                   NA),
                  gest1 = ifelse(round(gestlen/7,1) >= 18 & round(gestlen/7,1) <= 50,
                                 round(gestlen/7,1),
                                 ifelse(gestest >= 18 & gestest <= 50,
                                        gestest,
                                        NA)),
                  premo = ifelse(!is.na(gest1) & ifelse(as.numeric(MONTH_PRENATAL_CARE_BGN) <= 9,
                                                        as.numeric(MONTH_PRENATAL_CARE_BGN),
                                                        NA) <= gest1/4,
                                 ifelse(as.numeric(MONTH_PRENATAL_CARE_BGN) <= 9,
                                        as.numeric(MONTH_PRENATAL_CARE_BGN),
                                        NA),
                                 NA),
                  prevs = ifelse(as.numeric(NUM_PRENATAL_VISITS) < 90,
                                 as.numeric(NUM_PRENATAL_VISITS),
                                 NA),
                  premo = ifelse((prevs == 0 & premo >= 1) | (premo == 0 & prevs >= 1),
                                 NA,
                                 premo),
                  prevs = ifelse((prevs == 0 & premo >= 1) | (premo == 0 & prevs >= 1),
                                 NA,
                                 prevs),
                  bgrams = ifelse(birgrams >= 400 & birgrams <= 6000,
                                  birgrams,
                                  NA),
                  gest1 = ifelse(is.na(gest1),
                                 case_when(sex == 1 & 530 <= bgrams & bgrams <= 608 ~ 22,
                                           sex == 1 & 609 <= bgrams & bgrams <= 698 ~ 23,
                                           sex == 1 & 699 <= bgrams & bgrams <= 799 ~ 24,
                                           sex == 1 & 800 <= bgrams & bgrams <= 912 ~ 25,
                                           sex == 1 & 913 <= bgrams & bgrams <= 1040 ~ 26,
                                           sex == 1 & 1041 <= bgrams & bgrams <= 1183 ~ 27,
                                           sex == 1 & 1184 <= bgrams & bgrams <= 1342 ~ 28,
                                           sex == 1 & 1343 <= bgrams & bgrams <= 1536 ~ 29,
                                           sex == 1 & 1537 <= bgrams & bgrams <= 1751 ~ 30,
                                           sex == 1 & 1752 <= bgrams & bgrams <= 1978 ~ 31,
                                           sex == 1 & 1979 <= bgrams & bgrams <= 2219 ~ 32,
                                           sex == 1 & 2220 <= bgrams & bgrams <= 2458 ~ 33,
                                           sex == 1 & 2459 <= bgrams & bgrams <= 2693 ~ 34,
                                           sex == 1 & 2694 <= bgrams & bgrams <= 2909 ~ 35,
                                           sex == 1 & 2910 <= bgrams & bgrams <= 3111 ~ 36,
                                           sex == 1 & 3112 <= bgrams & bgrams <= 3291 ~ 37,
                                           sex == 1 & 3292 <= bgrams & bgrams <= 3433 ~ 38,
                                           sex == 1 & 3434 <= bgrams & bgrams <= 3533 ~ 39,
                                           sex == 1 & 3534 <= bgrams & bgrams <= 6000 ~ 40,
                                           sex == 2 & 496 <= bgrams & bgrams <= 568 ~ 22,
                                           sex == 2 & 569 <= bgrams & bgrams <= 650 ~ 23,
                                           sex == 2 & 651 <= bgrams & bgrams <= 744 ~ 24,
                                           sex == 2 & 745 <= bgrams & bgrams <= 849 ~ 25,
                                           sex == 2 & 850 <= bgrams & bgrams <= 968 ~ 26,
                                           sex == 2 & 969 <= bgrams & bgrams <= 1101 ~ 27,
                                           sex == 2 & 1102 <= bgrams & bgrams <= 1251 ~ 28,
                                           sex == 2 & 1252 <= bgrams & bgrams <= 1429 ~ 29,
                                           sex == 2 & 1430 <= bgrams & bgrams <= 1636 ~ 30,
                                           sex == 2 & 1637 <= bgrams & bgrams <= 1860 ~ 31,
                                           sex == 2 & 1861 <= bgrams & bgrams <= 2089 ~ 32,
                                           sex == 2 & 2090 <= bgrams & bgrams <= 2328 ~ 33,
                                           sex == 2 & 2329 <= bgrams & bgrams <= 2561 ~ 34,
                                           sex == 2 & 2562 <= bgrams & bgrams <= 2787 ~ 35,
                                           sex == 2 & 2788 <= bgrams & bgrams <= 2991 ~ 36,
                                           sex == 2 & 2992 <= bgrams & bgrams <= 3160 ~ 37,
                                           sex == 2 & 3161 <= bgrams & bgrams <= 3293 ~ 38,
                                           sex == 2 & 3294 <= bgrams & bgrams <= 3388 ~ 39,
                                           sex == 2 & 3389 <= bgrams & bgrams <= 6000 ~ 40),
                                 gest1),
                  uexpvis = case_when(gest1 >= 35 ~ gest1 - 26,
                                      gest1 == 34 ~ 9,
                                      gest1 >= 32 ~ 8,
                                      gest1 >= 30 ~ 7,
                                      gest1 >= 26 ~ 6,
                                      gest1 >= 22 ~ 5,
                                      gest1 >= 18 ~ 4,
                                      gest1 >= 14 ~ 3,
                                      gest1 >= 10 ~ 2,
                                      gest1 >= 6 ~ 1,
                                      gest1 >= 0 ~ 0),
                  expvis = case_when(is.na(premo) | premo == 0 ~ uexpvis,
                                     premo == 10 ~ uexpvis - 17,
                                     premo == 9 ~ uexpvis - 13,
                                     premo == 8 ~ uexpvis - 9,
                                     premo == 7 ~ uexpvis - 7,
                                     premo == 6 ~ uexpvis - 6,
                                     premo == 5 ~ uexpvis - 5,
                                     premo == 4 ~ uexpvis - 3,
                                     premo == 3 ~ uexpvis - 2,
                                     premo == 2 ~ uexpvis - 1,
                                     premo == 1 ~ uexpvis),
                  expvis = ifelse(expvis <= 0,
                                  1,
                                  expvis),
                  evratio = (prevs/expvis) * 100,
                  evindex = case_when(is.na(evratio) | is.na(premo) ~ 0,
                                      evratio > 109.99 ~ 4,
                                      evratio > 79.99 ~ 3,
                                      evratio > 49.99 ~ 2,
                                      evratio <= 49.99 ~ 1),
                  indexsum = ifelse(evindex == 0 | moindex4 == 0,
                                    NA,
                                    ifelse(evindex == 1 | (moindex4 <= 2 & moindex4 >= 1),
                                           1,
                                           ifelse(evindex == 3 & moindex4 <= 4 & moindex4 >= 3,
                                                  3,
                                                  ifelse(evindex == 4 & moindex4 <= 4 & moindex4 >= 3,
                                                         4,
                                                         NA)))),
                  gest2 = ifelse(as.numeric(EST_GEST_PERIOD) >= 10 & as.numeric(EST_GEST_PERIOD) <= 60,
                                 as.numeric(EST_GEST_PERIOD),
                                 ifelse(as.numeric(EST_GEST_PERIOD) < 99,
                                        round(as.numeric(EST_GEST_PERIOD)/7,1),
                                        NA)),
                  sga = case_when(birgrams >= 326 & birgrams < 736 & 
                                    gest2 >= 22 & gest2 < 23 & sex == 1 ~ 2,
                                  birgrams < 326 & 
                                    gest2 >= 22 & gest2 < 23 & sex == 1 ~ 1,
                                  birgrams >= 736 & 
                                    gest2 >= 22 & gest2 < 23 & sex == 1 ~ 3,
                                  birgrams >= 376 & birgrams < 852 & 
                                    gest2 >= 23 & gest2 < 24 & sex == 1 ~ 2,
                                  birgrams < 376 &  
                                    gest2 >= 23 & gest2 < 24 & sex == 1 ~ 1,
                                  birgrams >= 852 &  
                                    gest2 >= 23 & gest2 < 24 & sex == 1 ~ 3,
                                  birgrams >= 433 & birgrams < 982 & 
                                    gest2 >= 24 & gest2 < 25 & sex == 1 ~ 2,
                                  birgrams < 433 &  
                                    gest2 >= 24 & gest2 < 25 & sex == 1 ~ 1,
                                  birgrams >= 982 &  
                                    gest2 >= 24 & gest2 < 25 & sex == 1 ~ 3,
                                  birgrams >= 499 & birgrams < 1127 & 
                                    gest2 >= 25 & gest2 < 26 & sex == 1 ~ 2,
                                  birgrams < 499 &  
                                    gest2 >= 25 & gest2 < 26 & sex == 1 ~ 1,
                                  birgrams >= 1127 &  
                                    gest2 >= 25 & gest2 < 26 & sex == 1 ~ 3,
                                  birgrams >= 574 & birgrams < 1288 & 
                                    gest2 >= 26 & gest2 < 27 & sex == 1 ~ 2,
                                  birgrams < 574 &  
                                    gest2 >= 26 & gest2 < 27 & sex == 1 ~ 1,
                                  birgrams >= 1288 &  
                                    gest2 >= 26 & gest2 < 27 & sex == 1 ~ 3,
                                  birgrams >= 662 & birgrams < 1466 & 
                                    gest2 >= 27 & gest2 < 28 & sex == 1 ~ 2,
                                  birgrams < 662 &  
                                    gest2 >= 27 & gest2 < 28 & sex == 1 ~ 1,
                                  birgrams >= 1466 &  
                                    gest2 >= 27 & gest2 < 28 & sex == 1 ~ 3,
                                  birgrams >= 762 & birgrams < 1661 & 
                                    gest2 >= 28 & gest2 < 29 & sex == 1 ~ 2,
                                  birgrams < 762 &  
                                    gest2 >= 28 & gest2 < 29 & sex == 1 ~ 1,
                                  birgrams >= 1661 &  
                                    gest2 >= 28 & gest2 < 29 & sex == 1 ~ 3,
                                  birgrams >= 878 & birgrams < 1873 & 
                                    gest2 >= 29 & gest2 < 30 & sex == 1 ~ 2,
                                  birgrams < 878 &  
                                    gest2 >= 29 & gest2 < 30 & sex == 1 ~ 1,
                                  birgrams >= 1873 &  
                                    gest2 >= 29 & gest2 < 30 & sex == 1 ~ 3,
                                  birgrams >= 1007 & birgrams < 2159 & 
                                    gest2 >= 30 & gest2 < 31 & sex == 1 ~ 2,
                                  birgrams < 1007 &  
                                    gest2 >= 30 & gest2 < 31 & sex == 1 ~ 1,
                                  birgrams >= 2159 &  
                                    gest2 >= 30 & gest2 < 31 & sex == 1 ~ 3,
                                  birgrams >= 1159 & birgrams < 2439 & 
                                    gest2 >= 31 & gest2 < 32 & sex == 1 ~ 2,
                                  birgrams < 1159 &  
                                    gest2 >= 31 & gest2 < 32 & sex == 1 ~ 1,
                                  birgrams >= 2439 &  
                                    gest2 >= 31 & gest2 < 32 & sex == 1 ~ 3,
                                  birgrams >= 1348 & birgrams < 2727 & 
                                    gest2 >= 32 & gest2 < 33 & sex == 1 ~ 2,
                                  birgrams < 1348 &  
                                    gest2 >= 32 & gest2 < 33 & sex == 1 ~ 1,
                                  birgrams >= 2727 &  
                                    gest2 >= 32 & gest2 < 33 & sex == 1 ~ 3,
                                  birgrams >= 1561 & birgrams < 2972 & 
                                    gest2 >= 33 & gest2 < 34 & sex == 1 ~ 2,
                                  birgrams < 1561 &  
                                    gest2 >= 33 & gest2 < 34 & sex == 1 ~ 1,
                                  birgrams >= 2972 &  
                                    gest2 >= 33 & gest2 < 34 & sex == 1 ~ 3,
                                  birgrams >= 1787 & birgrams < 3205 & 
                                    gest2 >= 34 & gest2 < 35 & sex == 1 ~ 2,
                                  birgrams < 1787 &  
                                    gest2 >= 34 & gest2 < 35 & sex == 1 ~ 1,
                                  birgrams >= 3205 &  
                                    gest2 >= 34 & gest2 < 35 & sex == 1 ~ 3,
                                  birgrams >= 2030 & birgrams < 3415 & 
                                    gest2 >= 35 & gest2 < 36 & sex == 1 ~ 2,
                                  birgrams < 2030 &  
                                    gest2 >= 35 & gest2 < 36 & sex == 1 ~ 1,
                                  birgrams >= 3415 &  
                                    gest2 >= 35 & gest2 < 36 & sex == 1 ~ 3,
                                  birgrams >= 2278 & birgrams < 3591 & 
                                    gest2 >= 36 & gest2 < 37 & sex == 1 ~ 2,
                                  birgrams < 2278 &  
                                    gest2 >= 36 & gest2 < 37 & sex == 1 ~ 1,
                                  birgrams >= 3591 &  
                                    gest2 >= 36 & gest2 < 37 & sex == 1 ~ 3,
                                  birgrams >= 2499 & birgrams < 3765 & 
                                    gest2 >= 37 & gest2 < 38 & sex == 1 ~ 2,
                                  birgrams < 2499 &  
                                    gest2 >= 37 & gest2 < 38 & sex == 1 ~ 1,
                                  birgrams >= 3765 &  
                                    gest2 >= 37 & gest2 < 38 & sex == 1 ~ 3,
                                  birgrams >= 2696 & birgrams < 3931 & 
                                    gest2 >= 38 & gest2 < 39 & sex == 1 ~ 2,
                                  birgrams < 2696 &  
                                    gest2 >= 38 & gest2 < 39 & sex == 1 ~ 1,
                                  birgrams >= 3931 &  
                                    gest2 >= 38 & gest2 < 39 & sex == 1 ~ 3,
                                  birgrams >= 2849 & birgrams < 4064 & 
                                    gest2 >= 39 & gest2 < 40 & sex == 1 ~ 2,
                                  birgrams < 2849 &  
                                    gest2 >= 39 & gest2 < 40 & sex == 1 ~ 1,
                                  birgrams >= 4064 &  
                                    gest2 >= 39 & gest2 < 40 & sex == 1 ~ 3,
                                  birgrams >= 2944 & birgrams < 4154 & 
                                    gest2 >= 40 & gest2 < 41 & sex == 1 ~ 2,
                                  birgrams < 2944 &  
                                    gest2 >= 40 & gest2 < 41 & sex == 1 ~ 1,
                                  birgrams >= 4154 &  
                                    gest2 >= 40 & gest2 < 41 & sex == 1 ~ 3,
                                  birgrams >= 3018 & birgrams < 4214 & 
                                    gest2 >= 41 & gest2 < 42 & sex == 1 ~ 2,
                                  birgrams < 3018 &  
                                    gest2 >= 41 & gest2 < 42 & sex == 1 ~ 1,
                                  birgrams >= 4214 &  
                                    gest2 >= 41 & gest2 < 42 & sex == 1 ~ 3,
                                  birgrams >= 3086 & birgrams < 4330 & 
                                    gest2 >= 42 & gest2 < 50 & sex == 1 ~ 2,
                                  birgrams < 3086 &  
                                    gest2 >= 42 & gest2 < 50 & sex == 1 ~ 1,
                                  birgrams >= 4330 &  
                                    gest2 >= 42 & gest2 < 50 & sex == 1 ~ 3,
                                  birgrams >= 314 & birgrams < 755 & 
                                    gest2 >= 22 & gest2 < 23 & sex == 2 ~ 2,
                                  birgrams < 314 & 
                                    gest2 >= 22 & gest2 < 23 & sex == 2 ~ 1,
                                  birgrams >= 755 & 
                                    gest2 >= 22 & gest2 < 23 & sex == 2 ~ 3,
                                  birgrams >= 354 & birgrams < 869 & 
                                    gest2 >= 23 & gest2 < 24 & sex == 2 ~ 2,
                                  birgrams < 354 &  
                                    gest2 >= 23 & gest2 < 24 & sex == 2 ~ 1,
                                  birgrams >= 869 &  
                                    gest2 >= 23 & gest2 < 24 & sex == 2 ~ 3,
                                  birgrams >= 400 & birgrams < 996 & 
                                    gest2 >= 24 & gest2 < 25 & sex == 2 ~ 2,
                                  birgrams < 400 &  
                                    gest2 >= 24 & gest2 < 25 & sex == 2 ~ 1,
                                  birgrams >= 996 &  
                                    gest2 >= 24 & gest2 < 25 & sex == 2 ~ 3,
                                  birgrams >= 454 & birgrams < 1136 & 
                                    gest2 >= 25 & gest2 < 26 & sex == 2 ~ 2,
                                  birgrams < 454 &  
                                    gest2 >= 25 & gest2 < 26 & sex == 2 ~ 1,
                                  birgrams >= 1136 &  
                                    gest2 >= 25 & gest2 < 26 & sex == 2 ~ 3,
                                  birgrams >= 518 & birgrams < 1290 & 
                                    gest2 >= 26 & gest2 < 27 & sex == 2 ~ 2,
                                  birgrams < 518 &  
                                    gest2 >= 26 & gest2 < 27 & sex == 2 ~ 1,
                                  birgrams >= 1290 &  
                                    gest2 >= 26 & gest2 < 27 & sex == 2 ~ 3,
                                  birgrams >= 591 & birgrams < 1460 & 
                                    gest2 >= 27 & gest2 < 28 & sex == 2 ~ 2,
                                  birgrams < 591 &  
                                    gest2 >= 27 & gest2 < 28 & sex == 2 ~ 1,
                                  birgrams >= 1460 &  
                                    gest2 >= 27 & gest2 < 28 & sex == 2 ~ 3,
                                  birgrams >= 678 & birgrams < 1645 & 
                                    gest2 >= 28 & gest2 < 29 & sex == 2 ~ 2,
                                  birgrams < 678 &  
                                    gest2 >= 28 & gest2 < 29 & sex == 2 ~ 1,
                                  birgrams >= 1645 &  
                                    gest2 >= 28 & gest2 < 29 & sex == 2 ~ 3,
                                  birgrams >= 780 & birgrams < 1845 & 
                                    gest2 >= 29 & gest2 < 30 & sex == 2 ~ 2,
                                  birgrams < 780 &  
                                    gest2 >= 29 & gest2 < 30 & sex == 2 ~ 1,
                                  birgrams >= 1845 &  
                                    gest2 >= 29 & gest2 < 30 & sex == 2 ~ 3,
                                  birgrams >= 902 & birgrams < 2113 & 
                                    gest2 >= 30 & gest2 < 31 & sex == 2 ~ 2,
                                  birgrams < 902 &  
                                    gest2 >= 30 & gest2 < 31 & sex == 2 ~ 1,
                                  birgrams >= 2113 &  
                                    gest2 >= 30 & gest2 < 31 & sex == 2 ~ 3,
                                  birgrams >= 1041 & birgrams < 2364 & 
                                    gest2 >= 31 & gest2 < 32 & sex == 2 ~ 2,
                                  birgrams < 1041 &  
                                    gest2 >= 31 & gest2 < 32 & sex == 2 ~ 1,
                                  birgrams >= 2364 &  
                                    gest2 >= 31 & gest2 < 32 & sex == 2 ~ 3,
                                  birgrams >= 1219 & birgrams < 2619 & 
                                    gest2 >= 32 & gest2 < 33 & sex == 2 ~ 2,
                                  birgrams < 1219 &  
                                    gest2 >= 32 & gest2 < 33 & sex == 2 ~ 1,
                                  birgrams >= 2619 &  
                                    gest2 >= 32 & gest2 < 33 & sex == 2 ~ 3,
                                  birgrams >= 1436 & birgrams < 2847 & 
                                    gest2 >= 33 & gest2 < 34 & sex == 2 ~ 2,
                                  birgrams < 1436 &  
                                    gest2 >= 33 & gest2 < 34 & sex == 2 ~ 1,
                                  birgrams >= 2847 &  
                                    gest2 >= 33 & gest2 < 34 & sex == 2 ~ 3,
                                  birgrams >= 1668 & birgrams < 3058 & 
                                    gest2 >= 34 & gest2 < 35 & sex == 2 ~ 2,
                                  birgrams < 1668 &  
                                    gest2 >= 34 & gest2 < 35 & sex == 2 ~ 1,
                                  birgrams >= 3058 &  
                                    gest2 >= 34 & gest2 < 35 & sex == 2 ~ 3,
                                  birgrams >= 1918 & birgrams < 3250 & 
                                    gest2 >= 35 & gest2 < 36 & sex == 2 ~ 2,
                                  birgrams < 1918 &  
                                    gest2 >= 35 & gest2 < 36 & sex == 2 ~ 1,
                                  birgrams >= 3250 &  
                                    gest2 >= 35 & gest2 < 36 & sex == 2 ~ 3,
                                  birgrams >= 2169 & birgrams < 3450 & 
                                    gest2 >= 36 & gest2 < 37 & sex == 2 ~ 2,
                                  birgrams < 2169 &  
                                    gest2 >= 36 & gest2 < 37 & sex == 2 ~ 1,
                                  birgrams >= 3450 &  
                                    gest2 >= 36 & gest2 < 37 & sex == 2 ~ 3,
                                  birgrams >= 2410 & birgrams < 3646 & 
                                    gest2 >= 37 & gest2 < 38 & sex == 2 ~ 2,
                                  birgrams < 2410 &  
                                    gest2 >= 37 & gest2 < 38 & sex == 2 ~ 1,
                                  birgrams >= 3646 &  
                                    gest2 >= 37 & gest2 < 38 & sex == 2 ~ 3,
                                  birgrams >= 2587 & birgrams < 3802 & 
                                    gest2 >= 38 & gest2 < 39 & sex == 2 ~ 2,
                                  birgrams < 2587 &  
                                    gest2 >= 38 & gest2 < 39 & sex == 2 ~ 1,
                                  birgrams >= 3802 &  
                                    gest2 >= 38 & gest2 < 39 & sex == 2 ~ 3,
                                  birgrams >= 2730 & birgrams < 3923 & 
                                    gest2 >= 39 & gest2 < 40 & sex == 2 ~ 2,
                                  birgrams < 2730 &  
                                    gest2 >= 39 & gest2 < 40 & sex == 2 ~ 1,
                                  birgrams >= 3923 &  
                                    gest2 >= 39 & gest2 < 40 & sex == 2 ~ 3,
                                  birgrams >= 2817 & birgrams < 4005 & 
                                    gest2 >= 40 & gest2 < 41 & sex == 2 ~ 2,
                                  birgrams < 2817 &  
                                    gest2 >= 40 & gest2 < 41 & sex == 2 ~ 1,
                                  birgrams >= 4005 &  
                                    gest2 >= 40 & gest2 < 41 & sex == 2 ~ 3,
                                  birgrams >= 2873 & birgrams < 4040 & 
                                    gest2 >= 41 & gest2 < 42 & sex == 2 ~ 2,
                                  birgrams < 2873 &  
                                    gest2 >= 41 & gest2 < 42 & sex == 2 ~ 1,
                                  birgrams >= 4040 &  
                                    gest2 >= 41 & gest2 < 42 & sex == 2 ~ 3,
                                  birgrams >= 2936 & birgrams < 4136 & 
                                    gest2 >= 42 & gest2 < 50 & sex == 2 ~ 2,
                                  birgrams < 2936 &  
                                    gest2 >= 42 & gest2 < 50 & sex == 2 ~ 1,
                                  birgrams >= 4136 &  
                                    gest2 >= 42 & gest2 < 50 & sex == 2 ~ 3),
                  gestlen = ymd(PERSON_DOB) - ymd(DATE_OF_LAST_MENSES)
                  )
# Warnings exist for lstfetyr and lstfetmo: NAs introduced by coercion. There is
# some weird data causing this (e.g. "YYYY99" and "  88/ 8"). OK to let it slide
# for 1999-2017, but check subsequent years as they are added.

birth_berd <- birth_berd_bedrock %>% 
  bind_rows(birth_berd_2017) %>% 
  mutate(bcert = ifelse(is.na(State_File_Number),
                        as.numeric(CERT_NUM),
                        as.numeric(State_File_Number))) %>% 
  select(bcert, CERT_NUM, State_File_Number, source_data, year: sga, howlgm,
         CERT_TYPE:WIC_BENEFITS, Birth_Cert_Type:Res_Geo_School_District)
