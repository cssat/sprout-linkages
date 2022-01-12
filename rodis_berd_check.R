######################################################################
#                                                                    #
# Project:  RODIS                                                    #
#                                                                    #
# File: rodis_berd_check.R                                           #
#                                                                    #
# Purpose: Compare records produced by rodis_berd.R to records in    #
#          old BERD SQL table.                                       #
#                                                                    #
# Procedures: Run BERD_decode_id.sas first to get birth certificate  #
#             numbers from bc_uni. Run rodis_berd.R to generate      #
#             birth_berd data frame.                                 #
#                                                                    #
# Programmers: Karen Segar                                           #
#                                                                    #
# Dates: 10/21/2021 created                                          #
#                                                                    #
######################################################################

library(summarytools)
library(haven)
options("scipen" = 20)

# Make the negative bcertnums into negative IDs in order to avoid
# duplication. These are probably fetal death certificates, which
# we aren't interested in. They still don't explain the difference
# in N between the SQL records and the new files from DOH.
berd_bcert <- read_sas("../data_working/berd_bc_uni.sas7bdat") %>% 
  mutate(bcert = ifelse(bcertnum>0,
                        (file_year * 1000000) + bcertnum,
                        -1 * ((file_year * 1000000) - bcertnum)))

compare <- birth_berd %>% 
  select(bcert:sga) %>% 
  full_join(berd_bcert %>% select(bcert,bc_uni,fdid),
            by = "bcert")

not_birth_berd <- compare %>% 
  filter(is.na(source_data)) %>% 
  select(bcert,bc_uni,fdid)
freq(not_birth_berd$fdid) # Mostly not fetal deaths

berd_births <- read_csv("../data_working/berd_births.csv",
                        col_types = cols(bintend = col_number(),
                                         payid = col_number()),
                        na = "NULL")

compare_birth_berd <- compare %>% 
  full_join(berd_births, by = "bc_uni") %>% 
  filter(fdid.x == 0) %>% 
  arrange(bcert) %>% 
  mutate(birth_rec = ifelse(is.na(source_data),
                            0,1))

ctable(compare_birth_berd$birthyr.y,compare_birth_berd$birth_rec,prop = "r")
# Number of records in SQL table that are not in files from DOH decreases
# with file year. We think this is due to original birth certificates for
# adopted children being removed.
table(birth_berd$year)
# Years after 2013 also missing 220-365 records, based on published live births.

ctable(compare_birth_berd$bintend,compare_birth_berd$birth_rec, prop = "c")
ctable(compare_birth_berd$trib_res.y,compare_birth_berd$birth_rec, prop = "c")

ctable(compare_birth_berd$birplace.y, compare_birth_berd$birth_rec, prop = "c")

ctable(compare_birth_berd$birfacil.y,compare_birth_berd$birth_rec, prop = "c")

ctable(compare_birth_berd$bcnty,compare_birth_berd$birth_rec, prop = "c")

ctable(compare_birth_berd$bres.y,compare_birth_berd$birth_rec, prop = "c")

ctable(compare_birth_berd$diedyr.y,compare_birth_berd$birth_rec, prop = "c")

ctable(compare_birth_berd$attend,compare_birth_berd$birth_rec, prop = "c")

ctable(compare_birth_berd$cert,compare_birth_berd$birth_rec, prop = "c")

ctable(compare_birth_berd$delivpay.y,compare_birth_berd$birth_rec, prop = "c")
# These look a little different, with missing records more likely to have
# Medicaid or no payer indicated, and less likely to have private insurance.

ctable(compare_birth_berd$payid,compare_birth_berd$birth_rec, prop = "c")
# Similar issue here, but more missing data among non-missing records.

ctable(compare_birth_berd$whdied,compare_birth_berd$birth_rec, prop = "c")

ctable(compare_birth_berd$mcnty.y,compare_birth_berd$birth_rec, prop = "c")

ctable(compare_birth_berd$nicu,compare_birth_berd$birth_rec, prop = "c")
# Missing records more likely to have NICU.

ctable(compare_birth_berd$csection,compare_birth_berd$birth_rec, prop = "c")

ctable(compare_birth_berd$icu,compare_birth_berd$birth_rec, prop = "c")
# Missing records more likely to have ICU.

ctable(compare_birth_berd$case,compare_birth_berd$birth_rec, prop = "c")
# This variable looks really different between the records we have and the
# ones we don't. For records we have, 1/4 are "case". For those we don't, 2/3
# are "case".

# I checked a couple of the raw files in SAS to make sure R wasn't flaking out,
# and N was the same, missing records matched not_birth_berd. Our N
# is consistently lower than published live births:
# https://www.doh.wa.gov/Portals/1/Documents/Pubs/422-099-2018-2010-VitalStatHighlights.pdf

table(compare_birth_berd$birthyr.x,compare_birth_berd$birthyr.y)
table(compare_birth_berd$birthmo.x,compare_birth_berd$birthmo.y)
#View(compare_birth_berd %>% filter(birthmo.x != birthmo.y) %>% 
#       select(bcert, year, ends_with(".x"), ends_with(".y")))

# Check all of the variables computed in rodis_berd.R against their
# values in ODS_CA.rodis.berd.
berd_births_compare <- read_csv("../data_working/berd_births_compare.csv",
                        na = "NULL")

# fdid was computed from bc_uni and doesn't exist beyond 2013
check_birth_berd <- berd_births_compare %>%
  inner_join(berd_bcert %>% select(bcert,bc_uni),
            by = "bc_uni") %>% 
  filter(bcert %in% birth_berd$bcert) %>% 
  select(bcert, birthyr, birthmo, birthdy, trib_res, momle8ed, dadle8ed, 
         otherout, birplace, birfacil, bcountry, bres, diedyr, diedmo, dieddy, 
         partype, dadbirst, dcountry, dadbiryr, dadbirmo, dadbirdy, mombirst, 
         mcountry, mombiryr, mombirmo, mombirdy, momres, howlgy, howlgm, 
         chisp, dadhisp, momhisp, dadedu, momedu, maristat, lbnl, lbnd, 
         lstlivyr, lstlivmo, lstfetyr, lstfetmo, delivpay, gestest, prenatvs, 
         numatbir, order, birgrams, pripreg, benefit, momrace, dadrace, 
         kidrace, mcnty, sex, momage, dadage, momedu03, dadedu03, wic, 
         prenatmo, gestlen, dadocc, momocc, ddays, GRAV, PARITY, moindex4, 
         evindex, indexsum, sga)

birth_berd_to_2013 <- birth_berd %>% 
  filter(bcert %in% check_birth_berd$bcert) %>% 
  select(bcert, birthyr, birthmo, birthdy, trib_res, momle8ed, dadle8ed, 
         otherout, birplace, birfacil, bcountry, bres, diedyr, diedmo, dieddy, 
         partype, dadbirst, dcountry, dadbiryr, dadbirmo, dadbirdy, mombirst, 
         mcountry, mombiryr, mombirmo, mombirdy, momres, howlgy, howlgm, 
         chisp, dadhisp, momhisp, dadedu, momedu, maristat, lbnl, lbnd, 
         lstlivyr, lstlivmo, lstfetyr, lstfetmo, delivpay, gestest, prenatvs, 
         numatbir, order, birgrams, pripreg, benefit, momrace, dadrace, 
         kidrace, mcnty, sex, momage, dadage, momedu03, dadedu03, wic, 
         prenatmo, gestlen, dadocc, momocc, ddays, GRAV, PARITY, moindex4, 
         evindex, indexsum, sga) %>% 
  mutate(gestlen = as.numeric(gestlen))

birth_berd_diff <- setdiff(birth_berd_to_2013, check_birth_berd)

birth_berd_birthmo_diff <- setdiff(birth_berd_to_2013 %>% select(bcert,birthmo),
                                 check_birth_berd %>% select(bcert,birthmo)) %>% 
  inner_join(check_birth_berd %>% select(bcert,birthmo),by = "bcert")
birth_berd_birthdy_diff <- setdiff(birth_berd_to_2013 %>% select(bcert,birthdy),
                                   check_birth_berd %>% select(bcert,birthdy)) %>% 
  inner_join(check_birth_berd %>% select(bcert,birthdy),by = "bcert") %>% 
  mutate(varname = "birthdy")
birth_berd_trib_diff <- setdiff(birth_berd_to_2013 %>% select(bcert,trib_res),
                                   check_birth_berd %>% select(bcert,trib_res)) %>% 
  inner_join(check_birth_berd %>% select(bcert,trib_res),by = "bcert") %>% 
  mutate(varname = "trib_res")

# Check the value computed from the new data against the value from the BERD table.
compare_values <- lapply(names(birth_berd_to_2013)[2:length(names(birth_berd_to_2013))],
                         function(x) {
                           df <- setdiff(birth_berd_to_2013[c("bcert",x)],
                                         check_birth_berd[c("bcert",x)])
                           df <- df %>% 
                             inner_join(check_birth_berd[c("bcert",x)],
                                        by = "bcert")
                           df$varname <- x
                           names(df) <- c("bcert","valbirth","valberd","varname")
                           df <- df %>% 
                             mutate(valbirth = as.character(valbirth),
                                    valberd = as.character(valberd))
                           return(df)
                         })

compare_values_df <- bind_rows(compare_values)

compare_values_var_count <- compare_values_df %>% 
  group_by(varname) %>% 
  count() %>% 
  arrange(desc(n))
View(compare_values_var_count)
write_csv(compare_values_var_count,"../metadata/berd_discrepancies_20211102.csv")

# indexsum is very complicated to calculate (see variable diagram in Google drive in data_desc) and
# has many discrepancies with BERD table. Are we going to use this variable, or should I drop it?
# Same story for evindex and moindex4.

# There are a lot of discrepancies in birth state for mother and father. Maybe the BERD table vars
# came from literals or the FIPS letter code? Most common discrepancy is a known state code in
# current data having and unknown code in old data.

# BERD table has 133 non-NA values for dadedu03 for which the data we have are clearly 99/9 unknown.
# momedu03 only has 1 such case, but both have lots of data that was missing in BERD table.

# Recode APGAR = 0 to NA?

# dcountry & mcountry: BERD has a lot of data that we don't have. Were they from the FIPS letter codes?

# sga

# lstfetmo/lstfetyr doesn't seem to include induced terminations

# momedu/dadedu coding seems different 14 new = 15 old, 18 new =  17 old

# ncigs I can't tell what the algorithm for calculating this was in the BERD table.
# Sometimes it looks like a (rounded) average, sometimes it looks like it was
# selecting a non-zero trimester. All are integers.

# PARITY BERD seems to be assuming that a missing code = 0

# mcnty a lot of missing data in BERD could be filled in with RES_CNTY_CODE, but there
# are some mystery codes

# dad DOB discrepancies appear to be due to changes in data. We have a lot of DOBs that
# BERD doesn't, but sometimes the opposite is true

