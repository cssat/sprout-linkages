######################################################################
#                                                                    #
# Project:  RODIS                                                    #
#                                                                    #
# File: rodis_road_map.R                                             #
#                                                                    #
# Purpose: Document work flow of sprout-linkages project             #
#          old BERD SQL table.                                       #
#                                                                    #
# Procedures: Theoretically, this file should run from start to      #
#             finish, but we don't usually need to run everything    #
#             at once.
#                                                                    #
# Programmers: Karen Segar                                           #
#                                                                    #
# Dates: 12/17/2021 created                                          #
#                                                                    #
######################################################################

# Functions for downloading and processing data
source("helpers.R")

# Download data from Google drive. Where will it go when Joe leaves?
source("download_from_google_drive.R")

# The Famlink file used quotation marks inconsistently, making it hard
# to read in, so I edited it. Upload the edited file to Google drive.
drive_upload(
  media = "raw_data/famlink/CW Involvement and Risk for Injury Study.rpt_edited.txt",
  name = "CW Involvement and Risk for Injury Study.rpt_edited.txt",
  path = rodis_pii_dribble
)

# The Famlink file had people who were not involved with child welfare
# cases, so we pulled a list of people involved in allegations and
# removals.
# See removals.sql

# Upload the file exported from CA_ODS database to Google drive.
drive_upload(
  media = "raw_data/famlink/ca_ods_allegation_removal.csv",
  name = "ca_ods_allegation_removal.csv",
  path = rodis_pii_dribble
)

# Read the raw files and process the identifiers we will use to 
# match records in Glue
source("prep_for_glue.R")

# This is where we start working with AWS. See RODIS AWS steps.docx for details.

# training_data_for_Glue.R has the history of how the labeling files were
# constructed. It was necessary to correct the source data a couple of times
# to fill in missing data due to changes in data formats from year to year. 
# When this happens, the labels need to be reconstructed with corrected
# data and we need to overwrite the original ones when training the 
# machine learning transform.

# files_for_Link_Plus.R contains a record of how files were constructed to
# use in Link Plus. Most recent files are at the beginning of the file.

# RODIS_LinkPlusMatches.R has a function for reading matches produced by
# Link Plus and randomly selecting blocks with matches that would make
# good training sets for Glue.

# RODIS_AWS_S3_upload.R has the beginnings of a scripting solution to
# moving files up and down from AWS S3 buckets. But there's an access
# problem with the R code that the command line interface is not
# encountering with the same credentials.

# rodis_berd.R processes the birth data with the goal of matching
# the berd table in the CA_ODS SQL database. 

# Move this to sprout-linkages project?
# ..\scripts\BERD_decode_id.sas converts the id number in the SQL BERD table
# back to bcertnum so we can compare data in the old table to what we
# currently have. 

# rodis_berd_check.R compares records produced by rodis_berd.R to records in
# old BERD SQL table. Data is in ..\data_working. Move to sprout-linkages project?
# Contains reference to data in memory from rodis_berd.R.

# rodis_chars.R reads and processes the CHARS data. This file is just a stub for now.





