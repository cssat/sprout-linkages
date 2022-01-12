######################################################################
#                                                                    #
# Project:  RODIS                                                    #
#                                                                    #
# File: RODIS_AWS_S3_upload.R                                        #
#                                                                    #
# Purpose: Upload raw source files for births, deaths, CHARS, and    #
#          Famlink to S3 buckets                                     #
#                                                                    #
# Procedures: Run this just once in interactive mode                 #
#                                                                    #
# Programmers: Karen Segar                                           #
#                                                                    #
# Dates: 12/16/2021 created                                          #
#        12/24/2021 all files uploaded                               #
#                                                                    #
######################################################################

library(aws.s3)
#library(aws.signature)

# Run these lines one at a time (interactive mode)
# access_key_id <- readline("Enter your AWS access key ID (20-character code): ")
# secret_access_key <- readline("Enter your AWS secret access key: ")
# 
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

s3sync(path = "H:/RODIS/CSSAT/data_raw/original files/birth",
       bucket = "rodis-pii",
       prefix = "data_raw/birth/",
       direction = "upload",
       multipart = TRUE)

s3sync(path = "H:/RODIS/CSSAT/data_raw/original files/chars",
       bucket = "rodis-pii",
       prefix = "data_raw/CHARS/",
       direction = "upload",
       multipart = TRUE)

s3sync(path = "H:/RODIS/CSSAT/data_raw/original files/death",
       bucket = "rodis-pii",
       prefix = "data_raw/death/",
       direction = "upload",
       multipart = TRUE)

s3sync(path = "H:/RODIS/CSSAT/data_raw/famlink",
       bucket = "rodis-pii",
       prefix = "data_raw/Famlink/",
       direction = "upload",
       multipart = TRUE)
