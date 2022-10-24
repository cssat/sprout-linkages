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


# 1/29/2022 need to move some files
put_object(file = "s3://rodis-pii/output_chars/part-00000-44a1b353-671a-4984-8737-94c748a0a2b8-c000.snappy.parquet",
           object = "output_old/chars/part-00000-44a1b353-671a-4984-8737-94c748a0a2b8-c000.snappy.parquet",
           bucket = "rodis-pii")
file_list <- pull(data.table::rbindlist(get_bucket(bucket = "rodis-pii")) %>% 
                    filter(str_detect(Key,"44a1b353-671a-4984-8737-94c748a0a2b8") & Owner == "partnersforourchildren"),
                  Key)
for (i in 1:length(file_list)) {
  put_object(file = paste0("s3://rodis-pii/",file_list[i]),
             object = paste0("output_old/chars",
                             substr(file_list[i],13,nchar(file_list[i]))),
             bucket = "rodis-pii")
}
for (i in 1:length(file_list)) {
  delete_object(object = file_list[i],
             bucket = "rodis-pii")
}


