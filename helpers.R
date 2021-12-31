get_file_ids <- function(detection_string) {
  rodis_pii_files %>%
    filter(stringr::str_detect(name, detection_string))
}

download_multiple <- function(file_dribble, sub_dir, overwrite) {
  file_count <- nrow(file_dribble)
  
  for (i in 1:file_count) {
    drive_download(
      file = file_dribble[i, ],
      path = paste0(getwd(),
                    "/raw_data/",
                    sub_dir,
                    "/",
                    file_dribble$name[i]),
      overwrite = overwrite
    )
  }
  
}


read_linkage_data <- function(pii_type) {
  #create a list of the files from your target directory
  file_list <- list.files(path = paste0(getwd(),
                                        "/raw_data/",
                                        pii_type),
                          full.names = TRUE)
  
  #get a stored list of columns to be used in linking each type of PII
  linkage_fields <- readLines(paste0(getwd(),
                                     "/linkage_fields/",
                                     pii_type))
  
  #initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
  dataset <- tibble()
  
  if (pii_type %in% c("birth", "chars")) {
    #had to specify columns to get rid of the total column
    for (i in 1:length(file_list)) {
      message(paste0("reading file ", file_list[i]))
      
      temp_data <- read_sas(file_list[i], 
                            col_select = any_of(linkage_fields)) %>%
        mutate(source_data = file_list[i])
      dataset <-
        bind_rows(list(dataset, temp_data)) #for each iteration, bind the new data to the building dataset
    }
    return(dataset)
  } else if (pii_type %in% c("death_name")) {
    for (i in 1:length(file_list)) {
      message(paste0("reading file ", file_list[i]))
      if (as.numeric(str_extract(file_list[i],"(?<=DeathNamesF)[[:digit:]]{4}")) %in% 1999:2009) {
        temp_data <- read_csv(
          file_list[i],
          col_types = cols_only(
            CERT_NUM = col_character(),
            LAST_NAME = col_character(),
            FIRST_NAME = col_character(),
            MIDDLE_NAME = col_character(),
            SUFFIX = col_character(),
            DATE_OF_DEATH = col_character(),
            DOB = col_character(),
            SSN = col_character(),
            RES_ZIP = col_character(),
            SEX = col_character()
          )) %>% 
            mutate(source_data = file_list[i])
      }
      else if (as.numeric(str_extract(file_list[i],"(?<=DeathNamesF)[[:digit:]]{4}")) %in% 2010:2015) {
        temp_data <- read_csv(
          file_list[i],
          col_types = cols_only(
            `CERT-NUM` = col_character(),
            `LAST-NAME` = col_character(),
            `FIRST-NAME` = col_character(),
            `MIDDLE-NAME` = col_character(),
            SUFFIX = col_character(),
            `DATE-OF-DEATH` = col_character(),
            DOB = col_character(),
            SSN = col_character(),
            `RES-ZIP` = col_character(),
            SEX = col_character()
          )) %>% 
          rename(CERT_NUM = `CERT-NUM`,
                 LAST_NAME = `LAST-NAME`,
                 FIRST_NAME = `FIRST-NAME`,
                 MIDDLE_NAME = `MIDDLE-NAME`,
                 DATE_OF_DEATH = `DATE-OF-DEATH`,
                 RES_ZIP = `RES-ZIP`
          ) %>% 
          mutate(source_data = file_list[i])
      }
      else if (as.numeric(str_extract(file_list[i],"(?<=DeathNamesF)[[:digit:]]{4}")) %in% 2016:2017) {
        temp_data <- read_csv(
          file_list[i],
          col_types = cols_only(
            `State File Number` = col_character(),
            `Decedent Last Name` = col_character(),
            `Decedent First Name` = col_character(),
            `Decedent Middle Name` = col_character(),
            `Decedent Suffix` = col_character(),
            `Date of Death` = col_character(),
            `Date of Birth` = col_character(),
            `Social Security Number` = col_character(),
            `Residence Zip Code` = col_character(),
            Sex = col_character()
          )) %>% 
          rename(CERT_NUM = `State File Number`,
                 LAST_NAME = `Decedent Last Name`,
                 FIRST_NAME = `Decedent First Name`,
                 MIDDLE_NAME = `Decedent Middle Name`,
                 SUFFIX = `Decedent Suffix`,
                 DATE_OF_DEATH = `Date of Death`,
                 DOB = `Date of Birth`,
                 SSN = `Social Security Number`,
                 RES_ZIP = `Residence Zip Code`,
                 SEX = Sex
          ) %>% 
          mutate(DATE_OF_DEATH = gsub("-","",as.character(mdy(DATE_OF_DEATH))),
                 DOB = gsub("-","",as.character(mdy(DOB))),
                 source_data = file_list[i])
      }
      dataset <-
        bind_rows(list(dataset, temp_data)) #for each iteration, bind the new data to the building dataset
    }
    return(dataset)
  } else if (pii_type %in% c("death_stat")) {
    datalist <- vector(mode = "list", length = length(file_list))
    for (i in 1:length(file_list)) {
      message(paste0("reading file ", file_list[i]))
      temp_data <- suppressWarnings(read_excel(file_list[i],
                                               col_types = "text")) #suppressing warnings here as they are not 
      temp_data <-  suppressWarnings(temp_data %>%
                                       select(one_of(linkage_fields)
                                       )
      )
      datalist[[i]] <- temp_data  #for each iteration, add the new data to the building data list
    }
    return(datalist)
  }
  
  
}