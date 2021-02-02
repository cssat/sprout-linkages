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
      
      temp_data <- read_csv(
        file_list[i],
        col_types = cols_select(
          CERT_NUM = col_character(),
          LAST_NAME = col_character(),
          MIDDLE_NAME = col_character(),
          SUFFIX = col_character(),
          DATE_OF_DEATH = col_character(),
          DOB = col_character(),
          SSN = col_character(),
          RES_ZIP = col_character(),
          SEX = col_character()
        )
      )
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