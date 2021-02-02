# sprout-linkages
A repo to coordinate scripts to link data in the RODIS project.

There are three main scripts in this repo: 

1. A script that connects authorized persons to a restricted UW Google Drive to facilitate data downloads (`download_from_google_drive.R`),  
2. A script that preps data for subsequent linkages in AWS Glue (`prep_for_glue.R`), and 
3. A helper script sourced by the other two scripts (`helpers.R`). 

NOTES: 

* The `linkage_fields` directory contains a list of fields that are used to streamline the unioning of source data across multiple years and schemas. 
* The `raw_data` directory contains a file structure that is expected by the various scripts. Restricted data will be downloaded into these directories when the scripts are run by authorized persons. **DO NOT remove the `.gitignore` files from these directories!**
* The `prep_for_glue.R` script only produces unioned files for parents and children pulled from birth and chars data. This logic should be repeated with Famlink and death data once we are satisfied with the matches across birth and chars data. 
