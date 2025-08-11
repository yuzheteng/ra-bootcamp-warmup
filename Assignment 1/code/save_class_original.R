library(tidyverse)
library(readxl)

# Read data
file_list <- list.files("Desktop/ra-bootcamp-warmup/Assignment 1/data/raw/学級数", pattern = "\\.xlsx$",  full.names = TRUE)
class_list <- map(file_list, read_excel)

rename_columns <- function(df) {
  new_names <- names(df) %>%
    str_replace("計", "total") %>%
    str_replace("学級", "class_") %>%
    str_replace("以上", "_plus") 
  names(df) <- new_names
  return(df)}

class_list <- map(class_list, rename_columns)

#save data
saveRDS(class_list, file = "Desktop/ra-bootcamp-warmup/Assignment 1/data/original/class_cleaned_df.rds")
