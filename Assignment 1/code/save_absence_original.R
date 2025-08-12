library(tidyverse)
library(readxl)

#Read Data(student)
data1 <- read_excel("Desktop/ra-bootcamp-warmup/Assignment 1/data/raw/生徒数/生徒数.xlsx")

data1 <- data1 %>% rename( prefecture = `都道府県`, year = `年度`, students = `生徒数`)

saveRDS(data1, file = "Desktop/ra-bootcamp-warmup/Assignment 1/data/original/student_df.rds")

rm(list = ls())
#Read Data(absencestudent)
file_list <- list.files("Desktop/ra-bootcamp-warmup/Assignment 1/data/raw/不登校生徒数", pattern = "\\.xlsx$",  full.names = TRUE)
absence_list <- map(file_list, read_excel)
absence_list <- absence_list %>% map(~ rename(.x, prefecture = `都道府県`, absencestudents = `不登校生徒数`))
saveRDS(absence_list, file = "Desktop/ra-bootcamp-warmup/Assignment 1/data/original/schoolrefusal_df.rds")

