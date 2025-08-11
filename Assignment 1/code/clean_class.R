rm(list = ls())

library(tidyverse)
library(readxl)

#read data
class_list <- readRDS("Desktop/ra-bootcamp-warmup/Assignment 1/data/original/class_cleaned.rds")

library(stringr)

convert_wareki_to_ad <- function(wareki_str) {
  if (is.na(wareki_str)) return(NA_character_)
  wareki_str <- as.character(wareki_str)
  if (str_detect(wareki_str, "平成")) {
    year_part <- str_extract(wareki_str, "\\d+")
    year_num <- as.numeric(year_part)
    return(as.character(1988 + year_num))
  }
  if (str_detect(wareki_str, "令和")) {
    year_part <- str_extract(wareki_str, "\\d+")
    year_num <- as.numeric(year_part)
    return(as.character(2018 + year_num))
  }
  return(wareki_str)
}

class_list <- lapply(class_list, function(df) {
  # 1行1列目の和暦取得
  wareki_year <- df[1,1]
  ad_year <- convert_wareki_to_ad(wareki_year)
  
  # 列名に西暦を付ける（元の列名をad_yearに置き換え）
  colnames(df)[1] <- paste0(ad_year)
  
  return(df)
})

processed_list <- lapply(class_list, function(df) {
  # 年度情報を列に追加
  df$year <- ad_year
  return(df)})