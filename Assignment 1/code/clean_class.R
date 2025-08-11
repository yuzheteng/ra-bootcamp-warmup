rm(list = ls())

library(tidyverse)
library(readxl)

#read data
class_list <- readRDS("Desktop/ra-bootcamp-warmup/Assignment 1/data/original/class_cleaned_df.rds")

library(stringr)
library(dplyr)
library(purrr)
library(janitor) # make_clean_names() 用

# 和暦（平成・令和）を西暦に変換する関数
convert_wareki_to_ad <- function(wareki_str) {
  if (is.na(wareki_str)) return(NA_character_)
  wareki_str <- as.character(wareki_str)
  if (str_detect(wareki_str, "平成")) {
    year_part <- str_extract(wareki_str, "\\d+")
    year_num <- as.numeric(year_part)
    return(as.character(1988 + year_num))}
  if (str_detect(wareki_str, "令和")) {
    year_part <- str_extract(wareki_str, "\\d+")
    year_num <- as.numeric(year_part)
    return(as.character(2018 + year_num))}
  return(wareki_str)}

# まず名前がなければ名前をつける（必須）
if(is.null(names(class_list))) {
  names(class_list) <- paste0("df", seq_along(class_list))}
# 西暦名を取得してリスト名にセット
names(class_list) <- sapply(class_list, function(df) {
  wareki_year <- df[1, 1]
  convert_wareki_to_ad(wareki_year)})

class_list <- map(class_list, function(df) {
  # 1行目1列目から和暦取得
  wareki_year <- df[1, 1]
  # 西暦に変換
  ad_year <- convert_wareki_to_ad(wareki_year)
  # 1行目を列名として設定
  colnames(df) <- as.character(unlist(df[1, ]))
  colnames(df) <- str_replace_all(colnames(df), "学級", "")
  colnames(df)[1] <- "prefecture"  # 1列目は都道府県
  # 「計」列削除（total列など）
  df <- df[, !str_detect(colnames(df), "計")]
  # 1行目を削除
  df <- df[-1, ]
  # year列を追加（文字列として入れるなら as.character(), 数値なら as.numeric()）
  df <- mutate(df, year = as.numeric(ad_year))
  return(df)})
