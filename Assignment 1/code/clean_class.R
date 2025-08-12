rm(list = ls())

library(tidyverse)
library(readxl)
library(stringr)
library(dplyr)
library(purrr)
library(janitor)

#read data
class_list <- readRDS("Desktop/ra-bootcamp-warmup/Assignment 1/data/original/class_cleaned_df.rds")

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
  # 和暦取得と変換
  wareki_year <- df[1, 1]
  ad_year <- convert_wareki_to_ad(wareki_year)
  # 列名設定
  colnames(df) <- as.character(unlist(df[1, ]))
  colnames(df) <- str_replace_all(colnames(df), "学級", "")
  colnames(df) <- str_replace_all(colnames(df), "以上", "plus")
  colnames(df)[1] <- "prefecture"
  # 「計」列削除
  df <- df[, !str_detect(colnames(df), "計")]
  # 1行目削除
  df <- df[-1, ]
  # 型を統一
  df <- df %>%
    mutate(
      prefecture = as.character(prefecture),
      year = as.integer(ad_year))
  return(df)})

# すべてのデータフレームの列型を揃える
class_list <- map(class_list, ~ mutate(.x, across(everything(), as.character)))

# 結合
all_df <- bind_rows(class_list)

pref_names <- c(
  "北海道","青森","岩手","宮城","秋田","山形","福島",
  "茨城","栃木","群馬","埼玉","千葉","東京","神奈川",
  "新潟","富山","石川","福井","山梨","長野","岐阜",
  "静岡","愛知","三重","滋賀","京都","大阪","兵庫",
  "奈良","和歌山","鳥取","島根","岡山","広島","山口",
  "徳島","香川","愛媛","高知","福岡","佐賀","長崎",
  "熊本","大分","宮崎","鹿児島","沖縄")

all_df <- all_df %>%
  mutate(pref_code = match(prefecture, pref_names))

# 1. 学級数（幅の中央値）を計算する関数
calc_midpoint <- function(range_str) {
  # 例えば "25～30" のような文字列を中央値に変換
  nums <- str_split(range_str, "～", simplify = TRUE)
  nums <- as.numeric(nums)
  mean(nums)
}

# 2. 幅のある列名を抽出（「～」を含む列名）
range_cols <- names(all_df)[str_detect(names(all_df), "～")]

# 3. それ以外の数字だけの列名を抽出（例: "0", "1", ..., "24"）
# ここでは「～」を含まない数字だけの列を取る方法の一例
num_cols <- names(all_df)[str_detect(names(all_df), "^\\d+$")]

# 4. 学級数（階級）を用意するために幅のある列名を中央値に変換
#    - 「25～30」→27.5 のように  
#    - これをdata.frameにまとめる（学級数として使う）

# 学級数ベクトルを作成
class_sizes <- c()

# 数字の列はそのまま数値化（学級数 = 列名の数字）
class_sizes[num_cols] <- as.numeric(num_cols)

# 幅のある列は中央値計算
class_sizes[range_cols] <- sapply(range_cols, calc_midpoint)

# 5. 全ての対象列名の学級数（代表値）ベクトルが完成
# これを元に、各行の学級数×学校数を計算する

# 6. pivot_longerでlong形式にして計算する方法

all_long <- all_df %>%
  pivot_longer(
    cols = -c(prefecture, year, pref_code),
    names_to = "class_range",
    values_to = "school_count"
  ) %>%
  mutate(
    # school_countを数値化
    school_count = as.numeric(school_count),
    # class_sizesを参照してclass_sizeを割り当て
    class_size = class_sizes[class_range],
    # 学級数×学校数を計算
    class_school = class_size * school_count
  )

# 7. 年・都道府県ごとに合計

summary_df <- all_long %>%
  group_by(year, prefecture, pref_code) %>%  # pref_codeもグループに含める
  summarise(
    total_class_school = sum(class_school, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(pref_code, year) 


