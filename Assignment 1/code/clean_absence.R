library(tidyverse)
library(readxl)

#cleaning
absence_list <- readRDS("Desktop/ra-bootcamp-warmup/Assignment 1/data/original/schoolrefusal_df.rds")
if (is.null(names(absence_list)) || any(names(absence_list) == "")) { names(absence_list) <- as.character(2013:2022)}

# blank列の削除・型変換・year列の追加
absence_cleaned <- absence_list %>%
  # 各データフレームに処理を適用
  map(~ .x %>%
        # blank列の削除
        select(-blank) %>%
        # 文字列列を文字列型に、数値列を数値型に変換（必要に応じて）
        mutate(
          prefecture = as.character(prefecture),
          absencestudents = as.numeric(absencestudents)
        )) %>%
  # listを1つのデータフレームにまとめる（名前＝year）
  bind_rows(.id = "year") %>%
  # yearを整数型に変換（必要に応じて）
  mutate(year = as.integer(year))

#read student data
student_df   <- readRDS("Desktop/ra-bootcamp-warmup/Assignment 1/data/original/student_df.rds")  
# 生徒数を左、不登校者数を右に結合（year, prefectureで）
absence_merged <- student_df %>%
  left_join(absence_cleaned, by = c("year", "prefecture"))

# 不登校割合を計算
absence_merged <- absence_merged %>%
  mutate(absence_rate = absencestudents / students)
  
#save data
saveRDS(absence_merged, file = "Desktop/ra-bootcamp-warmup/Assignment 1/data/cleaned/absence_cleaned.rds")

