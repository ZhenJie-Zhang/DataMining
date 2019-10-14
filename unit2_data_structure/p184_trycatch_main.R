# ‧實務上R會搭配自動執行程式的排程軟體以批次方式(Batch 
#                          mode)在背景執行程式
# ‧當程式執行發生錯誤時就會停在錯誤的那一行而不繼續往
# 下執行，並導致程式被強制終止
# ‧tryCatch函式可協助偵測R程式遇到錯誤被強制終止的問題

setwd("E:/DataMining/unit2_data_structure/")
source("trycatch1.R", echo = TRUE)
source("trycatch2.R", echo = TRUE)
