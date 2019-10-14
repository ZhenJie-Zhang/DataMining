# 用R呈現樞紐分析功能
install.packages("plyr")
library(plyr)
# 產生40筆隨機資料
df <- data.frame(
  group = c(rep('個人戶', 20), rep('企業戶', 20)),
  sex = sample(c("M", "F"), size = 40, replace = TRUE),
  age = floor(runif(n = 40, min = 25, max = 40)),
  bill = floor(runif(n = 40, min = 199, max = 2600))
)

# 利用group, sex進行分組，並計算年齡的平均數、標準差以及bill總和與平均
ddply(df, .(group, sex), summarize, 
      mean_age = round(mean(age), 2),
      sd_age = round(sd(age), 2),
      sum_bill = sum(bill), 
      mean_bill = round(mean(bill), 2)
      )
#計算資料筆數count
ddply(df, c('group','sex'), nrow)
ddply(df, c('group','sex','age'), nrow) #是不是很像樞紐分析表的原始資料
