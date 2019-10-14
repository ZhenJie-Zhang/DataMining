# p305_用vcd計算列聯表百分比 ----
# install.packages("vcd")
library(vcd)
# 產生40筆隨機資料
df <- data.frame(
  group = c(rep('個人戶', 20), rep('企業戶', 20)),
  sex = sample(c("M", "F"), size = 40, replace = TRUE),
  age = floor(runif(n = 40, min = 25, max = 40)),
  bill = floor(runif(n = 40, min = 199, max = 2600))
)
# bill的百分比統計
prop.table(df$bill)
data.frame(df$bill,prop.table(df$bill))

# p306_vcd ----
table(df$age)
 
#轉成百分比
prop.table( table(df$age) )

#二維列聯表轉成百分比
table(df$age, df$group)
prop.table(table(df$age, df$group) )    #    表格內全部加總 = 1
prop.table(table(df$age, df$group) ,2)  # 參數2表示各行加總 = 1
prop.table(table(df$age, df$group) ,1)  # 參數1表示各列加總 = 1

# p309_ftable搭配vcd ----
#交叉分析表 ftable
z <- data.frame(Main= c('豚','牛','牛','牛','豚','牛','豚'), 
                sub=c('有','沒有','沒有','有','有','有','沒有'), 
                drink=c('tea','coffee','coffee','tea','coffee','tea','coffee'))
z
ftable(z, row.vars = 1:2, col.vars = "drink")
prop.table( ftable(z, row.vars = 1:2, col.vars = "drink") )
