#交叉分析表 ftable
z <- data.frame (Main= c('豚','牛','牛', '牛', '豚', '牛', '豚'), 
                 sub= c('有', '沒有', '沒有', '有', '有', '有', '沒有'), 
                 drink= c('tea','coffee','coffee','tea','coffee','tea','coffee'))
z
ftable(z, row.vars = 1:2, col.vars = "drink")
ftable(z, row.vars = "Main", col.vars = "drink")

library(vcd)
prop.table( ftable(z, row.vars = 1:2, col.vars = "drink") )
