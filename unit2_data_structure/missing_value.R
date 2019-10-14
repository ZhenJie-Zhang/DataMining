tmp <- c(2,3,8,NA,4,NA,9,12,NA)
is.na(tmp)
any(is.na(tmp))
sum(is.na(tmp))
is.nan(0/0)
is.infinite(1/0)
summary(tmp)

install.packages("mice")
install.packages("missForest")
library(mice)
library(missForest)

data(iris)
data <- prodNA(iris, noNA = 0.05)

complete.cases(data)
summary(data)
md.pattern(data)
str(data)

# 圖形化檢視遺漏值 ----
install.packages("VIM")
library(VIM)
aggr_plot <- aggr(data, col = c('navyblue', 'red'), numbers = T, sortVars = T,
                  labels=names(data),cex.axis=.7,gap=3,
                  ylab=c("Histogram of missing data", "Pattern"))

# 刪除所有遺漏值資料 ----
new_data <- data[complete.cases(data),]
summary(data)
summary(new_data)

# 用平均值取代遺漏值 ----
new_data1 <- data

# 算出各欄位的平均數            #na.rm = T表示不計算NA
new_data1.mean_col_1 <- mean(new_data1[,1], na.rm = T)
new_data1.mean_col_2 <- mean(new_data1[,2], na.rm = T)
new_data1.mean_col_3 <- mean(new_data1[,3], na.rm = T)
new_data1.mean_col_4 <- mean(new_data1[,4], na.rm = T)

# 取出各欄位中有遺漏值的"列位置"
na.row1 <- is.na(new_data1[,1])
na.row2 <- is.na(new_data1[,2])
na.row3 <- is.na(new_data1[,3])
na.row4 <- is.na(new_data1[,4])

# 用平均數取代遺漏值
new_data1[na.row1,1] <- new_data1.mean_col_1
new_data1[na.row2,2] <- new_data1.mean_col_2
new_data1[na.row3,3] <- new_data1.mean_col_3
new_data1[na.row4,4] <- new_data1.mean_col_4

summary(new_data1)

# 利用資料探勘模型來取代遺漏值 ----
mice.data <- mice(data, 
                  m = 3, 
                  maxit = 30, 
                  method = "cart", 
                  seed = 188)

new_data1 <- complete(mice.data, 1)
new_data2 <- complete(mice.data, 2)
new_data3 <- complete(mice.data, 3)

Training_data_set <- new_data2
kmeans(Training_data_set[-5], nstart = 20, centers = 5)

# 使用隨機森林，進行遺漏值預測 ----
mice.data <- mice(data, 
                  m = 1, 
                  maxit = 30, 
                  method = "rf", 
                  seed = 188)
new_data1 <- complete(mice.data, 1)

summary(data)
summary(new_data1)

kmeans(Training_data_set[-5], nstart = 20, centers = 5)

