# 遺漏值、空值
# ‧ 資料建立時未輸入
# –人為疏失，故意或是不小心造成資料沒有被輸入
# ‧ 設備故障
# –機器故障，導致資料無法輸入
# ‧ 因資料內容不一致而被刪除
# –當資料內容不一致時，為了避免錯誤的資料影響分析的準確性，
#  可能會將該項資料以空值取代，因此產生資料的遺缺

# 遺漏值處理
# ‧捨棄該欄位(單一欄位遺漏紀錄過多)
# 遺漏資料佔整個欄位筆數的比例過大(>50%)，便可直接刪除
# ‧捨棄有遺漏值的資料(整筆資料刪除)
# 蒐集的資料量很多，而遺缺資料只佔一小部分時，便可直接刪除；若是資料量不多
# 時，會造成大量資訊流失
# ‧平均值取代(連續變數)
# ‧眾數取代(類別變數)
# ‧將遺漏值當作新的類別變項
# ‧利用資料探勘預測模型填補預測值

# 遺漏值檢測
# ‧R的遺漏值是以ＮＡ表示
# ‧遺漏值判斷：
# ‧is.na 識別遺漏值 not available
# ‧is.nan 識別不可能得值 not a number
# ‧is.infinte 識別無窮值


tmp <- c(2,3,8,NA,4,NA,9,12,NA)
is.na(tmp)
any(is.na(tmp))
sum(is.na(tmp))
is.nan(0/0)
is.infinite(1/0)
summary(tmp)

# mice套件
# R在遺漏值處裡最常使用的就是mice套件，其全名
# 為Multivariate Imputation via Chained 
# Equations，裡面會運用資料探勘的方法，針對遺
# 漏值進行「模擬填值」的動作

install.packages("mice") #安裝 mice 套件
install.packages("missForest") #安裝產生遺漏值套件

library(mice) #載入 mice 套件
library(missForest) #載入產生遺漏值套件

data(iris)
data <- prodNA(iris, noNA = 0.05) #在Iris 資料中隨機產生5%遺漏值

# 當一筆資料是完整的，回傳TRUE；
# 當一筆資料有遺漏值，回傳FALSE
complete.cases(data)

#檢視遺漏值 ----
summary(data)
md.pattern(data)
str(data)

# 圖形化檢視遺漏值 ----
install.packages("VIM") #安裝圖形檢視遺漏值套件
library(VIM)            #載入圖形檢視遺漏值套件
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

