# p406_PCA主成分分析 ----
install.packages("C50")
library(C50)
data(churn)
data <- churnTrain[,c(-1,-3,-4,-5,-20)] # 不要第1, 3, 4, 5, 20欄
pca_Traindt <- princomp( data , cor=T) # cor=T 單位不同
summary(pca_Traindt)
screeplot(pca_Traindt,type="lines") #繪製陡坡圖
p <- predict(pca_Traindt) #直接算出主成分
head(p,5)
p[,c(1:7)] #取出7個主成分

print(pca_Traindt$loadings, digits = 8, cutoff=0)  #cutoff=0表示接近0也要顯示


# p411_實作 --藥劑量與感冒痊癒天數的簡單迴歸模型 ----
# 自行產生藥劑量與感冒痊癒天數資料
x <- c(3,3,4,3,6,8,8,9) #藥劑量
y <- c(22,25,18,20,16,9,12,5) #感冒痊癒天數

New_x <- data.frame(x=5) #預測當x=5時的痊癒天數
  
# 建立一個線性迴歸模型
Train <- data.frame(x = x, y = y)
lmTrain <- lm(formula = y ~ x, data = Train)
predicted <- predict(lmTrain , newdata = New_x)
#預測當x=5時的痊癒天數

# 模型摘要
summary(lmTrain)

# 作圖
plot(y~x , main = "依藥劑量預測痊癒天數", xlab = "藥劑量", ylab = "感冒痊癒天數", family = "STHeiti")
points(x = New_x, y = predicted, col="green", cex = 2, pch = 16)
abline(reg = lmTrain$coefficients, col = "red", lwd = 1)#函數繪製輔助線


# 多元迴歸模型實作 -- 藥劑量、睡眠時間與感冒痊癒天數 ----
# 自行產生藥劑量、平均每日睡眠時間與感冒痊癒天數資料
x1 <- c(3,3,4,3,6,8,8,9) #藥劑量
x2 <- c(3,1,6,4,9,10,8,11) #平均每日睡眠時數
y <- c(22,25,18,20,16,9,12,5) #感冒痊癒天數

#新患者資料
New_x1 <- 5 #預測當x=5時的痊癒天數
New_x2 <- 7 #每日睡眠時數
New_data <- data.frame(x1 = 5, x2=7)

# 建立一個線性迴歸模型
Train <- data.frame(x1 = x1, x2=x2, y = y)
lmTrain <- lm(formula = y ~., data = Train)

#預測新患者感冒痊癒天數
predicted <- predict(lmTrain , newdata = New_data)
predicted

# 模型摘要
summary(lmTrain )


#讀入CSV檔
babyData=read.table("E:/DataMining/data/babies.csv",header=T,sep = ",")

#排除有遺漏值的資料列
babyData=na.exclude(babyData)

#訓練樣本70%與測試樣本30%
n=0.3*nrow(babyData)
test.index=sample(1:nrow(babyData),n)
Train=babyData [-test.index,]
Test=babyData[test.index,]

#確認訓練樣本與測試樣本分不一致
par(mfrow=c(1,3)) 

#讓R的繪圖視窗切割成 1 X 2 的方塊
hist(babyData$bwt) # 比較元資料集與切割後樣本的資料分布是否相同，分布類似或相同則可代表元資料集
hist(Train$bwt)    # 訓練樣本
hist(Test$bwt)     # 與測試樣本

#建模
# install.packages("rpart")
library(rpart)
baby.tree=rpart(bwt~. ,data=Train) # 使用CART分類回歸樹演算法
baby.tree
par(mfrow=c(1,1))
plot(baby.tree)
text(baby.tree , cex=.8)

#variable importance
baby.tree$variable.importance

# 數值變數預測效果評估: MAPE(Mean Absolute Percentage Error)
# 絕對百分比誤差MAPE的公式為: 
#         各個樣本的(實際值-預測值)/實際值取絕對值後的平均
#         預測效果為 GOOD:  MAPE <10%
#         預測效果為 OK: 10% <= MAPE <20%
#         預測效果為 BAD: MAPE >=20%
# MAPE of train and test group
y=babyData$bwt[-test.index]
y_hat=predict(baby.tree,newdata=Train, type="vector")
train.MAPE=mean(abs(y-y_hat)/y)
cat("MAPE(train)=",train.MAPE*100,"%\n")

y=babyData$bwt[test.index]
y_hat=predict(baby.tree,newdata=Test, type="vector")
test.MAPE=mean(abs(y-y_hat)/y)
cat("MAPE(test)=",test.MAPE*100,"%\n")

#讀入CSV檔
babyData=read.table("E:/DataMining/data/babies.csv",header=T,sep = ",")

#排除有遺漏值的資料列
babyData=na.exclude(babyData)

#訓練樣本70%與測試樣本30%
n=0.3*nrow(babyData)
test.index=sample(1:nrow(babyData),n)
Train=babyData [-test.index,]
Test=babyData[test.index,]

#建模
lmTrain <- lm(formula = bwt ~., data = Train)
summary(lmTrain)
# 數值變數預測效果評估: MAPE(Mean Absolute Percentage Error)
y=babyData$bwt[test.index]
y_hat=predict(lmTrain,newdata=Test, type="response")
test.MAPE=mean(abs(y-y_hat)/y)
cat("MAPE(test)=",test.MAPE*100,"%\n")
#再次建模，去掉age與weight
lmTrain_1 <- lm(formula = bwt ~ gestation+parity+height+smoke, data = Train)
summary(lmTrain_1)
# 數值變數預測效果評估: MAPE(Mean Absolute Percentage Error)
y=babyData$bwt[test.index]
y_hat=predict(lmTrain_1,newdata=Test, type="response")
test.MAPE=mean(abs(y-y_hat)/y)
cat("MAPE(test)=",test.MAPE*100,"%\n")

