# install.packages("class")
library(class)
data(iris)
#(1)設定亂數種子
set.seed(123)
#(2)取得資料筆數
n <- nrow(iris)
#(3)取得訓練樣本數的index，70%建模，30%驗證
train_idx <- sample(seq_len(n), size = round(0.7 * n))
#(4)產出訓練資料與測試資料
traindata <- iris[train_idx,]
testdata <- iris[ - train_idx,]
train_y <- traindata[,5]
test_y <- testdata[,5]
#(5)設定K，K通常可以設定為資料筆數的平方根
k_set <- as.integer(sqrt(n)) + 1
#(6)建立模型
pred <- knn(train = traindata[-5], test = testdata[-5], cl = train_y, k = k_set)
#(7) 混淆矩陣計算準確度
message("準確度：",sum(diag(table(test_y,pred))) / sum(table(test_y,pred)) *100,"%")

# 安裝決策樹Packages ----
# 一次安裝所有packages
packages <- c("C50","tree", "rpart","randomForest")
for (i in packages){  install.packages(i) }

#一次載入packages
sapply(packages, FUN = library, character.only = TRUE)

search()

#訓練樣本70%, 測試樣本30%
install.packages("caret")
library(caret)
sample_Index <- createDataPartition(y=iris$Species,p=0.7,list=FALSE)
iris.train=iris[sample_Index,]
iris.test=iris[-sample_Index,]

#確認訓練樣本與測試樣本分不一致
par(mfrow=c(1,2)) 
#讓R的繪圖視窗切割成 1 X 2 的方塊

plot(iris.train$Species, main="Training")
plot(iris.test$Species, main="Testing")
#模型訓練
iris.C50tree=C5.0(Species~ . ,data=iris.train)
summary(iris.C50tree)
plot(iris.C50tree)

#訓練樣本的混淆矩陣(confusion matrix)與預測正確率
y = iris$Species[sample_Index]
y_hat= predict(iris.C50tree,iris.train,type='class')
table.train=table(y,y_hat)
cat("Total records(train)=",nrow(iris.train),"\n")
#預測正確率 = 矩陣對角對角總和 / 矩陣總和
cat("Correct Classification Ratio(train)=", 
sum(diag(table.train))/sum(table.train)*100,"%\n")
#測試樣本的混淆矩陣(confusion matrix)與預測正確率
y = iris$Species[-sample_Index]
y_hat= predict(iris.C50tree,iris.test,type='class')
table.test=table(y,y_hat)
cat("Total records(test)=",nrow(iris.test),"\n")
#預測正確率 = 矩陣對角對角總和 / 矩陣總和
cat("Correct Classification Ratio(test)=", 
    sum(diag(table.test))/sum(table.test)*100,"%\n")



#沿用C50的訓練樣本70%與測試樣本30%
#模型訓練
iris.RFtree = randomForest(Species ~ ., data=iris.train, importane=T, proximity =TRUE, ntree=300)
print(iris.RFtree )
#變數重要性
(round(importance(iris.RFtree ),2))
#訓練樣本的混淆矩陣(confusion matrix)與預測正確率
table.rf=iris.RFtree$confusion
cat("CORRECTION RATIO(train)=", sum(diag(table.rf)/sum(table.rf))*100,"%\n")
#測試樣本的混淆矩陣(confusion matrix)與預測正確率
y = iris$Species[-sample_Index]
y_hat = predict(iris.RFtree ,newdata=iris.test)
table.test=table(y,y_hat)
cat("Correct Classification Ratio(test)=", sum(diag(table.test))/sum(table.test)*100,"%\n")

#分群模型
(iris.clutRF=randomForest(iris[,-5]))
#繪圖 Multi-Dimension plot 
MDSplot(iris.clutRF,iris$Species)

data() #查看內建資料集

#載入C50 churn資料集
data(churn)   #載入C50 churn資料集
?(churn) #資料說明
names(churnTrain)
str(churnTrain)
str(churnTest)

#模型訓練
data_train = churnTrain[,-3] # 排除 "area_code"欄位
churn.tree=rpart(churn~.,data=data_train)
churn.tree

# 繪製CART決策樹
plot(churn.tree)
text(churn.tree,cex = .8) #cex表示字體大小

# 變數重要性
churn.tree$variable.importance

#訓練樣本的混淆矩陣(confusion matrix)與預測正確率
y = churnTrain$churn
y_hat=predict(churn.tree,newdata=churnTrain,type="class")
table.train=table(y,y_hat)
#預測正確率 = 矩陣對角對角總和 / 矩陣總和
cat("Correct Classification Ratio(train)=", sum(diag(table.train))/sum(table.train)*100,"%\n")

#測試樣本的混淆矩陣(confusion matrix)與預測正確率
y = churnTest$churn
y_hat=predict(churn.tree,newdata=churnTest,type="class")
table.test=table(y,y_hat)
#預測正確率 = 矩陣對角對角總和 / 矩陣總和
cat("Correct Classification Ratio(test)=", sum(diag(table.test))/sum(table.test)*100,"%\n")

#繪製 Gain Chart & Lift Chart
setwd("E:/DataMining/data")
source('Gain_lift_Chart.r', encoding = 'Big5') #老師寫的UDF
y = ifelse(churnTest$churn=='yes',1,0) #記得要把正例轉成1，負例轉成0
y_hat=predict(churn.tree,newdata=churnTest,type="class")   
y_prob = predict(churn.tree,newdata=churnTest,type="prob") #預測流失機率
#呼叫老師寫的UDF
DT =Gain_Lift_Chart(y,y_hat,y_prob) 
par(mfrow = c(1, 2))
# Gain Chart
plot(DT$row_index, DT$target_rto, 
     xlab = "全體人數累積百分比",  
     ylab = "正例人數累積百分比", 
     type = "l", 
     main = "Gain Chart")
#Lift Chart
plot(DT$row_index, DT$lift, 
     xlab = "全體人數累積百分比", 
     ylab = "Lift",type = "l", 
     main = "Lift Chart")

# 測試樣本評分
y_prob = predict(churn.tree,newdata=churnTest,type="prob")[,1] #取正例預測機率
# ROC Curve
install.packages("ROCR")
library(ROCR)
pred <- prediction(y_prob, labels = churnTest$churn)
# tpr: True Positive Ratio 正確預測正例;
# fpr: False Positive Ratio  誤判為正例
perf <- performance(pred, "tpr", "fpr") 
plot(perf)
points(c(0,1),c(0,1),type="l",lty=2)  #畫虛線
#AUC
perf <- performance(pred, "auc")
( AUC = perf@y.values[[1]] )
( Gini = (AUC-0.5) *2 )*100

# Iift chart
perf <- performance(pred, "lift" , "rpp") 
plot(perf)

# 載入 UDF: Gain_lift_Chart
source('Lorenz_Curve.r', encoding = 'Big5')  #老師寫的UDF
# 測試樣本評分
y_p = ifelse(churnTest$churn=='yes',1,0) 
y_n = ifelse(churnTest$churn=='yes',0,1)
y_prob = predict(churn.tree,newdata=churnTest,type="prob") #預測流失機率
# 繪製圖型
DT_l =Lorenz_Curve(y_p,y_n,y_prob) 
par(mfrow=c(1,1))
# 繪製 Lorenz Curve 
plot(DT_l$target_rto,DT_l$nontarget_rto , xlab = "反例人數累積百分比", ylab = "正例人數累積百分比" ,type = "l", main = "Lorenz Curve ")
points(c(0,1),c(0,1),type="l",lty=2)  #畫虛線

# 羅吉斯迴歸實做-電信業客戶流失模型 ----
#載入C50 churn資料集
data(churn)   #載入C50 churn資料集

data_train = churnTrain[,-3] # 排除 "area_code"欄位
data_train = churnTrain[,-1] # 排除 “state"欄位

data_train$churn = ifelse(data_train$churn=='yes',1,0)  # 羅吉斯回歸預設對 y=1 建模產出推估機率

#模型訓練
logitM=glm(formula=churn~., data= data_train, family=binomial(link="logit"), na.action=na.exclude)

summary(logitM)

#訓練樣本的混淆矩陣(confusion matrix)與預測正確率
install.packages("InformationValue") # for optimalCutoff
library(InformationValue)
y = data_train$churn
y_hat=predict(logitM,newdata=churnTrain,type="response")
#optimalCutoff(y, y_hat)[1] 提供了找到最佳截止值，減少錯誤分類錯誤
table.train=table(y, y_hat > optimalCutoff(y, y_hat)[1] ) 
#預測正確率 = 矩陣對角對角總和 / 矩陣總和
cat("Correct Classification Ratio(train)=", sum(diag(table.train))/sum(table.train)*100,"%\n")

#測試樣本的混淆矩陣(confusion matrix)與預測正確率
y = ifelse(churnTest$churn=='yes',1,0)
y_hat=predict(logitM,newdata=churnTest,type="response")

table.test=table(y, y_hat > optimalCutoff(y, y_hat)[1] )
#預測正確率 = 矩陣對角對角總和 / 矩陣總和
cat("Correct Classification Ratio(test)=", sum(diag(table.test))/sum(table.test)*100,"%\n")

# ROC Curve
library(ROCR)
y_prob <- predict(logitM,newdata=churnTest,type="response")
y_prob <- exp(y_prob)/(1+exp(y_prob)) #odds轉機率
pred <- prediction(y_prob, labels = churnTest$churn)
# tpr: True Positive Ratio 正確預測正例;
# fpr: False Positive Ration誤判為正例
perf <- performance(pred, "tpr", "fpr") 
plot(perf)
points(c(0,1),c(0,1),type="l",lty=2)  #畫虛線
#AUC
perf <- performance(pred, "auc")
( AUC = perf@y.values[[1]] )

#Gini
( Gini = (AUC-0.5) *2 *100 )

install.packages("neuralnet") #多層神經網路:倒傳遞類神經網路
install.packages("nnet") #單層神經網路
library(nnet)
library(neuralnet)

data(iris)

# One-hot Encoding
head(class.ind(iris$Species))
data <- cbind(iris, class.ind(iris$Species))
# 產生建模與測試樣本 7:3
n=0.3*nrow(data)
test.index=sample(1:nrow(data),n)
Train=data[-test.index,]
Test=data[test.index,]
# 建模
formula.bpn <- setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
BNP = neuralnet(formula = formula.bpn,
                hidden=c(3,2), # 兩層隱藏層，第一層有3個神經元，第二層有2個神經元
                data=Train,
                learningrate = 0.01, # learning rate
                threshold = 0.01,
                stepmax = 5e5 # 最大的ieration數 = 500000(5*10^5)
                )

# 繪製網路圖
plot(BNP)
# 預測
cf=compute(BNP,Test[,1:4]) 
Ypred = as.data.frame( round(cf$net.result) ) #預測結果
# 建立一個新欄位，叫做Species
Ypred$Species <- ""
# 把預測結果轉回Species的型態
for(i in 1:nrow(Ypred)){
  if(Ypred[i, 1]==1){ Ypred[i, "Species"] <- "setosa"}
  if(Ypred[i, 2]==1){ Ypred[i, "Species"] <- "versicolor"}
  if(Ypred[i, 3]==1){ Ypred[i, "Species"] <- "virginica"}
}
Ypred
table(Test$Species,Ypred$Species)
#  混淆矩陣 (預測率有95.56%)
message("準確度：",sum(diag(table(Test$Species,Ypred$Species))) / 
            sum(table(Test$Species,Ypred$Species)) *100,"%")

# R的支持向量機 SVM工作包：e1071----

install.packages("e1071")
library(e1071)
data(iris)
data <- iris
# 產生建模與測試樣本
n=0.3*nrow(data)
test.index=sample(1:nrow(data),n)
Train=data[-test.index,]
Test=data[test.index,]
# 建模
svm = svm(Species ~ . ,data=Train)
summary(svm)
# 測試樣本預測正確率
Ypred = predict(svm, Test)
#  混淆矩陣 (預測率有93.33%)
message("準確度：",sum(diag(table(Test$Species,Ypred))) / sum(table(Test$Species,Ypred)) *100,"%")


# input data
setwd("c:/R_WORK")
x=read.table("NaiveBayes_MailLoan.csv", header=T, sep=",")
install.packages("klaR") # 安裝工作包
library(klaR)
#建模
fit_Bayes1=NaiveBayes(申辦貸款~.,x[,-1])
#查看模型
fit_Bayes1
#預測模型
pre_Bayes1=predict(fit_Bayes1,x[,-1])
pre_Bayes1
table(x$申辦貸款,pre_Bayes1$class)
#  混淆矩陣
message("準確度：",sum(diag(table(x$申辦貸款,pre_Bayes1$class))) / sum(table(x$申辦貸款,pre_Bayes1$class)) *100,"%")

iris_new <- iris[, -5]
set.seed(123)
Cluster_km <- kmeans(iris_new, nstart=15, centers=3) # center就是設定群數
# nstart 是指重新隨意放 k 個中心點的次數, 一般建議 nstart >= 10
table(Cluster_km$cluster, iris[, 5]) #觀察分群結果與實際類別的差別
plot(iris_new $Petal.Width, iris_new $Petal.Length, col=Cluster_km$cluster)

Cluster_km

WSS_ratio <- rep(NA, times = 10)
for (k in 1:length(WSS_ratio)) 
{
  Cluster_km <- kmeans(iris_new, nstart=15,centers=k)     
  WSS_ratio[k] <- Cluster_km$tot.withinss
}
#畫陡坡圖
plot(WSS_ratio, type="b", main = "陡坡圖")

setwd("E:/DataMining/data") #設定工作目錄
x=read.table("三國武將資料集.csv", header=T, sep=",") #讀取武將資料集
model_data <- data.frame(x$統率, x$武力, x$智力, x$政治) #讀取切割變數
set.seed(123) #設定隨機種子
WSS_ratio <- rep(NA, times = 10) #設定組內距離平方和變數
for (k in 1:length(WSS_ratio)) #建置
{
  Cluster_km <- kmeans(model_data, nstart=15,centers=k)     
  WSS_ratio[k] <- Cluster_km$tot.withinss
}
#畫陡坡圖
plot(WSS_ratio, type="b", main = "陡坡圖")

Cluster_km <- kmeans(model_data[-1], nstart=15,centers=3) #建模，群數 K = 3
final_data <- data.frame(x,cluster=as.character(Cluster_km$cluster)) #將原始資料集與模型結果進行比對
#安裝資料清理工作包 dplyr
# install.packages("dplyr")
library(dplyr)
with_model_data <- tbl_df( final_data ) #轉成dplyr格式

result <-
  with_model_data %>% 
  dplyr::group_by(cluster) %>% 
  summarise(
    count = n(), 
    median_統率 = median(統率, na.rm = TRUE),
    median_武力 = median(武力, na.rm = TRUE),
    median_智力 = median(智力, na.rm = TRUE),
    median_政治 = median(政治, na.rm = TRUE)
  ) #分析各群切割變數
# write.table(result , file='result.csv', col.names=T, row.names=F, sep=",", quote = F) #結果寫出成csv檔
subset(final_data,  final_data$cluster==1)[,1:5]  #查看群一武將
subset(final_data,  final_data$cluster==2)[,1:5]  #查看群二武將
subset(final_data,  final_data$cluster==3)[,1:5]  #查看群三武將
# 安裝 VCD工作包
# install.packages("vcd")
library(vcd)
table(final_data$cluster,final_data$槍)
100*prop.table( table(final_data$cluster,final_data$槍)  ,1)  # 參數2表示各行加總 = 1
