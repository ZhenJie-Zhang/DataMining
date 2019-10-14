# install.packages("class")
library(class)
data(iris)
#(1)�]�w�üƺؤl
set.seed(123)
#(2)���o��Ƶ���
n <- nrow(iris)
#(3)���o�V�m�˥��ƪ�index�A70%�ؼҡA30%����
train_idx <- sample(seq_len(n), size = round(0.7 * n))
#(4)���X�V�m��ƻP���ո��
traindata <- iris[train_idx,]
testdata <- iris[ - train_idx,]
train_y <- traindata[,5]
test_y <- testdata[,5]
#(5)�]�wK�AK�q�`�i�H�]�w����Ƶ��ƪ������
k_set <- as.integer(sqrt(n)) + 1
#(6)�إ߼ҫ�
pred <- knn(train = traindata[-5], test = testdata[-5], cl = train_y, k = k_set)
#(7) �V�c�x�}�p��ǽT��
message("�ǽT�סG",sum(diag(table(test_y,pred))) / sum(table(test_y,pred)) *100,"%")

# �w�˨M����Packages ----
# �@���w�˩Ҧ�packages
packages <- c("C50","tree", "rpart","randomForest")
for (i in packages){  install.packages(i) }

#�@�����Jpackages
sapply(packages, FUN = library, character.only = TRUE)

search()

#�V�m�˥�70%, ���ռ˥�30%
install.packages("caret")
library(caret)
sample_Index <- createDataPartition(y=iris$Species,p=0.7,list=FALSE)
iris.train=iris[sample_Index,]
iris.test=iris[-sample_Index,]

#�T�{�V�m�˥��P���ռ˥������@�P
par(mfrow=c(1,2)) 
#��R��ø�ϵ������Φ� 1 X 2 �����

plot(iris.train$Species, main="Training")
plot(iris.test$Species, main="Testing")
#�ҫ��V�m
iris.C50tree=C5.0(Species~ . ,data=iris.train)
summary(iris.C50tree)
plot(iris.C50tree)

#�V�m�˥����V�c�x�}(confusion matrix)�P�w�����T�v
y = iris$Species[sample_Index]
y_hat= predict(iris.C50tree,iris.train,type='class')
table.train=table(y,y_hat)
cat("Total records(train)=",nrow(iris.train),"\n")
#�w�����T�v = �x�}�﨤�﨤�`�M / �x�}�`�M
cat("Correct Classification Ratio(train)=", 
sum(diag(table.train))/sum(table.train)*100,"%\n")
#���ռ˥����V�c�x�}(confusion matrix)�P�w�����T�v
y = iris$Species[-sample_Index]
y_hat= predict(iris.C50tree,iris.test,type='class')
table.test=table(y,y_hat)
cat("Total records(test)=",nrow(iris.test),"\n")
#�w�����T�v = �x�}�﨤�﨤�`�M / �x�}�`�M
cat("Correct Classification Ratio(test)=", 
    sum(diag(table.test))/sum(table.test)*100,"%\n")



#�u��C50���V�m�˥�70%�P���ռ˥�30%
#�ҫ��V�m
iris.RFtree = randomForest(Species ~ ., data=iris.train, importane=T, proximity =TRUE, ntree=300)
print(iris.RFtree )
#�ܼƭ��n��
(round(importance(iris.RFtree ),2))
#�V�m�˥����V�c�x�}(confusion matrix)�P�w�����T�v
table.rf=iris.RFtree$confusion
cat("CORRECTION RATIO(train)=", sum(diag(table.rf)/sum(table.rf))*100,"%\n")
#���ռ˥����V�c�x�}(confusion matrix)�P�w�����T�v
y = iris$Species[-sample_Index]
y_hat = predict(iris.RFtree ,newdata=iris.test)
table.test=table(y,y_hat)
cat("Correct Classification Ratio(test)=", sum(diag(table.test))/sum(table.test)*100,"%\n")

#���s�ҫ�
(iris.clutRF=randomForest(iris[,-5]))
#ø�� Multi-Dimension plot 
MDSplot(iris.clutRF,iris$Species)

data() #�d�ݤ��ظ�ƶ�

#���JC50 churn��ƶ�
data(churn)   #���JC50 churn��ƶ�
?(churn) #��ƻ���
names(churnTrain)
str(churnTrain)
str(churnTest)

#�ҫ��V�m
data_train = churnTrain[,-3] # �ư� "area_code"���
churn.tree=rpart(churn~.,data=data_train)
churn.tree

# ø�sCART�M����
plot(churn.tree)
text(churn.tree,cex = .8) #cex���ܦr��j�p

# �ܼƭ��n��
churn.tree$variable.importance

#�V�m�˥����V�c�x�}(confusion matrix)�P�w�����T�v
y = churnTrain$churn
y_hat=predict(churn.tree,newdata=churnTrain,type="class")
table.train=table(y,y_hat)
#�w�����T�v = �x�}�﨤�﨤�`�M / �x�}�`�M
cat("Correct Classification Ratio(train)=", sum(diag(table.train))/sum(table.train)*100,"%\n")

#���ռ˥����V�c�x�}(confusion matrix)�P�w�����T�v
y = churnTest$churn
y_hat=predict(churn.tree,newdata=churnTest,type="class")
table.test=table(y,y_hat)
#�w�����T�v = �x�}�﨤�﨤�`�M / �x�}�`�M
cat("Correct Classification Ratio(test)=", sum(diag(table.test))/sum(table.test)*100,"%\n")

#ø�s Gain Chart & Lift Chart
setwd("E:/DataMining/data")
source('Gain_lift_Chart.r', encoding = 'Big5') #�Ѯv�g��UDF
y = ifelse(churnTest$churn=='yes',1,0) #�O�o�n�⥿���ন1�A�t���ন0
y_hat=predict(churn.tree,newdata=churnTest,type="class")   
y_prob = predict(churn.tree,newdata=churnTest,type="prob") #�w���y�����v
#�I�s�Ѯv�g��UDF
DT =Gain_Lift_Chart(y,y_hat,y_prob) 
par(mfrow = c(1, 2))
# Gain Chart
plot(DT$row_index, DT$target_rto, 
     xlab = "����H�Ʋֿn�ʤ���",  
     ylab = "���ҤH�Ʋֿn�ʤ���", 
     type = "l", 
     main = "Gain Chart")
#Lift Chart
plot(DT$row_index, DT$lift, 
     xlab = "����H�Ʋֿn�ʤ���", 
     ylab = "Lift",type = "l", 
     main = "Lift Chart")

# ���ռ˥�����
y_prob = predict(churn.tree,newdata=churnTest,type="prob")[,1] #�����ҹw�����v
# ROC Curve
install.packages("ROCR")
library(ROCR)
pred <- prediction(y_prob, labels = churnTest$churn)
# tpr: True Positive Ratio ���T�w������;
# fpr: False Positive Ratio  �~�P������
perf <- performance(pred, "tpr", "fpr") 
plot(perf)
points(c(0,1),c(0,1),type="l",lty=2)  #�e��u
#AUC
perf <- performance(pred, "auc")
( AUC = perf@y.values[[1]] )
( Gini = (AUC-0.5) *2 )*100

# Iift chart
perf <- performance(pred, "lift" , "rpp") 
plot(perf)

# ���J UDF: Gain_lift_Chart
source('Lorenz_Curve.r', encoding = 'Big5')  #�Ѯv�g��UDF
# ���ռ˥�����
y_p = ifelse(churnTest$churn=='yes',1,0) 
y_n = ifelse(churnTest$churn=='yes',0,1)
y_prob = predict(churn.tree,newdata=churnTest,type="prob") #�w���y�����v
# ø�s�ϫ�
DT_l =Lorenz_Curve(y_p,y_n,y_prob) 
par(mfrow=c(1,1))
# ø�s Lorenz Curve 
plot(DT_l$target_rto,DT_l$nontarget_rto , xlab = "�ϨҤH�Ʋֿn�ʤ���", ylab = "���ҤH�Ʋֿn�ʤ���" ,type = "l", main = "Lorenz Curve ")
points(c(0,1),c(0,1),type="l",lty=2)  #�e��u

# ù�N���j�k�갵-�q�H�~�Ȥ�y���ҫ� ----
#���JC50 churn��ƶ�
data(churn)   #���JC50 churn��ƶ�

data_train = churnTrain[,-3] # �ư� "area_code"���
data_train = churnTrain[,-1] # �ư� ��state"���

data_train$churn = ifelse(data_train$churn=='yes',1,0)  # ù�N���^�k�w�]�� y=1 �ؼҲ��X�������v

#�ҫ��V�m
logitM=glm(formula=churn~., data= data_train, family=binomial(link="logit"), na.action=na.exclude)

summary(logitM)

#�V�m�˥����V�c�x�}(confusion matrix)�P�w�����T�v
install.packages("InformationValue") # for optimalCutoff
library(InformationValue)
y = data_train$churn
y_hat=predict(logitM,newdata=churnTrain,type="response")
#optimalCutoff(y, y_hat)[1] ���ѤF���̨κI��ȡA��ֿ��~�������~
table.train=table(y, y_hat > optimalCutoff(y, y_hat)[1] ) 
#�w�����T�v = �x�}�﨤�﨤�`�M / �x�}�`�M
cat("Correct Classification Ratio(train)=", sum(diag(table.train))/sum(table.train)*100,"%\n")

#���ռ˥����V�c�x�}(confusion matrix)�P�w�����T�v
y = ifelse(churnTest$churn=='yes',1,0)
y_hat=predict(logitM,newdata=churnTest,type="response")

table.test=table(y, y_hat > optimalCutoff(y, y_hat)[1] )
#�w�����T�v = �x�}�﨤�﨤�`�M / �x�}�`�M
cat("Correct Classification Ratio(test)=", sum(diag(table.test))/sum(table.test)*100,"%\n")

# ROC Curve
library(ROCR)
y_prob <- predict(logitM,newdata=churnTest,type="response")
y_prob <- exp(y_prob)/(1+exp(y_prob)) #odds����v
pred <- prediction(y_prob, labels = churnTest$churn)
# tpr: True Positive Ratio ���T�w������;
# fpr: False Positive Ration�~�P������
perf <- performance(pred, "tpr", "fpr") 
plot(perf)
points(c(0,1),c(0,1),type="l",lty=2)  #�e��u
#AUC
perf <- performance(pred, "auc")
( AUC = perf@y.values[[1]] )

#Gini
( Gini = (AUC-0.5) *2 *100 )

install.packages("neuralnet") #�h�h���g����:�˶ǻ������g����
install.packages("nnet") #��h���g����
library(nnet)
library(neuralnet)

data(iris)

# One-hot Encoding
head(class.ind(iris$Species))
data <- cbind(iris, class.ind(iris$Species))
# ���ͫؼһP���ռ˥� 7:3
n=0.3*nrow(data)
test.index=sample(1:nrow(data),n)
Train=data[-test.index,]
Test=data[test.index,]
# �ؼ�
formula.bpn <- setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
BNP = neuralnet(formula = formula.bpn,
                hidden=c(3,2), # ��h���üh�A�Ĥ@�h��3�ӯ��g���A�ĤG�h��2�ӯ��g��
                data=Train,
                learningrate = 0.01, # learning rate
                threshold = 0.01,
                stepmax = 5e5 # �̤j��ieration�� = 500000(5*10^5)
                )

# ø�s������
plot(BNP)
# �w��
cf=compute(BNP,Test[,1:4]) 
Ypred = as.data.frame( round(cf$net.result) ) #�w�����G
# �إߤ@�ӷs���A�s��Species
Ypred$Species <- ""
# ��w�����G��^Species�����A
for(i in 1:nrow(Ypred)){
  if(Ypred[i, 1]==1){ Ypred[i, "Species"] <- "setosa"}
  if(Ypred[i, 2]==1){ Ypred[i, "Species"] <- "versicolor"}
  if(Ypred[i, 3]==1){ Ypred[i, "Species"] <- "virginica"}
}
Ypred
table(Test$Species,Ypred$Species)
#  �V�c�x�} (�w���v��95.56%)
message("�ǽT�סG",sum(diag(table(Test$Species,Ypred$Species))) / 
            sum(table(Test$Species,Ypred$Species)) *100,"%")

# R������V�q�� SVM�u�@�]�Ge1071----

install.packages("e1071")
library(e1071)
data(iris)
data <- iris
# ���ͫؼһP���ռ˥�
n=0.3*nrow(data)
test.index=sample(1:nrow(data),n)
Train=data[-test.index,]
Test=data[test.index,]
# �ؼ�
svm = svm(Species ~ . ,data=Train)
summary(svm)
# ���ռ˥��w�����T�v
Ypred = predict(svm, Test)
#  �V�c�x�} (�w���v��93.33%)
message("�ǽT�סG",sum(diag(table(Test$Species,Ypred))) / sum(table(Test$Species,Ypred)) *100,"%")


# input data
setwd("c:/R_WORK")
x=read.table("NaiveBayes_MailLoan.csv", header=T, sep=",")
install.packages("klaR") # �w�ˤu�@�]
library(klaR)
#�ؼ�
fit_Bayes1=NaiveBayes(�ӿ�U��~.,x[,-1])
#�d�ݼҫ�
fit_Bayes1
#�w���ҫ�
pre_Bayes1=predict(fit_Bayes1,x[,-1])
pre_Bayes1
table(x$�ӿ�U��,pre_Bayes1$class)
#  �V�c�x�}
message("�ǽT�סG",sum(diag(table(x$�ӿ�U��,pre_Bayes1$class))) / sum(table(x$�ӿ�U��,pre_Bayes1$class)) *100,"%")

iris_new <- iris[, -5]
set.seed(123)
Cluster_km <- kmeans(iris_new, nstart=15, centers=3) # center�N�O�]�w�s��
# nstart �O�����s�H�N�� k �Ӥ����I������, �@���ĳ nstart >= 10
table(Cluster_km$cluster, iris[, 5]) #�[����s���G�P������O���t�O
plot(iris_new $Petal.Width, iris_new $Petal.Length, col=Cluster_km$cluster)

Cluster_km

WSS_ratio <- rep(NA, times = 10)
for (k in 1:length(WSS_ratio)) 
{
  Cluster_km <- kmeans(iris_new, nstart=15,centers=k)     
  WSS_ratio[k] <- Cluster_km$tot.withinss
}
#�e�~�Y��
plot(WSS_ratio, type="b", main = "�~�Y��")

setwd("E:/DataMining/data") #�]�w�u�@�ؿ�
x=read.table("�T��Z�N��ƶ�.csv", header=T, sep=",") #Ū���Z�N��ƶ�
model_data <- data.frame(x$�βv, x$�Z�O, x$���O, x$�F�v) #Ū�������ܼ�
set.seed(123) #�]�w�H���ؤl
WSS_ratio <- rep(NA, times = 10) #�]�w�դ��Z������M�ܼ�
for (k in 1:length(WSS_ratio)) #�ظm
{
  Cluster_km <- kmeans(model_data, nstart=15,centers=k)     
  WSS_ratio[k] <- Cluster_km$tot.withinss
}
#�e�~�Y��
plot(WSS_ratio, type="b", main = "�~�Y��")

Cluster_km <- kmeans(model_data[-1], nstart=15,centers=3) #�ؼҡA�s�� K = 3
final_data <- data.frame(x,cluster=as.character(Cluster_km$cluster)) #�N��l��ƶ��P�ҫ����G�i����
#�w�˸�ƲM�z�u�@�] dplyr
# install.packages("dplyr")
library(dplyr)
with_model_data <- tbl_df( final_data ) #�নdplyr�榡

result <-
  with_model_data %>% 
  dplyr::group_by(cluster) %>% 
  summarise(
    count = n(), 
    median_�βv = median(�βv, na.rm = TRUE),
    median_�Z�O = median(�Z�O, na.rm = TRUE),
    median_���O = median(���O, na.rm = TRUE),
    median_�F�v = median(�F�v, na.rm = TRUE)
  ) #���R�U�s�����ܼ�
# write.table(result , file='result.csv', col.names=T, row.names=F, sep=",", quote = F) #���G�g�X��csv��
subset(final_data,  final_data$cluster==1)[,1:5]  #�d�ݸs�@�Z�N
subset(final_data,  final_data$cluster==2)[,1:5]  #�d�ݸs�G�Z�N
subset(final_data,  final_data$cluster==3)[,1:5]  #�d�ݸs�T�Z�N
# �w�� VCD�u�@�]
# install.packages("vcd")
library(vcd)
table(final_data$cluster,final_data$�j)
100*prop.table( table(final_data$cluster,final_data$�j)  ,1)  # �Ѽ�2���ܦU��[�` = 1