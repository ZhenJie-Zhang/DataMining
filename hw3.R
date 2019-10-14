library(C50)
library(rpart)
library(dplyr)
library(ROCR)
library(randomForest)

data(churn)
full <- bind_rows(churnTrain, churnTest)
data <- churnTrain[,c(-1,-3,-4,-5,-20)] # ���n��1, 3, 4, 5, 20��
pca_Traindt <- princomp( data , cor=T) # cor=T ��줣�P
# summary(pca_Traindt)
# screeplot(pca_Traindt,type="lines") #ø�s�~�Y��

# plot(pca_Traindt$sdev^2, type = "b",
#      xaxt = "n",
#      xlim = c(1,10), 
#      ylim = c(median(pca_Traindt$sdev^2),max(pca_Traindt$sdev^2)),
#      xlab = "",
#      ylab = "Variances")
# axis(1, at=c(1:10), labels=names(pca_Traindt$sdev[1:10]))
# test <- data.frame(cbind(c(1:15),pca_Traindt$sdev^2))
# colnames(test) <- c("x", "y")
# test.lm <- lm( formula = y~poly(x,14) , data=test)
# lines(predict(test.lm), col = "blue")

# print(pca_Traindt$loadings, digits = 8, cutoff=0)  #cutoff=0���ܱ���0�]�n���
p <- predict(pca_Traindt) #������X�D����
data.pca <- data.frame(p[,c(1:8)]) #���X7�ӥD����
data.pca$churn <- churnTrain$churn
churn.tree <- rpart(churn~. , data=data.pca)
# churn.tree
# plot(churn.tree)
# text(churn.tree, cex=0.8)

y_prob = predict(churn.tree,
                 newdata=data.frame(predict(pca_Traindt,newdata = churnTest)),
                 type="prob")[,1] 

pred <- prediction(y_prob, labels = churnTest$churn)

# tpr: True Positive Ratio ���T�w������;
# fpr: False Positive Ration�~�P������
perf <- performance(pred, "tpr", "fpr") 
plot(perf, xlim=c(0,1), ylim=c(0,1))
points(c(0,1),c(0,1),type="l",lty=2)  #�e��u
perf <- performance(pred, "auc")
( AUC = perf@y.values[[1]] )
( Gini = (AUC-0.5) *2 )*100
AUClist <- data.frame()
AUClist[nrow(AUClist)+1,"AUC"] <- AUC


# PCA�B�z�ƭȫ��ܼơA������A�A�[�W���O���ܼơA���M����ؼ� ----
data.pca <- cbind(data.pca,churnTrain[,c(1,4,5)])
test.pca <- data.frame(predict(pca_Traindt,newdata = churnTest))
test.pca <- cbind(test.pca, churnTest[,c(1,4,5)])
churn.tree <- rpart(churn~. , data=data.pca)
y_prob = predict(churn.tree,
                 newdata=test.pca,
                 type="prob")[,1]
pred <- prediction(y_prob, labels = churnTest$churn)
perf <- performance(pred, "tpr", "fpr") 
plot(perf, add=T, col="green")
perf <- performance(pred, "auc")
( AUC = perf@y.values[[1]] )
( Gini = (AUC-0.5) *2 )*100
AUClist[nrow(AUClist)+1,"AUC"] <- AUC


# ����PCA�B�z�A�����ϥΦ^�k�M���ƫؼ� ----
data_train = churnTrain[,-3] # �ư� "area_code"���
churn.tree=rpart(churn~.,data=data_train)
y_prob = predict(churn.tree,newdata=churnTest,type="prob")[,1] 

pred <- prediction(y_prob, labels = churnTest$churn)

# tpr: True Positive Ratio ���T�w������;
# fpr: False Positive Ration�~�P������
perf <- performance(pred, "tpr", "fpr") 
plot(perf, add=T, col="blue")
# points(c(0,1),c(0,1),type="l",lty=2)  #�e��u
perf <- performance(pred, "auc")
( AUC = perf@y.values[[1]] )
( Gini = (AUC-0.5) *2 )*100
AUClist[nrow(AUClist)+1,"AUC"] <- AUC
AUClist$Gini <- (AUClist$AUC-0.5)*2


# churn.forest= randomForest(churn ~ ., data=churnTrain[,-3], importane=T, proximity =TRUE, ntree=300)
# print(churn.forest)
# y_prob = predict(churn.tree,newdata=churnTest,type="prob")[,1] 
# pred <- prediction(y_prob, labels = churnTest$churn)
# perf <- performance(pred, "tpr", "fpr") 
# plot(perf, add=T, col="red")
# perf <- performance(pred, "auc")
# ( AUC = perf@y.values[[1]] )
# ( Gini = (AUC-0.5) *2 )*100
# AUClist[nrow(AUClist)+1,"AUC"] <- AUC
# AUClist$Gini <- (AUClist$AUC-0.5)*2