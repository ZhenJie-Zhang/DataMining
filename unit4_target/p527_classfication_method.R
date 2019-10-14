#Dbscan演算法請安裝 fpc 工作包
install.packages("ggplot2")
install.packages("fpc")
library(ggplot2)
library(fpc)
#產出分群資料
x1 <- seq(0,pi,length.out=500)
y1 <- sin(x1) + 0.2*rnorm(500)
x2 <- 2 + seq(0,pi,length.out=500)
y2 <- cos(x2) + 0.2*rnorm(500)
data <- data.frame(c(x1,x2),c(y1,y2))
names(data) <- c('x','y')    
qplot(data$x, data$y)

p <- ggplot(data,aes(x,y))
#建置Dbscan模型
DbscanModel <- dbscan(data,eps=0.15,MinPts=4)
p + geom_point(size=2.5, aes(colour=factor(DbscanModel $cluster)))+theme(legend.position='top') #繪圖
#建置K-means模型，看看效果
KmeansModel <- kmeans(data,centers=2,nstart=10)
p + geom_point(size=2.5,aes(colour=factor(KmeansModel $cluster)))+theme(legend.position='top') #繪圖

install.packages("arules")
library(arules)
# (1)建置超市交易資料，7筆交易資料
transaction_data <- data.frame(Bread =  c(T,T,F,T,T,F,F),
                               Milk  =  c(T,F,T,T,T,T,F),
                               Diaper = c(F,T,T,T,T,F,T), 
                               Beer =   c(F,T,T,T,F,F,T),
                               Coke =   c(F,F,T,F,T,T,T)
                               )
# (2)建置關聯規則模型
rule=apriori(transaction_data,parameter=list(supp=0.1,conf=0.8,maxlen=4))   
# (3)查看rule
inspect(rule)
summary(rule)
inspect(head(sort(rule,by="support"),20))  #依照support排序
inspect(head(sort(rule,by="confidence"),20)) #依照confidence排序
inspect(head(sort(rule,by="lift"),20))     #依照lift排序
#繪圖
install.packages("arulesViz")
library(arulesViz)

plot(rule)
plot(rule, method="graph", control=list(type="items"))

data <- read.csv("E:/DataMining/data/shopcart.csv",header = FALSE)
data2=as(data,"transactions") #轉呈交易檔
# 建置關聯規則模型
rule=apriori(data2,parameter=list(supp=0.2,conf=0.8,maxlen=4))  

inspect(rule)
plot(rule)
plot(rule, method="graph", control=list(type="items"))

