data("chickwts")
str(chickwts)
table(chickwts$feed)
splt1 = split(chickwts$weight, chickwts$feed)
splt1
chickwts[,2]
chickwts[1,]
chickwts[sample(1:nrow(chickwts), 5, replace = F),]

High <- c(120, 134, 110, 158, 100, 101, 140, 105)
Weight <- sample(20:25, 8,replace = TRUE)
Gender <- c("男", "女", "男", "男", "男", "女", "女", "女")
High > 130
High[High > 130]
Gender[High >130]
report <- data.frame (High, Weight, Gender)
boy.pass <- report[Gender == "男" & High>130, ]
boy.pass

#使用rownames作為切割條件----
iris
rownames(iris)
#切割rownames是7的倍數的觀察值
iris[as.integer(rownames(iris))%% 7 == 0,]

# subset(欲切割的物件, 切割條件, 欄位挑選)
subset(iris, iris$Sepal.Length > 7)
subset(iris, iris$Sepal.Length > 7, select = -Species)
subset(iris, iris$Sepal.Length > 7, select = c(Sepal.Length, Species))
