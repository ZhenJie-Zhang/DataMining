# 變數名稱  含意
# Survived  生存情況:存活(1)或死亡(0)
# Pclasss   座艙等級(1, 2, 3)
# Name      乘客名字
# Sex       乘客性別
# Age       乘客年齡
# SibSp     在船兄弟姊妹數/配偶數
# Parch     在船父母數/子女數
# Ticket    船票編號
# Fare      船票價格
# Cabin     座艙號
# Embarked  登船港口

setwd("E:/DataMining/unit2_data_structure/titanic")
test <- read.csv("E:/DataMining/titanic/test.csv", header = T, stringsAsFactors = F)
test <- read.csv("test.csv", header = T)
train <- read.csv("E:/DataMining/titanic/train.csv", header = T, stringsAsFactors = F)
summary(test)
str(test)
View(test)
is.na(test)
test[test$Cabin == "",]


library(mice)
complete.cases(test)

create_report(test)
create_report(train)

# https://www.kaggle.com/mrisdal/exploring-survival-on-the-titanic/report
# https://kknews.cc/zh-tw/other/oolj96m.html
# https://zhuanlan.zhihu.com/p/29130905
# https://officeguide.cc/r-logistic-regression-titanic-survival-analysis/
# 加載相應包
library('ggplot2') # 可視化
library('ggthemes') # 可視化
library('scales') # 可視化
library('dplyr') # 數據處理
library('mice') # 缺失值填補
library('randomForest') # 建模

#初步觀察數據
# 檢查數據
str(train)
str(test)
head(train)
head(test)

#可以看到，除了Survived欄位不同外，其他欄位均相同。合併訓練集與測試集，為下一步數據清洗做準備
full <- bind_rows(train, test)
str(full)
summary(full)

# 1.數據清洗 ----
# a. 觀察姓名變量----
# 從乘客名字中提取頭銜
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
# 查看按照性別劃分的頭銜數量?
table(full$Sex, full$Title)

# 對於那些出現頻率較低的頭銜合併為一類 
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
# 對於一些稱呼進行重新指定（按含義） 如mlle, ms指小姐, mme 指女士
full$Title[full$Title =='Mlle']<- 'Miss'
full$Title[full$Title =='Ms'] <- 'Miss'
full$Title[full$Title =='Mme']<- 'Mrs'
full$Title[full$Title %in% rare_title] <- 'Rare Title'
# 重新查看替換後的情況
table(full$Sex, full$Title)

# 最後從乘客姓名中，提取姓氏
full$Surname <- sapply(full$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])
nlevels(factor(full$Surname))

# b.家庭情況是否會影響生存結果?----
# 目前為止我們已經處理完乘客姓名這一變量，並從其中提取了一些新的變量。 
# 下一步考慮衍生一些家庭相關的變量 
# 首先，生成家庭人數family size這一變量。
# 可以基於已有變量SubSp和Parch（具體含義參照上面）

# 生成家庭人數變量，包括自己在內
full$Fsize <- full$SibSp + full$Parch + 1
# 生成一個家庭變量：以姓_家庭人數 格式
full$Family <- paste(full$Surname, full$Fsize, sep='_')
# 使用 ggplot2 繪製家庭人數與生存情況之間的關係
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

# https://rstudio-pubs-static.s3.amazonaws.com/329677_8f579b9e46284caeb9d3a72b7fdb7ac3.html
library(ggplot2)
ggplot(full,aes(x=factor(Fsize),fill=factor(Survived)))+
  geom_bar()+
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))

ggplot(full,aes(x=factor(Fsize),fill=factor(Survived)))+
  geom_bar(position="fill")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_fill(vjust=0.5))

percentData <- full %>% group_by(Fsize) %>% count(Survived) %>%
  mutate(ratio=scales::percent(n/sum(n)))

ggplot(full,aes(x=factor(Fsize),fill=factor(Survived)))+
  geom_bar(position="fill")+
  geom_text(data=percentData, aes(y=n,label=ratio),
            position=position_fill(vjust=0.5))



# 通過圖形我們可以明顯發現以下特點：
# - 1. 個人上船和家庭人數>4人的家庭的存活人數小於死亡人數
# - 2. 家庭人數size在[2:4]的存活人數要高於死亡人數
# 因此我們可以將家庭人數變量進行分段合併，
# 明顯的可以分為3段：個人，小家庭，大家庭，由此生成新變量。
ftable(full[1:891,c('Survived','Pclass','Sex')],
       row.vars = c('Sex','Survived'),
       col.vars = 'Pclass')
table(full[1:891,c('Pclass','Sex','Survived')])
mosaicplot(table(full[1:891,c('Pclass','Sex','Survived')]), 
           sort=c(2, 1, 3), dir=c('v', 'h', 'h'), color=T, off=3)

# datasets::Titanic----
mosaicplot(Titanic, main = "Survival on the Titanic", color = TRUE)

# https://en.wikipedia.org/wiki/Mosaic_plot
# https://commons.wikimedia.org/wiki/File:Mosaic_titanic_independent.png
# Mosaic_plot - wiki, example table
ftable(apply(Titanic, c('Sex','Survived','Class'), sum))
ftable(apply(Titanic, c(2,4,1), sum))

# Mosaic_plot - wiki, example-----
par(mfrow=c(1,2))
# png("mosaic_titanic_independent.png", width=800, height=480)
titanic <- apply(Titanic, c(1,2,4), sum)
# titanic <- apply(Titanic, c('Class','Sex','Survived'), sum)
mosaicplot(titanic, sort=c(2, 1, 3), dir=c('v', 'h', 'h'), color=T, off=3)
independent <- outer(outer(apply(titanic, 1, sum), apply(titanic,2, sum)), apply(titanic, 3, sum))/sum(titanic)^2
dimnames(independent) <- dimnames(titanic)
mosaicplot(independent, sort=c(2, 1, 3), dir=c('v', 'h', 'h'), color=T, off=3)
# dev.off()

# 離散化
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize > 1 & full$Fsize <= 4]<- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'
# 通過馬賽克圖（mosaic plot）查看家庭規模與生存情況之間關係
mosaicplot(table(full$FsizeD,full$Survived), main='Family Size by Survival', shade=TRUE)


# 從圖上也可以顯而易見的觀察出來，個人與大家庭不利於生存下來，
# 而相對的小家庭當中生存率相對較高
# c. 試著生成更多變量
# 可以發現在乘客客艙變量passenger cabin也存在一些有價值的信息如客艙層數deck
# 可以看出這一變量有很多缺失值
full$Cabin[1:28]

# 第一個字母即為客艙層數.如:
strsplit(full$Cabin[2], NULL)[[1]]
strsplit(full$Cabin[2], NULL)[[1]][1]

# 建立一個層數變量（Deck）變化取值從 A - F:
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
summary(full$Deck)

# 這裡有很多可以進一步操作的地方，
# 如有些乘客名下包含很多間房 (e.g., row 28: "C23 C25 C27"), 
# 但是考慮到這一變量數值的稀疏性（sparseness），有1014 個缺失值。 
# 後面就不再進一步考慮。

# a. 登船港口缺失——中位數 ----
summary(full$Embarked)
full[full$Embarked=="", c(-4:-5, -18:-13)]
# 乘客 62 and 830 缺少登船港口信息。
full[c(62, 830), 'Embarked']

# 我估計對於有相同艙位等級（passenger class）和票價（Fare）的乘客
# 也許有著相同的登船港口位置embarkment 。
# 我們可以看到他們支付的票價分別為：$ 80 和 $ 80 
# 同時他們的艙位等級分別是：1 和 1 。 
# 那麼他們最有可能是在哪個港口登船的呢？
median(full[full$Embarked == "C" & full$Pclass==1,'Fare'])
median(full[full$Embarked == "Q" & full$Pclass==1,'Fare'])
median(full[full$Embarked == "S" & full$Pclass==1,'Fare'])

# 去除缺失值乘客的ID
embark_fare <- full %>% filter(PassengerId != 62 & PassengerId != 830)
# 用 ggplot2 繪製embarkment, passenger class, & median fare 三者關係圖
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80),
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

# 很明顯！
# 從港口 ('C')出發的頭等艙支付的票價的中位數為80。
# 因此我們可以放心的把處於頭等艙且票價在$80的乘客62和830 的出發港口缺失值替換為'C'。
# 因為他們票價為80且處於頭等艙,因而他們很有可能都是從港口C登船的。
full$Embarked[c(62, 830)] <- 'C'

# b. 票價缺失 ——中位數 ----
# 這裡發現1044行的乘客票價為空值
summary(full$Fare)
full[is.na(full$Fare), c(-4:-5, -18:-13)]

# 提取1044行數據
full[1044, ]

# 這是從港口Southampton ('S')出發的三等艙乘客。
# 從相同港口出發且處於相同艙位的乘客數目為 (n = 494)。
sum(full$Embarked=="S" & full$Pclass==3)
nrow(full[full$Embarked=="S" & full$Pclass==3,])

ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ],
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)), 
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) + theme_few()

# 從得到的圖形上看,將缺失值用中位數進行替換是合理的。替換數值為$8.05
# 基於出發港口和客艙等級，替換票價缺失值
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, 
                          na.rm = TRUE)

# c. 年齡缺失——預測填補 ----
# 
# 最後，正如我們之前觀察到的，在用戶年齡（Age）中有大量的缺失存在。 
# 這裡我們將基於年齡和其他變量構建一個預測模型對年齡缺失值進行預測。
# S統計缺失數量
sum(is.na(full$Age))
## [1] 263

# 通常我們會使用 rpart (recursive partitioning for regression) 包來做缺失值預測 
# 在這裡我將使用 mice 包進行處理。
# 具體理由，你可以通過閱讀關於
# 基於鏈式方程 Chained Equations的
# 多重插補法MultipleImputation（MICE）的內容MICE (PDF)。
# 在這之前我們先要對因子變量（factor variables）因子化，
# 然後再進行多重插補法。
colnames(full)
# 使因子變量因子化
factor_vars <-  c('PassengerId','Pclass','Sex','Embarked', 
                  'Title','Surname','Family','FsizeD')
full[factor_vars] <-  lapply(full[factor_vars],function(x) as.factor(x))
# 設置隨機種子
set.seed(129)
# 剔除一些沒什麼用的變量之後，再執行多重插補法，:
mice_mod <-  mice(full[, !names(full) %in% 
                         c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], 
                  method='rf')
# 保存完整輸出
mice_output <-  complete(mice_mod)

# 讓我們對比數據填補前與填補後的數據分布情況。確保數據分布沒用發生偏移
# 繪製年齡分布圖
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data',
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output',
     col='lightgreen', ylim=c(0,0.04))
# 結果看起來不錯，那麼下面可以用mice模型的結果對原年齡數據進行替換。

# MICE模型結果替換年齡變量.
full$Age <- mice_output$Age
# 檢查缺失值是否被完全替換了
sum(is.na(full$Age))
## [1] 0

# 現在，我們已經完成了對所有重要變量的缺失值的替換工作。 
# 但是這一切還沒結束，我們可以對年齡變量進一步對的劃分。

# 2.特徵工程 ----
# 現在我們知道每一位乘客的年齡，
# 那麼我們可以基於年齡生成一些變量如兒童（Child）和 母親（Mother）。
# 劃分標準：
# - 兒童 ： 年齡Age < 18
# - 母親 ： 1 女性； 2 年齡 > 18; 3 擁有超過1個子女 4 頭銜不是'Miss'。

# 首先我們來看年齡與生存情況之間的關係
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) +
  geom_histogram() +
  # 分性別來看，因為前面我們知道 性別對於生存情況有重要影響
  facet_grid(.~Sex) +
  theme_few()

# 生成兒童（child）變量, 並且基於此劃分兒童child與成人adult
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'
# 展示對應人數
table(full$Child, full$Survived)
## 
## 0 1
## Adult 484 274
## Child 65 68

# 從結果看，兒童的生存率要高於成人但是這並不意味著作為兒童就一定可以生還。
# 正如我們當年看《鐵達尼號》電影時，最後船員要求母親和兒童先上船一樣。
# 下面來生成母親這個變量。

# 生成母親變量
full$Mother <- 'NotMother'
full$Mother[  full$Sex =='female'
            & full$Parch > 0 
            & full$Age > 18 
            & full$Title != 'Miss'] <- 'Mother'
# 統計對於數量
table(full$Mother, full$Survived)
## 
## 0 1
## Mother 16 39
## Not Mother 533 303
# 對新生成的兩個變量完成因子化。
full$Child <- factor(full$Child)
full$Mother <- factor(full$Mother)

# 至此，所有我們需要的變量都已經生成，並且其中沒有缺失值。 
# 為了保險起見，我們進行二次確認。

# 這個起到什麼作用?
md.pattern(full[1:891,])
library(VIM)
aggr(full[1:891,], col = c('navyblue', 'red'), numbers=TRUE, sortVars=TRUE, 
     labels=names(full), cex.axis=.7, gap=3, 
     ylab=c("Histogram of missing data", "Pattern"))
summary(full[1:891,])
# 訓練集(Train Data)的Deck有687個缺失值(NaN)，因為Cabin內容為空值("")

# 現在我們終於完成對泰坦尼克數據集（the Titanic dataset）中所有的變量缺失值的填補，
# 並基於原有變量構建了一些新變量，希望這些可以在最終的生存情況預測時起到幫助。

# 模型設定與預測 ----
# 在完成上面的工作之後，我們進入到最後一步：預測鐵達尼號上乘客的生存狀況。 
# 在這裡我們使用隨機森林分類算法(The RandomForest Classification Algorithm) 
# 我們前期那麼多工作都是為了這一步服務的。

# a. 拆分訓練集與測試集 ----
# 我們第一步需要將數據變回原先的訓練集與測試集。
# 將數據拆分為訓練集與測試集
train <- full[1:891,]
test <- full[892:1309,]

# b. 建立模型
# 我們利用訓練集訓練建立隨機森林 randomForest 模型。
# 設置隨機種子
set.seed(754)
# 建立模型l (注意: 不是所有可用變量全部加入)
rf_model <- randomForest(factor(Survived) ~ 
                           Pclass + Sex + Age + 
                           SibSp + Sex*Parch + Fare + 
                           Embarked + Title + FsizeD + 
                           Child + Mother, 
                         data = train)
# 顯示模型誤差
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
# 黑色那條線表示：整體誤差率（the overall error rate）低於20% 
# 紅色和綠色分別表示：遇難與生還的誤差率至此相對於生還來說，我們可以更準確的預測出死亡。


# c.變量重要性 ----
# 通過計算Gini係數得到相應變量的重要性排序
# 獲取重要性係數
importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
# 基於重要性係數排列變量
rankImportance <- varImportance %>% 
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
#通過 ggplot2 繪製相關重要性變量圖
ggplot(rankImportance, aes(x = reorder(Variables, Importance),
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') +
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour= 'red') +
  labs(x = 'Variables') +
  coord_flip() +
  theme_few()

# 我們從圖上可以看出哪些變量才是對我們預測最重要的變量 
# 從圖上看頭銜和性別對於生存情況影響最大，其次是船票價格和年齡。
# 而相應的乘客艙位排第五。 
# 而最出乎我意料的是母親和孩子對於生存與否的影響最小排在11和10. 這個我小時候看鐵達尼號的印象相差甚遠。

# d.預測
# 下面到了最後一步了----預測結果！ 
# 在這裡可以把剛才建立的模型直接應用在測試集上。 
# # 基於測試集進行預測

prediction <- predict(rf_model, test)
# 將結果保存為數據框，按照Kaggle提交文檔的格式要求。[兩列：PassengerId and Survived (prediction)]
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
# 將結果寫入文件
# write.csv(solution, file = 'rf_mod_Solution1.csv', row.names = F)
# 但為了達到最佳的預測結果，我們也可以重新構建不同的模型，或者用不同的變量進行組合。

test$Survived <- as.numeric(as.character(prediction))
full <- bind_rows(test, train)

y <- train$Survived
y_hat <- as.numeric(as.character(predict(rf_model, train)))
confusionMatrix <- table(y, y_hat)
# TP <- confusionMatrix[2,2]
# TN <- confusionMatrix[1,1]
# accuracy <- (TP+TN)/sum(confusionMatrix)
accuracy <- sum(diag(confusionMatrix))/sum(confusionMatrix)
cat("Accuracy(train)=",round(accuracy*100,2),"%\n")
