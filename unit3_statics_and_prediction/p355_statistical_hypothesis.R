# p377_使用rnorm函數產生常態分佈的亂數，還可以指定平均數與標準差 ----
set.seed(123)
x<- rnorm(100, mean=20, sd=5)
hist(x)
summary(x)
hist(x, probability = TRUE, ylim = c(0,max(density(x)$y)))
lines(density(x), col="red")

#試試
x <- rnorm(10000, mean=20, sd=5)
hist(x)

# p378_使用runif函數產生均勻分佈(Uniform Distribution)的亂數， ----
# 可以指定最小值與最大值
set.seed(123)
x <- runif(60000, 1, 6)
hist(x)
summary(x)
hist(x)

# p379_常態分佈檢定 ----
# H0: 該分布為常態, H1:該分布不是常態
# shapiro.test(iris$Petal.Length)
# Shapiro-Wilk Normality Test 檢定資料是不是常態分佈，
# Royston (1995)認定p.value < 0.1 資料不是常態分佈
data(iris)
shapiro.test(iris$Petal.Length)
hist(iris$Petal.Length, prob=T)

# 以Petal.Length的平均數與標準差透過dnorm產出常態分佈機率值
curve(dnorm(x,mean(iris$Petal.Length),sd(iris$Petal.Length)), col="red",add=T)

# dnorm 常態分佈的機率密度函數
# add=T表示覆蓋原圖

# p380_H0: 該分布為常態, H1:該分布不是常態----
shapiro.test(iris$Sepal.Width)
hist(iris$Sepal.Width,prob=T)
curve(dnorm(x,mean(iris$Sepal.Width),sd(iris$Sepal.Width)), 
      col="red",add=T)


# p383_t檢定– t.test ----
install.packages('C50')
library(C50)
?(churn)
data(churn)
str(churnTrain)

# p385_單一樣本平均數檢定-- t檢定 ----
# 白天通話通數平均為160通
# H0:白天通話通數平均數 = 160通
# H1:白天通話通數平均數 != 160通
# 顯著水準設定 alpha = 0.05
# 檢定統計量(t)
library(C50)
data(churn)
t.test(churnTrain$total_day_calls, mu=160, alternative="two.sided")

# 作圖
mean(churnTrain$total_day_calls)
sd(churnTrain$total_day_calls)
x <- density(churnTrain$total_day_calls)
plot(density(churnTrain$total_day_calls), xlim=c(0, 250))
lines(density(churnTrain$total_day_calls + 60))
polyred = curve(dnorm(x, 
                      mean = mean(churnTrain$total_day_calls), 
                      sd = sd(churnTrain$total_day_calls)), 
                add = T, col="green", xlim=c(160,200))

abline(v=160, col="red")
desity.Train <- density(churnTrain$total_day_calls)
drawx <- desity.Train$x[which(desity.Train$x > 160)]
drawy <- desity.Train$y[which(desity.Train$x > 160)]
polyred <- list(drawx,drawy)
names(polyred) <- c('x','y')
polygon(c(160,polyred$x), c(0,polyred$y), col = "red")

alpha = .05
z.half.alpha = qnorm(1-alpha/2) 
sd.Train <- sd(churnTrain$total_day_calls)
mean.Train <- mean(churnTrain$total_day_calls)
t.alpha <- z.half.alpha * sd.Train + mean.Train
abline(v=t.alpha, col='skyblue')
desity.Train <- density(churnTrain$total_day_calls)
drawx <- desity.Train$x[which(desity.Train$x > t.alpha)]
drawy <- desity.Train$y[which(desity.Train$x > t.alpha)]
alphagon <- list(drawx,drawy)
names(alphagon) <- c('x','y')
polygon(c(t.alpha,alphagon$x), c(0,alphagon$y), density = 30)



# p388_兩組樣本平均數檢定-- t檢定 ----
# 想知道area_code_408與area_code_415白天通話平均通數是否顯著相同
# step_1 先進行變異數檢定
# H0: [變異數_(total_day_calls_408)]/[變異數_(total_day_calls_415)]  = 1
# H1: [變異數_(total_day_calls_408)]/[變異數_(total_day_calls_415)] != 1
# 顯著水準 alpha = 0.05
# 檢定統計量(F)
library(C50)
data(churn)
var.test(churnTrain$total_day_calls[churnTrain$area_code=='area_code_408' ], 
         churnTrain$total_day_calls[churnTrain$area_code=='area_code_415' ])

# 想知道area_code_408與area_code_415白天通話平均通數是否顯著相同
# step_2
# H0: [平均數_(total_day_calls_408)] – [平均數_(total_day_calls_415)]  = 0
# H1: [平均數_(total_day_calls_408)] – [平均數_(total_day_calls_415)] != 0
# 顯著水準 alpha = 0.05
# 檢定統計量(t)
t.test(churnTrain$total_day_calls[churnTrain$area_code=='area_code_408' ], 
       churnTrain$total_day_calls[churnTrain$area_code=='area_code_415' ], 
       mu=0, var.equal=T)

# 配對樣本t檢定 ----
# 想知道新的促銷對刷卡金額是否有影響
# H0: [平均數_(刷卡金額_促銷之前)]– [平均數_(刷卡金額_促銷之後)] = 0
# H1: [平均數_(刷卡金額_促銷之前)]– [平均數_(刷卡金額_促銷之後)]!= 0
# 顯著水準 alpha = 0.05
# 檢定統計量(t)
Crd_amt_before <- rnorm(10000,mean=4032,sd=570)
Crd_amt_after <- rnorm(10000,mean=5661,sd=690)
t.test(Crd_amt_before, Crd_amt_after, mu=0, paired=T, var.equal=F)

# 卡方獨立性檢定– chisq.test ----
# 想知道性別與客戶流失是否相關
# 兩組樣本必須是類別資料
# H0: 兩因素無關係(「性別」與「客戶流失」無關係)
# H1: 兩因素有關係(「性別」與「客戶流失」有關係)
# 顯著水準 alpha = 0.05
# 檢定統計量(chisq)
dt <- matrix(c(38,45,100,77),ncol=2)
chisq.test(dt)

# 相關係數檢定– cor.test ----
# 想知道客戶夜間通話次數與日間通話次數的相關程度
# 兩組樣本必須是數值資料
# H0: 兩因素相關係數  = 0(「客戶夜間通話次數」與「日間通話次數」無關係)
# H1: 兩因素相關係數 != 0(「客戶夜間通話次數」與「日間通話次數」有關係)
# 顯著水準 alpha = 0.05
# 檢定統計量(corr)
cor(churnTrain$total_night_calls, churnTrain$total_day_calls)
cor.test(churnTrain$total_night_calls, churnTrain$total_day_calls)

# 單因子變異數檢定– anova.test ----
# 想知道不同教育程度與每日平均上網時數是否有差異
# H0: [u_(Go to Internet_高中)]= [u_(Go to Internet_大學)]= [u_(Go to Internet_研究所)]
#     平均上網時數沒有差異 → 三組的平均上網時數都相同
# H1: 至少有兩組平均上網時數有差異
# 顯著水準 alpha = 0.05
# 檢定統計量(chisq)
df <- data.frame(
  group = c(rep(1, 20), rep(2, 20), rep(3,20) ), #1:高中, 2:大學, 3:研究所
  GoInternet = floor(runif(n = 60, min = 1, max = 10))
)
lm_df <- lm(GoInternet~group, data=df)
anova(lm_df)
library(ggplot2)
qplot(df$GoInternet, geom='density')

# 另外一個函數aov
lm_df <- lm(group~GoInternet, data=df)
anova(lm_df)
a <- aov(lm_df)
summary(a)

# 多重比較
boxplot(GoInternet~group, data=df)

# 多重比較 TukeyHSD()
fit <- aov(GoInternet~factor(group), data=df)
TukeyHSD(fit)

# 多重比較
plot(TukeyHSD(fit))
