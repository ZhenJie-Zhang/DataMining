# p327_使用電信業客戶流失資料 ----
library(C50)
data(churn)
attach(churnTrain)
par(mfrow=c(2,2)) #圖片區配置 2*2 的圖，共4個圖

#設定分組組數
hist(total_day_minutes, xlab=" 白天通話分鐘數", main="breaks =11", 
     ylab="門號數", col="red" ) # 參數breaks預設為11
hist(total_day_minutes, xlab=" 白天通話分鐘數", main="breaks =2", 
     ylab="門號數", col="red", breaks=2 ) # 參數breaks設為2
hist(total_day_minutes, xlab=" 白天通話分鐘數", main="breaks =20", 
     ylab="門號數", col="red", breaks=20 ) # 參數breaks設為20
hist(total_day_minutes, xlab=" 白天通話分鐘數", main="breaks =7", 
     ylab="門號數", col="red", breaks=7 ) # 參數breaks設為7

# p329_直方圖搭配繪製機率密度函數曲線 ----
par(mfrow=c(1,2)) 
total_day_minutes_DS <- density(total_day_minutes) #機率密度函數
hist(total_day_minutes, xlab=" 白天通話分鐘數", main="breaks =7",
     ylab="門號數", col="red", breaks=7 ) 
hist(total_day_minutes, xlab=" 白天通話分鐘數", main="breaks =7",
     ylab="門號數", col="red", breaks=7, probability = TRUE )
lines(total_day_minutes_DS)
total_day_minutes_DS

# p330_直方圖搭配繪製機率密度函數曲線(density) ----
par(mfrow=c(1,3))
plot(rnorm(1000))  #從常態分配隨機取出1000個值
plot(density(rnorm(1000))) #y=f(x)
hist(rnorm(1000), probability = TRUE )
lines(density(rnorm(1000)))

par(mfrow=c(1,3))
plot(rpois(1000,0.1)) #從卜瓦松分配隨機取出1000個值
plot(density(rpois(1000,0.1)))
hist(rpois(1000,0.1), probability = TRUE)
lines(density(rpois(1000,0.1)))
