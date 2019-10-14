# p296_課堂練習 -- 相關係數COR
#製作兒童健檢資料
High <- c( 120, 134, 110, 158, 100, 101, 140, 105)
Weight <- sample(20:28,8)
cor(High,Weight)  #相關係數
plot(High, Weight)
lmTrain <- lm(formula = Weight ~ High, data = data.frame(High, Weight))
abline(reg = lmTrain$coefficients, col = "red", lwd = 1)#函數繪製輔助線

# p297_使用空氣品質資料 airquality
# Ozone：臭氧
# Solar.R : 太陽輻射
# *Tony老師: 這個資料即可以進行臭氧預測
data("airquality")
head(airquality, 6)
cor(airquality[,1:4], use = "pairwise")#產出兩兩相關的相關係數矩陣
pairs(airquality[,1:4])#繪圖
