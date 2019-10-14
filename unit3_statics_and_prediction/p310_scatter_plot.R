#散佈圖
data(iris)
attach(iris)
plot(Petal.Length~Petal.Width, col=Species) #散佈圖指令

lmTrain <- lm(formula = Petal.Length ~ Petal.Width, data = iris) #簡單迴歸
abline(reg = lmTrain$coefficients, col = "red", lwd = 1) #函數繪製輔助線
  