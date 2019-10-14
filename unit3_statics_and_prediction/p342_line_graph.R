# 利用線圖看趨勢
years <- sort(round(runif(10,2000,2010),0))
newbornbaby <- sort(round(runif(10,200,1000),0))
dt <- data.frame(newbornbaby,years)

par(mfrow=c(4,2)) #在一張畫布上輸出3*2個圖型
plot(newbornbaby ~ years , data=dt, type='l', col=1) #只畫線
plot(newbornbaby ~ years , data=dt, type='b', col=2) #畫線與點
plot(newbornbaby ~ years , data=dt, type='c', col=3) #把'b'的圖去點
plot(newbornbaby ~ years , data=dt, type='h', col=4) #垂直線
plot(newbornbaby ~ years , data=dt, type='s', col=5) #階梯圖
plot(newbornbaby ~ years , data=dt, type='S', col=6) #階梯圖
plot(newbornbaby ~ years , data=dt, type='o', col=2) #階梯點圖
plot(newbornbaby ~ years , data=dt, type='p', col=4) #點圖
