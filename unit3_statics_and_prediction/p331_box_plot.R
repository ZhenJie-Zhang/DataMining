# 利用churnTrain資料集
boxplot(total_day_minutes, horizontal=TRUE, xlab="白天通話分鐘數", 
          col="pink")

# 顯示白天、晚上與半夜的通話分鐘數
dt <- data.frame(total_eve_minutes, total_night_minutes, 
                   total_day_minutes)
boxplot(dt, horizontal=TRUE, xlab="通話分鐘數", col="pink")

# 顯示白天、晚上與半夜的通話分鐘數
boxplot(dt, horizontal=FALSE, xlab="通話分鐘數", col=terrain.colors(3))
legend("topright", title="撥打分鐘數", c('eve','night','day'), 
         fill=terrain.colors(3), horiz=FALSE, ncol = 1, cex = 0.6)
# col=terrain.colors(3)選擇地球色3個
# 圖例的nocl為顯示欄位數；cex為文字的大小


# 分組的盒鬚圖，檢視在不同地區流失客戶與未流失客戶於晚上通話時間的分布
boxplot(total_eve_minutes~area_code*churn,horizontal=FALSE, 
        xlab="夜晚通話分鐘數",col=terrain.colors(3))

