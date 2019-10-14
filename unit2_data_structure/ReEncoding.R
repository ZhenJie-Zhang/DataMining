# 重新編碼 ----
# 準備資料 ----
data("cars")
str(cars)
summary(cars)

# 將速度分成3類 ----
# 第一類speed<12; 第二類12 < speed < 15; 第三類speed >= 15; 
cars$speed
x1 = cars$speed
new_cars_band = 1*(x1<12) + 2*(x1>=12&x1<15) + 3*(x1>=15)
new_cars_band
# 將數字標籤轉換成文字 ----
label = c('慢', '中', '快')
new_cars_band = label[new_cars_band]
new_cars_band

# 再將速度標籤轉碼，變成車種標籤，使用%in% ----
# '慢', '中'轉成'一般轎車'; '快'轉成'跑車'
# 使用%in%
car_categ = c('一般轎車', '跑車')
new_cars_band1 = 1*(new_cars_band %in% c('慢', '中')) + 2*(new_cars_band %in% c('快'))
new_cars_band1 = car_categ[new_cars_band1]
new_cars_band1

# 再將車種標籤轉碼，變成車種標籤，使用ifelse ----

# within就像是 SQL 語法中的 Case When ----
# 將速度分成 3 類 , 慢 speed<12 ; 中 speed <15 ; 快 speed >= 15
new_cars <- cars
new_cars <- within(new_cars,
                   {
                     speed_level <- NA
                     speed_level[cars$speed<12] <- "慢"
                     speed_level[cars$speed>=12 & cars$speed<15] <- "中"
                     speed_level[cars$speed>=15] <- "快"
                   }
                  )
head(new_cars,5)