# 準備資料 ----
data("cars")
str(cars)
summary(cars)

new_cars <- transform(new_cars, 
                      new_var1 = new_cars$speed * new_cars$dist,
                      new_var2 = new_cars$dist * 100
                     )
head(new_cars, 10)
