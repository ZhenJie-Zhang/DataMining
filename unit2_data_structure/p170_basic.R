x=c(4,1,3,7,2,1,4,1)
names(x) = paste('V',1:length(x),sep='')
x
x[x==1] #取值
which(x==1) #取出相對位置
unique(x) #去除重複資料
length(x)
sort(x)
