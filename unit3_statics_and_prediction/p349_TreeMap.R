# https://drive.google.com/drive/folders/1867lecqhbuj0cqRLfXf4FH4Yx3fDoMa0
install.packages('treemap')
library(treemap)
y=data.frame(C_ID =c('群集1','群集2','群集3','群集4','群集5','群集6','群集7'),  
             C_RTO=c(0.22,0.3,0.16,0.04,0.19,0.03,0.06)  
             )
treemap(y,index=c('C_ID'),vSize='C_RTO',vColor='C_RTO')
x <- read.table(file.choose(),header=T, sep=",", fileEncoding='big5') #選擇TaiwanGov.csv
treemap(x,index=c('縣市'),vSize='面積',vColor='面積')
treemap(x,index=c('縣市','行政區名稱'),vSize='面積',vColor='面積')
treemap(x,index=c('縣市','行政區名稱'),vSize='人口數',vColor='人口數')
