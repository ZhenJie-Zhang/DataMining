mtcars
str(mtcars)

# 長條圖
data(mtcars)
attach(mtcars)
table(cyl) #利用汽缸數產生次數分配
T_cyl = table(cyl)
barplot(T_cyl , main="cyl 汽缸數次數分配表", 
        xlab="汽缸數", col=c("red", "blue", "green"), 
        names.arg=c("4 汽缸", "6 汽缸", "8 汽缸"), border = "cyan")
# col 指派直條圖顏色
#文字 "black"   "red"     "green3"  "blue"    "cyan"    "magenta" "yellow" "gray"
#
#數字
#col=1黑, col=2紅, col=3綠, col=4深藍, col=5天藍, col=6玫瑰紅, col=7黃
# names.arg指派X軸標籤

barplot(T_cyl , 
        main="cyl 汽缸數次數分配表", 
        xlab="汽缸數", 
        col=c("red", "blue", "green"), 
        names.arg=c("4 汽缸", "6 汽缸", "8 汽缸"), border = "cyan",
        horiz=TRUE)
# horiz=TRUE 畫橫式直條圖

library("vcd")
prop.table( table(cyl) )
T_cyl1 = prop.table( table(cyl) )
barplot(T_cyl1 , main="cyl 汽缸數次數分配表", xlab="汽缸數", 
         col=c("red", "blue", "green"), 
         names.arg=c("4 汽缸", "6 汽缸", "8 汽缸"), border = "cyan", 
         horiz=TRUE)
# 分組長條圖----
T_cyl2 = table(am,cyl) #建立 變速器與汽缸數交叉表
T_cyl2
barplot(T_cyl2 , 
        main="cyl 汽缸數次數分配表", 
        xlab="汽缸數", 
        col=c("red", "blue"), 
        names.arg=c("4 汽缸", "6 汽缸", "8 汽缸"), 
        border = "cyan",
        horiz=FALSE, 
        legend = rownames(T_cyl2), beside=TRUE)

# legend 是圖例
# beside是分組圖還是堆疊圖

# 長條圖(百分比堆疊圖)
T_cyl3 = prop.table( table(am,cyl) ,2)
par(las=1) #標籤=1，表示標籤文字為水平。 標籤=2，表示標籤文字為垂直。
barplot(T_cyl3 , main="cyl 汽缸數次數百分比堆疊圖", xlab="汽缸數", col=c("red", "blue"), 
        names.arg=c("4 汽缸", "6 汽缸", "8 汽缸"), border = "cyan",
        horiz=FALSE, legend = c('自動','手動'), beside=FALSE, cex.names=2)

#cex.names=2 表示標籤文字大小為原來的兩倍

# 長條圖(百分比堆疊圖)
barplot(T_cyl3 , main="cyl 汽缸數次數百分比堆疊圖", xlab="汽缸數", 
        col=c("red", "blue"), names.arg=c("4 汽缸", "6 汽缸", "8 汽缸"), border = "cyan",
        horiz=FALSE, legend = c('自動','手動'), beside=FALSE, cex.names=2, space=2)
#space=2 表示直條間的距離

