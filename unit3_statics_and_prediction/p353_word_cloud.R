# p353_文字雲----
# install.packages("wordcloud") # 文字雲繪圖工具
d=read.table("E:/DataMining/data/WordCount.csv", header=T, sep = ",") #讀取詞頻表

# 製作文字雲 ----
# random.order = F 頻率高的字會在中間
# ordered.colors = F 字體顏色利用詞頻頻率挑選
# rainbow(150)產出150組顏色
library(wordcloud)
wordcloud(d$words, d$freq, min.freq = 300, random.order = F, ordered.colors = F, 
          colors = rainbow(150))

# 製文字雲2 ----
# https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html
require(devtools)
install_github("lchiffon/wordcloud2")
library(wordcloud2)
wordcloud2(data = demoFreq)

# Example1: use color and backgroundcolor
wordcloud2(demoFreq, color = "random-light", backgroundColor = "grey")
# Example2: use rotations
wordcloud2(demoFreq, minRotation = -pi/6, maxRotation = -pi/6, minSize = 10,
           rotateRatio = 1)
# Example3: use figure file as a mask.
# this function in this Version 1.2.1335 of RStuion still have some bug
# you can try to "refresh" Viewer or try to "show in new window"
figPath = system.file("examples/t.png",package = "wordcloud2")
wordcloud2(demoFreq, figPath = figPath, size = 1.5,color = "skyblue")
letterCloud(demoFreq, word = "R", size = 2)
