# p338_簡單圓餅圖 ----
pieces <- c(8,8,2,4,2)
pie(pieces , labels = c('工作','睡覺','念書','打電動','聊天'), main="生活時間分配圖") 

# p339_簡單圓餅圖 加上百分比----
pieces <- c(8,8,2,4,2)
pct <- round(pieces/sum(pieces)*100)  # 計算各個類別百分比
lbls <- paste(c('工作','睡覺','念書','打電動','聊天'),pct,'%', sep='')
pie(pieces , labels = lbls, main="生活時間分配圖") 

# p340_簡單圓餅圖 加上百分比----
pieces <- c(8,8,2,4,2)
pct <- round(pieces/sum(pieces)*100)  # 計算各個類別百分比
lbls <- paste(c('工作','睡覺','念書','打電動','聊天'),'\n',pct,'%', sep='')
pie(pieces , labels = lbls, main="生活時間分配圖", cex=1.5, cex.main=2) 
#cex=1.5標籤字放大1.5倍；cex.main=2標題放大兩倍
