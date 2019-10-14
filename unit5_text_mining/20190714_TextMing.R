# 通過CRAN安裝 ----
install.packages("jiebaR")

# 新建立一個分詞器 ----
library(jiebaR)
seg_worker = worker()

# 分詞 ----
segment("我是一段文本",seg_worker)
dir(show_dictpath())

# 利用R+JiebaR實作文字雲-R程式前置作業 ----
install.packages("devtools")
install.packages("tidyRSS")
install.packages("XML")
install.packages("RCurl")
install.packages("plyr")
install.packages("wordcloud")
install.packages("wordcloud2")

library(tidyRSS)
library(XML)
library(RCurl)
library(jiebaR)
library(stringr)
library(plyr)
library(wordcloud)
library(wordcloud2)

# 利用R+JiebaR實作文字雲-R處理RSS ----
library(tidyRSS)
rss_url <- 'https://udn.com/rssfeed/news/2/6638/12653?ch=news'
rss <- tidyRSS::tidyfeed(feed = rss_url)
rss$feed_title # RSS標題
rss$feed_link # RSS超連結
rss$feed_description # 說明
rss$feed_language # 語系
rss$item_title # 文章標題
rss$item_link # 文章超連結


rss <-tidyRSS::tidyfeed(feed = rss_url)
ua <-"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.132 Safari/537.36"
myHttpHeader <-c(
  "User-Agent"=ua ,
  "accept"="text/html,application /xhtml+xml,application /xml;q =0.9,image/ webp,image /apng ,*/*;q=0.8",
  "accept-Language"="zh-TW,zh;q=0.9,en-US;q=0.8,en;q=0.7",
  "accept-encoding"="gzip, deflate, br",
  "Connection"="keep-alive",
  "cache-control"="no-cache",
  "Accept-Charset"="UTF8,utf -8;q=0.7,*;0.7"
)
curl_handle <- getCurlHandle()
curlSetOpt(.opts = list(myHttpHeader), cookiejar ="cookies.txt", useragent = ua, followlocation = TRUE, curl=curl_handle, verbose = TRUE)

# 實作 ----
data <- list()
i <- 1
for(link in rss$item_link){
  print(paste(i, link, sep=","))
  html_doc <- htmlParse(getURL(link, curl = curl_handle), encoding = "UTF-8")
  article_item <- xpathSApply(html_doc, '//*[@id="story_body_content"]//p', xmlValue)
  article_item <- gsub("\\s+", "", article_item)
  article_item <- gsub("$", "", article_item)
  data[i] <- article_item
  i <- i+1
  t <- sample(2:5,1)
  Sys.sleep(t)
}
data <- unlist(data)

cutter = worker(stop_word = "E:/DataMining/stop_words.utf8", user = "E:/DataMining/user.dict.utf8")
# cutter = worker()
seg_words <- cutter <= data

library(plyr)
table_words <- count(seg_words)

library(wordcloud)
wordcloud(table_words[,1], table_words[,2], random.order = F)
table_words1 <- table_words[order(table_words$freq, decreasing = TRUE),]
library(wordcloud2)
wordcloud2(table_words1, color = "random-light", backgroundColor = "grey", shape = 'star')
