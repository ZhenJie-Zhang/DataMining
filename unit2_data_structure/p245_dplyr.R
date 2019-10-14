# 安裝與載入
install.packages("dplyr")
library(dplyr)
# 基本操作-- 切割, filter() 
iris
iris_df <- tbl_df( iris ) #將資料轉成較友善的 tbl_df格式
filter(iris_df, Sepal.Width>2) #切割
# 基本操作-- 欄位挑選, select() 
select( iris_df, Sepal.Length, Species) 
# 基本操作-- 欄位挑選, select() 
head(iris,3)
select(iris_df, -starts_with("Petal")) #排除欄位中包含Petal的欄位
# 基本操作-- 排序, arrange() 
arrange(iris_df, desc(Sepal.Length))  #遞減排序
arrange(iris_df, Sepal.Length) #遞增排序
# 基本操作-- 增加一列在後面, mutate() 
iris_df_n <- mutate(iris_df,new_col = Sepal.Length*Sepal.Width)
iris_df_n
# 基本操作-- 抽樣, sample_n(), sample_frac() 
sample_n(iris_df, 30) #隨機抽30個
# 基本操作-- 抽樣, sample_n(), sample_frac() 
sample_frac(iris_df, 0.2) #隨機抽20%
# 基本操作-- 計數, count()
count(iris_df, Species) 
# left join / inner join / anti join
a <- data.frame(T_name=c('Tony','Orozco','Justin'), Age=c(25,24,26))
b <- data.frame(T_name=c('Tony','Orozco','Justin','Carol'), 
                Salary=c(20000,25000,30000,18000)    )
a_left_join_b <- left_join(a, b, by = "T_name")
b_left_join_a <- left_join(b, a, by = "T_name")
a_inner_join_b <- inner_join(a, b, by = "T_name") 
#列出 b不在a的資料
anti_join(b, a, by = "T_name") 
# 基本操作-- 去除重複值計數, n_distinct
n_distinct(iris_df$Species)
# 基本操作-- 資料列分組, group by
x <- group_by(iris_df, Species)
group_by_S <- summarise(x , 
                        count = n(), 
                        Sepal.Length_avg = mean(Sepal.Length, na.rm = TRUE),
                        Sepal.Length_max = max(Sepal.Length, na.rm = TRUE),
                        Sepal.Length_min = min(Sepal.Length, na.rm = TRUE)
                        )
# 透過 %>% 可將上一個命令列的輸出變成下一個命令列的輸入
filter(iris_df, Species != "setosa") %>%   #將   filter(...) 的篩選傳給下一列group_by(...)
group_by(Species) %>%                      #將 group_by(...) 的分組傳給下一列summarise(...)
summarise( 
    count = n(), 
    Sepal.Length_avg = mean(Sepal.Length, na.rm = TRUE),
    Sepal.Length_max = max(Sepal.Length, na.rm = TRUE),
    Sepal.Length_min = min(Sepal.Length, na.rm = TRUE)
)

