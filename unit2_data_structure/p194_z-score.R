# ‧(數值-平均值)/標準差
# ‧將數值標準化為平均值為0，標準差為1的數列
# ‧不受平均值波動的影響
# ‧根據常態分布原理
# –有95%的案例Z-score是介於±1.96之間
# –有99%的案例Z-score是介於±3之間
# –可以根據此原則定義極端值


# scale參數 center=T表示用平均值計算，scale=F表示不除以標準差
data <- c(1,2,3,6,3)
scale(data,center = T, scale = F)
scale(data,center = T, scale = T)

a <- scale(data,center = T, scale = T)
attributes(a)
attributes(a)$'scaled:center'
