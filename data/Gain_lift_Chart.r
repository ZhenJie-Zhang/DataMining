Gain_Lift_Chart <- function (y,y_hat,y_prob) 
{
  
  #產出計算資料集 
  gain_chart_DT = cbind( y,y_hat,y_prob)
  gain_chart_DT <- as.data.frame(gain_chart_DT)
  names(gain_chart_DT) <- c("positive_flg","y_hat","yes_prob")
  
  #依照 第3個欄位 [yes_prob] 流失機率遞減排序
  gain_chart_DT = gain_chart_DT[order(gain_chart_DT[, 3], decreasing = TRUE), ]
  
  #計算累積總人數(A)
  #取index row(gain_chart_DT[, 1, drop = FALSE])
  gain_chart_DT$row_index = row(gain_chart_DT[, 1, drop = FALSE])/nrow(gain_chart_DT)
  
  #計算累積正例數
  gain_chart_DT$target_cum = cumsum(gain_chart_DT[, "positive_flg"])
  
  #計算累積正例數比例(B)
  gain_chart_DT$target_rto = gain_chart_DT$target_cum/sum(gain_chart_DT[, "positive_flg"])
  
  #計算Lift (C)
  gain_chart_DT$lift = gain_chart_DT$target_rto / gain_chart_DT$row_index
  
  return(gain_chart_DT)
} 