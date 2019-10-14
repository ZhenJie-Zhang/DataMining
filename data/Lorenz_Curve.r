Lorenz_Curve <- function(y_p,y_n,y_prob)
{
  #產出計算資料集 
  L_chart_DT = as.data.frame( cbind(y_p,y_n,y_prob) )
  names(L_chart_DT) <- c("positive_flg","nagative_flg","yes_prob")
  
  #依照 第3個欄位 [yes_prob] 流失機率遞減排序
  L_chart_DT = L_chart_DT[order(L_chart_DT[, 3], decreasing = TRUE), ]
  
  #計算累積正例數
  L_chart_DT$target_cum = cumsum(L_chart_DT[, "positive_flg"])
  
  #計算累積正例數比例
  L_chart_DT$target_rto = L_chart_DT$target_cum/sum(L_chart_DT[, "positive_flg"])
  
  #計算累積反例數
  L_chart_DT$nontarget_cum = cumsum(L_chart_DT[, "nagative_flg"])
  
  #計算累積反例數比例
  L_chart_DT$nontarget_rto = L_chart_DT$nontarget_cum/sum(L_chart_DT[, "nagative_flg"])
  
  return(L_chart_DT)
  
}
