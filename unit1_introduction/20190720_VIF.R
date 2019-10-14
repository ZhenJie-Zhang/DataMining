data("iris")
iris
iris[1]
iris[,1]
iris[-5]
iris[,-5]

VIF <- function(variable,data,data_name){
  var.nums <- length(variable) # 或去變數數量
  vif.table <- matrix(,nrow=var.nums,ncol=2) # 用於儲存計算結果
  for(i in 1:var.nums){ #循環計算機各個變數的 VIF
    text=""
    for(ii in variable[-i]){
      text=paste(text,ii,sep="+")
    }
    text=paste(text, ",data=",sep="")
    text=paste(text,data_name,sep="")
    text=paste(variable[i],substr(text, 2, nchar(text)),sep="~")
    text=paste("temp.lm <- lm(",text,sep="")
    text=paste(text,")",sep="")
    eval(parse(text=text)) #執行 lm()
    temp.d <- summary(temp.lm) # 取得 r2
    vif.table[i,1] <- variable[i] # 標註變量
    vif.table[i,2] <- round(1/(1-temp.d$r.squared),2) # 計算 VIF 顯示兩位小數    
  }
  colnames(vif.table) <- c("Var.", "VIF") # 給 Header
  vif.table <- as.data.frame(vif.table) # 轉化為數據框
  return(vif.table) #返回值
}

iris_vif <- iris[-5]
VIF(names(iris_vif), iris_vif, "iris_vif")
   
