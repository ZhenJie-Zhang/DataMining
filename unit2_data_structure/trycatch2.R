tryCatch(
  {
    x = "1"
    z = sqrt(x)
  },
  # 遇到 warning 時的自訂處理函數
  warning = function(msg){
    message(paste("[Warning]", msg, "\n"))
    return(NULL)
  },
  # 遇到 error 時的自訂處理函數
  error = function(msg){
    message(paste("[Error]",msg, "\n"))
    return(NA)
  }
)
ifelse(exists("z"), z, "z does not exist!")
a= x
a
