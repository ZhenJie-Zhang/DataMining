data <- c(1,2,3,6,3)
scale(data,center = T, scale = F)
scale(data,center = T, scale = T)

a <- scale(data,center = T, scale = T)
attributes(a)
attributes(a)$'scaled:center'
