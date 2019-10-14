a <- data.frame(T_name=c('Tony','Orozco','Justin'), Age = c(25,24,26))
a
b <- data.frame(T_name=c('Tony','Orozco','Justin', 'Carol'), Salary = c(20000, 25000, 30000, 18000))
b
merge(a, b, by.x = "T_name", by.y = "T_name")
