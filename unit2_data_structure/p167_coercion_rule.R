# ±j«×¬O character > numeric > logical
# The coercion rule goes logical -> integer -> numeric -> complex -> character
x <- rep(TRUE,5)
x
x[1] <- 2
x
x[1] <- 'a'
x
x <- c(2, "two", FALSE)
x
