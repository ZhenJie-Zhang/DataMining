# p327_�ϥιq�H�~�Ȥ�y����� ----
library(C50)
data(churn)
attach(churnTrain)
par(mfrow=c(2,2)) #�Ϥ��ϰt�m 2*2 ���ϡA�@4�ӹ�

#�]�w���ղռ�
hist(total_day_minutes, xlab=" �դѳq�ܤ�����", main="breaks =11", 
     ylab="������", col="red" ) # �Ѽ�breaks�w�]��11
hist(total_day_minutes, xlab=" �դѳq�ܤ�����", main="breaks =2", 
     ylab="������", col="red", breaks=2 ) # �Ѽ�breaks�]��2
hist(total_day_minutes, xlab=" �դѳq�ܤ�����", main="breaks =20", 
     ylab="������", col="red", breaks=20 ) # �Ѽ�breaks�]��20
hist(total_day_minutes, xlab=" �դѳq�ܤ�����", main="breaks =7", 
     ylab="������", col="red", breaks=7 ) # �Ѽ�breaks�]��7

# p329_����Ϸf�tø�s���v�K�ר�Ʀ��u ----
par(mfrow=c(1,2)) 
total_day_minutes_DS <- density(total_day_minutes) #���v�K�ר��
hist(total_day_minutes, xlab=" �դѳq�ܤ�����", main="breaks =7",
     ylab="������", col="red", breaks=7 ) 
hist(total_day_minutes, xlab=" �դѳq�ܤ�����", main="breaks =7",
     ylab="������", col="red", breaks=7, probability = TRUE )
lines(total_day_minutes_DS)
total_day_minutes_DS

# p330_����Ϸf�tø�s���v�K�ר�Ʀ��u(density) ----
par(mfrow=c(1,3))
plot(rnorm(1000))  #�q�`�A���t�H�����X1000�ӭ�
plot(density(rnorm(1000))) #y=f(x)
hist(rnorm(1000), probability = TRUE )
lines(density(rnorm(1000)))

par(mfrow=c(1,3))
plot(rpois(1000,0.1)) #�q�R�˪Q���t�H�����X1000�ӭ�
plot(density(rpois(1000,0.1)))
hist(rpois(1000,0.1), probability = TRUE)
lines(density(rpois(1000,0.1)))