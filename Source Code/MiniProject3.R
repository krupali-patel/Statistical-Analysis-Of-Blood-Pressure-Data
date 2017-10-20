bloodpressure <- read.table(file="bp.txt",header = TRUE)


#=========================
# Question 1 a  
#=========================

par(mfrow=c(1,2)) # 2 plots in 1 row
boxplot(bloodpressure, range=0,col=c('red','royalblue2'),names=c( 'Arm Method','Finger Method'))

#=========================
# Question 1 b - Histogram
#=========================


# frequency histogram by default
par(mfrow=c(1,2))
hist(bloodpressure$armsys, xlab="blood pressure values", ylab="frequency", main="Armsys method") 
hist(bloodpressure$fingsys, xlab="blood pressure values", ylab="frequency", main=" Fingsys method") 

myhist <- hist(bloodpressure$armsys)
multiplier <- myhist$counts / myhist$density
mydensity <- density(bloodpressure$armsys)
mydensity$y <- mydensity$y * multiplier[1]

plot(myhist)
lines(mydensity)

myhist <- hist(bloodpressure$fingsys)
multiplier <- myhist$counts / myhist$density
mydensity <- density(bloodpressure$fingsys)
mydensity$y <- mydensity$y * multiplier[1]

plot(myhist)
lines(mydensity)

#=========================
# Question 1 b - QQ Plot
#=========================
par(mfrow=c(1,2))
qqnorm(bloodpressure$armsys,main = 'QQ-plot : Arm Method')
qqline(bloodpressure$armsys)
qqnorm(bloodpressure$fingsys,main = 'QQ-plot : Finger Method')
qqline(bloodpressure$fingsys)


#qqplot(bloodpressure$armsys,bloodpressure$fingsys)


#=========================
# Question 1 c
#=========================
conf.int <- function(mu, sigmax,sigmay, n,m, alpha) {
  
  ci <- mu + c(-1, 1) * qnorm(1 - (alpha/2)) * sqrt((sigmax^2/n)+(sigmay^2/m)) 
  return(ci)
}

armsys.m<- mean(bloodpressure$armsys)
armsys.sigma <- sd(bloodpressure$armsys)



fingsys.m<- mean(bloodpressure$fingsys)
fingsys.sigma <- sd(bloodpressure$fingsys)

alpha <- 0.05
diff.m <- armsys.m-fingsys.m

n<- nrow(bloodpressure)

ci <- conf.int(mu = diff.m , sigmax = armsys.sigma , sigmay = fingsys.sigma, n,n,alpha)
print("\n Assuming that both the random variables are independent")
cat("mean of arm:",armsys.m)
cat("\nmean of fing", fingsys.m)
cat("\ndiffre",diff.m)
print(ci)

#===============================

pairedconf.int <- function(mu, sigma,n,talpha_by_2) {
  
  ci <- mu + c(-1, 1) * qt(talpha_by_2, n-1) * sigma/sqrt(n)
  return(ci)
}
diff <- (bloodpressure$armsys- bloodpressure$fingsys)

alpha<-0.05
talpha_by_2<- 1-(alpha/2)
print(talpha_by_2)

n <- length(diff)
ci<- pairedconf.int(mean(diff),sd(diff),length(diff),talpha_by_2)
print(ci)




