#=========================
# Question 2
#=========================

#=====================================
# function to find confidence interval
#======================================
proportion.conf.int <- function(phat,n, alpha) {
  
  ci <- phat + c(-1, 1) * qnorm(1 - (alpha/2)) * sqrt(phat*(1-phat)/n)
  return(ci)
}

#=====================================
# Function that uses monte carlo simulation to find various values that follow binomial distribution
#======================================
simulate.data <- function(sim,i,j)
{
  data<- replicate(sim,rbinom(i,1,j))
  return (data)
}





#=====================================
# Function to compute phat and confidence interval for various simulated value
#(here sim=500 so we will have 500 phat and confidence interval values one for each simulation)
#======================================
compute.phatAndCi <- function (sim,data,n)
  
{
  matrix_ci<-matrix(nrow=2,ncol=sim)
  counter<-0
  c<-1  
 phat<-numeric()
 
  
  while(c<=sim) 
  {
    index=1:n
    phat[c]<- sum(data[index,c]==1)/n
   # cat("\tphat \t",phat[c])
    if(phat[c]!=0)
    {
    ci<- proportion.conf.int(phat[c],i,alpha)
    
    matrix_ci[1,c] <- ci[1]
    matrix_ci[2,c] <-ci[2]
    }
    #if there is no success we will not  compute ci for that phat because if we do so we will get ci as 0,0 
    #and as per our condition of if statement above it will include failure events phat 
    else
    {
      matrix_ci[1,c] <- -99
      matrix_ci[2,c] <- -99
    }
  #  cat("\t ci: ",matrix_ci[1,c], "\t",matrix_ci[2,c],"\n")
    # if p lies within confidence interval we will count on those 'p's and find the probability of getting such 'p'
    if(phat[c]>=matrix_ci[1,c] && phat[c]<=matrix_ci[2,c])
      counter=counter+1
    c=c+1
  }
# cat("dimension",length(phat),dim(matrix_ci))
#  cat("counter for n",counter)
  return (counter/sim)
}

alpha=0.05
xaxis<-numeric()
yaxis<-numeric()


n<-c(5,10,30,50,100)
p<- c(0.05,0.1,0.25,0.5,0.9,0.95)

c<-1
for(i in n)
{
  
  for(j in p)
  {
    sim<- 500
    data<- simulate.data(sim,i,j)
    #print(data)
    prob<-compute.phatAndCi(sim,data,i)
    xaxis[c]<- i   # xaxis - this will store all values of n
    yaxis[c]<-prob # yaxis - this will store values of probability for corresponding n values and p values
    c=c+1
    
  }
}
plot(x=xaxis,y=yaxis,xlab = "n",ylab = "probability" )
y<- replicate(length(n),95)
x<- replicate(length(n),0)
abline(a=0.95,b=0) #normal confidence line
