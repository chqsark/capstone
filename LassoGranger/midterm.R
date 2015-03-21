library(Matrix)
library(glmnet)
library(matlab)
library(R.matlab)
# utility function for import from csv file
import.csv <- function(filename){
  read.csv(filename, sep = ",", header = TRUE)
}
write.csv <- function(ob,filename) {
  write.table(ob, filename, sep = ",", row.names = FALSE)
}
getwd()
setwd("E:\\Capstone")
my.data = import.csv("close_price_NoNA.csv")
lasso.granger <- function (p, N, T, data) {
am = zeros(T-p, p*N)
bm = zeros(T-p, 1)
i = 1
for (i in (p+1):T) {
  bm[i-p,1] = data[1,i]
  am[i-p,] = reshape(t(as.matrix(data[,(i-p):(i-1)])),1,N*p)
}
################################
#test reshape
#########################################################
#x = matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3, ncol = 3)
#x
#reshape(fliplr(x),1,9)
#########################################################
# find best lambda
#########################################################
lasso.cv = cv.glmnet(am, bm, nfold = 10)
parameter = lasso.cv$lambda.1se
plot(lasso.cv)
fit = glmnet(am,bm, family = 'gaussian',alpha = 1,lambda = parameter)
#plot(fit)
#lasso.cv$lambda.1se
th = 0
vals2 = fit$beta
#nrow(vals2)
################calculate aic and reformat the coefficients matrix###################
x = am%*%vals2
#aic is used to select lambda in chorpula.lasso
#aic = norm(am%*%vals2 - bm)^2/(T-p) + (sum(abs(as.matrix(vals2)) > th))*2/(T-p)
n1Coeff = zeros(N, p)
for (i in (1:N)) {
  n1Coeff[i, ] = vals2[((i-1)*p+1): (i*p),]
}
#n1Coeff
sumCause = apply(abs(n1Coeff), 1, sum)
result = as.matrix(sumCause)
cause = (result > th)*result
return (cause)
}
#############################
p = 5
N = nrow(my.data)
T = ncol(my.data) - 1
data = my.data[,-1]
stock = lasso.granger(p,N,T,data)
result = data.frame(stock)
nf = nrow(data)
for (i in 2:nf) {
  newdata = rbind(data[i,], data[-i,])
  stock = lasso.granger(p,N,T,newdata)
  t = data.frame(stock)
  t1 = data.frame(t[1,])
  if(i != nrow(newdata)) {
    t2 = data.frame(t[2:i+1,])
    t3 = data.frame(t[(i+1):nrow(newdata),])
    colnames(t2) = c("value")
    colnames(t1) = c("value")
    colnames(t3) = c("value")
    res = rbind(t2, t1)
    res = rbind(res, t3)
    result = cbind(result,res)
  }
  else {
    t2 = data.frame(t[-1,])
    colnames(t1) = c("value")
    colnames(t2) = c("value")
    res = rbind(t2,t1)
    result = cbind(result, res)
  }  
}
colnames(result) = my.data[,1]
final.result = cbind(stockname = my.data[,1],result)
#colnames(final.result)[1] = c("stock name")
write.csv(final.result, "result2.csv")
matr = matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), ncol = 4, nrow = 3)
matr2 = reshape(matr, 2, 6)
