# library for matrix manipulation
library(Matrix)
# lasso granger library
library(glmnet)
# matlab R library to use  matlab functions
library(matlab)
library(R.matlab)
# utility function for import from csv file
ImportCsv <- function(filename){
  read.csv(filename, sep = ",", header = TRUE)
}
# utility function to write to a csv file
WriteCsv <- function(object, filename) {
  write.table(object, filename, sep = ",", row.names = FALSE)
}

LassoGranger <- function (lag, num, dates, data) {
  # Performs one step of lasso granger, which is to predict one stock based on
  # its own previous data and the previous data of all other stocks. The function
  # will return the lasso coefficients.
  #
  # Args:
  #   lag: number of previous dates as input to run the regression
  #   num: number of stocks
  #   dates: number of total dates in the data
  #   data: all the data in the file on which we will perform lasso granger
  #
  # Returns:
  #   cause: the granger causality coefficicents of one stock against all stocks
  # Dependency: The glmnet package to perform lasso.
  
  # am is the input matrix 
  am <- zeros(dates - lag, lag * num)
  # bm are the response variable
  bm <- zeros(dates - lag, 1)
  
  # If lag is 5, for one particular stock, its stock data on day 6 will be 
  # regressed based on the stock data from day 1 to day 5 of all the stocks, and 
  # its stock data on day 7 will be regressed based on the stock data from day 2 
  # to day 6 of all the stocks, and so on.
  for (i in (lag + 1):dates) {
    bm[i-lag, 1] <- data[1, i]
    am[i-lag, ] <- reshape(t(as.matrix(data[, (i-lag):(i-1)])), 1, num * lag )
  }
  ########### Find best lambda ###########
  # using cv.glmnet() function to get the model with low cross-validation error
  lasso.cv <- cv.glmnet(am, bm)
  # lambda.1se is one element of the result of cv.glmnet() function which 
  # gives a smaller model with cross-validation error not siginificatly
  # different from the CV-minimizing model
  parameter <- lasso.cv$lambda.1se
  
  # fit the lasso granger model
  fit <- glmnet(am, bm, family = 'gaussian', alpha = 1, lambda = parameter)
  # get the lasso coefficients 
  vals2 <- fit$beta
  
  ##############reformat the coefficients matrix and derive the result###################
  # reformat the result into a matrix with rows representing each stock and with columns
  # representing each lagged day
  n1Coeff <- zeros(num, lag)
  for (i in (1 : num)) {
    n1Coeff[i, ] <- vals2[((i - 1) * lag + 1): (i * lag), ]
  }
  
  # get the final lasso coefficient by combining the coefficient of each lagged day of
  # one stock together
  th <- 0
  # adding the absolute values on each row to get the coefficient related to one stock
  sumCause <- apply(n1Coeff, 1, sum)
  result <- as.matrix(sumCause)
  cause <- (result > th) * result
  return (cause)
}

# assign values to all the parameters to perform the lasso granger function
lag <- 5
num <- nrow(my.data)
dates <- ncol(my.data) - 1
data <- my.data[,-1]
stock <- LassoGranger(lag, num, dates, data)
result <- data.frame(stock)
nf <- nrow(data)

# since the lasso granger function always uses the first row of the data
# file as the response variable, we have to put the targeted stock datas on 
# the first row, as a result, the coeffcient related to its own will be the first,
# so we need to move it to its corresponding position.
for (i in 2 : nf) {
  newdata <- rbind(data[i, ], data[-i, ])
  stock <- LassoGranger(lag, num, dates, newdata)
  t <- data.frame(stock)
  t1 <- data.frame(t[1, ])
  if(i != nrow(newdata)) {
    t2 <- data.frame(t[2:i+1, ])
    t3 <- data.frame(t[(i+1):nrow(newdata),])
    colnames(t2) <- c("value")
    colnames(t1) <- c("value")
    colnames(t3) <- c("value")
    res <- rbind(t2, t1)
    res <- rbind(res, t3)
    result <- cbind(result,res)
  } else {
    t2 <- data.frame(t[-1,])
    colnames(t1) <- c("value")
    colnames(t2) <- c("value")
    res <- rbind(t2,t1)
    result <- cbind(result, res)
  }  
}
colnames(result) <- my.data[,1]
final.result <- cbind(stockname = my.data[,1],result)

############################################################################################################################
#perform linear regression among variables that have non-zero correlation coefficients to validate the causal relationships
############################################################################################################################

validation <- function(correlation, my.data, lag) {
# This method is to perform linear regression on lagged time series of target variable 
# and lagged time series of all the causal variables
  # Args:
  #   lag: number of previous dates as input to run the regression
  #   correlation: correlation matrix among all variables
  #   my.data: the original data where correlation matrix derive from
  #
  # Returns:
  #   finalresult: the MSE for the last 60% data 
  # Dependency: The lm package to perform linear regression.
  colname <- correlation[, 1]
  correlation <- t(correlation[, -1])
  colnames(correlation) <- colname
  res = rep('n/a', nrow(correlation))
  for(j in 1: nrow(correlation)) {
    targetstock <- correlation[j, -j]
    targetstock <- data.frame(targetstock)
    colnames(targetstock) <- c("targetstock")
    causalstock <- rownames(subset(targetstock, as.numeric(as.character(targetstock)) > 0))
    rownames(my.data) <- my.data[,1]
    onedata <- my.data[c(causalstock), ]
    onedata <- rbind(my.data[j,], onedata)
    # prepare input matrix and response variable and establish the linear regression model
    # am is the input matrix 
    one.data <- onedata[, -1]
    column <- ncol(one.data)
    train <- one.data[, 1: (column/5*3)]
    num <- nrow(onedata)
    dates <- ncol(train) 
    am <- zeros(dates - lag, lag * num)
    # bm are the response variable
    bm <- zeros(dates - lag, 1)
    for (i in (lag + 1):dates) {
      bm[i-lag, 1] <- train[1, i]
      am[i-lag, ] <- reshape(t(as.matrix(train[, (i-lag):(i-1)])), 1, num * lag )
    }
    model <- lm(bm~am)
    colnum <- ncol(one.data)
    input <- one.data[, (colnum - dates + 1): colnum]
    newdata <- zeros(ncol(input)-lag, lag * num)
    for (i in (lag + 1) : ncol(input)) {
      newdata[i-lag, ] <- reshape(t(as.matrix(input[, (i-lag):(i-1)])), 1, num * lag )
    }
    result = predict.lm(model, data.frame(newdata))
    MSE = 0
    for (i in 1:length(result)) {
      MSE = MSE + (result[i] - one.data[1,colnum - dates + i])^2
    }
    MSE = MSE / length(result)
    res[j] = MSE
  }
  rs = data.frame(res)
  finalresult = data.frame(cbind(as.character(colname), res))
  colnames(finalresult) = c("variable", "MSE")
  return (finalresult)
  
}
