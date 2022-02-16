# ISOM674: HW1 
suppressWarnings(if(!require("ISLR")) { install.packages("ISLR"); require("ISLR") })
suppressWarnings(if(!require("rgl")) { install.packages("rgl"); require("rgl") })
suppressWarnings(if(!require("FNN")) { install.packages("FNN"); require("FNN") })
library(FNN)
library(ISLR)
library(rgl)

AllData <- read.table("C:\\Users\\BSim0\\Downloads\\GradedHW1-All-Data.csv",header=T,sep=",", stringsAsFactors = F,na.strings="")

# 1. The goal is to randomly split the entire data set into training, validation, and test samples using only single family homes and then using a 50%/25%/25% split.

AllData <- AllData[AllData$Bldg.Type=="1Fam",]

RPerm <- sample(nrow(AllData))
AllData <- AllData[RPerm,]

TrainInd <- ceiling(nrow(AllData)/2)
ValInd <- ceiling((nrow(AllData)-TrainInd)/2)+TrainInd

TrainData <- AllData[1:TrainInd,]
ValData <- AllData[(TrainInd+1):ValInd,]
TestData <- AllData[(ValInd+1):nrow(AllData),]

# 2.In any analysis that you do, you should always examine the individual variables (both the x's and the y). Examine the variables. Answer the questions on the Google form.

trainData <- read.table("C:\\Users\\BSim0\\Downloads\\GradedHW1-Train-Data.csv",header=T,sep=",", stringsAsFactors = F,na.strings="")

ValData <- read.table("C:\\Users\\BSim0\\Downloads\\GradedHW1-Validation-Data.csv",header=T,sep=",", stringsAsFactors = F,na.strings="")

TestData <- read.table("C:\\Users\\BSim0\\Downloads\\GradedHW1-Test-Data.csv",header=T,sep=",", stringsAsFactors = F,na.strings="")

trainData$Bldg.Age <- 2010 - trainData$Year.Built
ValData$Bldg.Age <- 2010 - ValData$Year.Built
TestData$Bldg.Age <- 2010 - TestData$Year.Built


trainData <- trainData[c("Lot.Area", "Total.Bsmt.SF","Gr.Liv.Area","Full.Bath", "Bedroom.AbvGr", "Bldg.Age", "SalePrice")]
ValData <- ValData[c("Lot.Area", "Total.Bsmt.SF","Gr.Liv.Area","Full.Bath", "Bedroom.AbvGr", "Bldg.Age", "SalePrice")]
TestData <- TestData[c("Lot.Area", "Total.Bsmt.SF","Gr.Liv.Area","Full.Bath", "Bedroom.AbvGr", "Bldg.Age", "SalePrice")]

#2.1
View(trainData)
which(is.na(trainData), arr.ind=TRUE)
which(is.na(ValData), arr.ind=TRUE)
which(is.na(TestData), arr.ind=TRUE)

trainData <- na.omit(trainData)
ValData <- na.omit(ValData)
TestData <- na.omit(TestData)

#2.2
hist(trainData$Lot.Area, 30)
hist(trainData$Total.Bsmt.SF, 30)
hist(trainData$Gr.Liv.Area, 30)
hist(trainData$Full.Bath, 30)
hist(trainData$Bedroom.AbvGr, 30)
hist(trainData$Bldg.Age, 30)

# 3.Without transforming or standardizing the variables, fit a k-NN regressions to the data for k=1, 2,..,40. Make of plot of the MSEcalculated from the validation data against k.Answer the questions on the Google form.Turn in the plot of the MSE's. k has instructed below.

calc_sqrt_MSE <- function(y_predicted,y_actual) {
  n <- length(y_predicted)
  MSE <- sum( (y_actual - y_predicted)^2 ) / n
  sqrt_MSE <- sqrt(MSE) 
  return(sqrt_MSE)
}

df <- data.frame(k_value=integer(),
                 MSE=double(),
                 stringsAsFactors=FALSE)

# Declare x and y variables
trainData_x <- trainData[,c(1:6)]
trainData_y <- trainData[,7]
ValData_x <- ValData[,c(1:6)]
ValData_y <- ValData[,7]
TestData_x <- TestData[,c(1:6)]
TestData_y <- TestData[,7]

for(k in seq(1,40,by=1)) {
  out <- knn.reg(train=trainData_x, test=ValData_x, y=trainData_y,k=k)
  ypred <- matrix(out$pred, nrow=out$n)
  df[nrow(df) + 1,] = c(k, calc_sqrt_MSE(ypred, ValData_y) )
}
df


#4.Determine the best k(in question 3) and then determine the MSE using the test data.

sprintf("Best MSE = %s" , min(df[2])) 
sprintf("where k is %s", df[which.min(df$MSE), 1])

#4.2
df_test <- data.frame(k_value=integer(),
                 MSE=double(),
                 stringsAsFactors=FALSE)

for(k in seq(1,40,by=1)) {
  out <- knn.reg(train=trainData_x, test=TestData_x, y=trainData_y,k=k)
  ypred <- matrix(out$pred, nrow=out$n)
  df_test[nrow(df_test) + 1,] = c(k, calc_sqrt_MSE(ypred, TestData_y) )
}
df_test

sprintf("k=12, MSE_test = %s" , df_test[12,]) 
sprintf("where k_test is %s", 12)

# 5.Repeat question 3 but standardize the variables. Do not transform the variables, just standardize them. Again, answer the questions on the Google form and turn in the plot of the MSEvs. kas instructed below.

standardize <- function(x, training) {
  standardized_df <- data.frame(x)
  for (feature in 1:length(x) ) {
    standardized_df[, feature] <- (x[ ,feature] - mean(training[ ,feature]) ) 
    standardized_df[, feature] <- standardized_df[, feature] / sd(training[ ,feature])
  }
  return(standardized_df)
}

x_trainData_std <- standardize(trainData_x, trainData_x)
x_ValData_std <- standardize(ValData_x, trainData_x)
x_TestData_std <- standardize(TestData_x, trainData_x)

df2 <- data.frame(k_value=integer(),
                 MSE=double(),
                 stringsAsFactors=FALSE)

for(k in seq(1,40,by=1)) {
  out <- knn.reg(train=x_trainData_std, test=x_ValData_std, y=trainData_y, k=k)
  ypred <- matrix(out$pred, nrow=out$n)
  df2[nrow(df2) + 1,] = c(k, calc_sqrt_MSE(ypred, ValData_y) )
}
df2

# 6.Repeat question 4 using the results from question 5 (untransformed but standardized variables).

sprintf("Best standardized MSE = %s" , min(df2[2])) 
sprintf("where k is %s", df2[which.min(df2$MSE), 1])

#6.3
df2_test <- data.frame(k_value=integer(),
                      MSE=double(),
                      stringsAsFactors=FALSE)

for(k in seq(1,40,by=1)) {
  out <- knn.reg(train=x_trainData_std, test=x_TestData_std, y=trainData_y, k=k)
  ypred <- matrix(out$pred, nrow=out$n)
  df2_test[nrow(df2_test) + 1,] = c(k, calc_sqrt_MSE(ypred, TestData_y) )
}
df2_test

sprintf("k=12, standardized MSE_test = %s" , df2_test[12,]) 
sprintf("where k_test is %s", 12)

# 7.Next transform the variables as appropriate and refit the k-NN regression without standardizing the variables. That is, repeat question 3 but using transformed variables.Turn in the plot of the MSEvs. kas instructed below.
trainData_x[trainData_x == 0] <- trainData_x[trainData_x == 0] + .0001
ValData_x[ValData_x == 0] <- ValData_x[ValData_x == 0] + .0001
TestData_x[TestData_x == 0] <- TestData_x[TestData_x == 0] + .0001

x_TrainData_trans <- trainData_x
y_TrainData_trans <- trainData_y
x_ValData_trans <- ValData_x
y_ValData_trans <- ValData_y
x_TestData_trans <- TestData_x
y_TestData_trans <- TestData_y

x_TrainData_trans$Lot.Area <- log(trainData_x$Lot.Area)
#x_TrainData_trans$Total.Bsmt.SF <- log(trainData_x$Total.Bsmt.SF)
x_TrainData_trans$Total.Bsmt.SF <- (trainData_x$Total.Bsmt.SF)^(1/2)
x_TrainData_trans$Gr.Liv.Area <- log(trainData_x$Gr.Liv.Area)
x_TrainData_trans$Full.Bath <- trainData_x$Full.Bath
x_TrainData_trans$Bedroom.AbvGr <- trainData_x$Bedroom.AbvGr
x_TrainData_trans$Bldg.Age <- (trainData_x$Bldg.Age)^(1/2)
y_TrainData_trans <- log(trainData_y)

x_ValData_trans$Lot.Area <- log(ValData_x$Lot.Area)
#x_ValData_trans$Total.Bsmt.SF <- log(ValData_x$Total.Bsmt.SF)
x_ValData_trans$Total.Bsmt.SF <- (ValData_x$Total.Bsmt.SF)^(1/2)
x_ValData_trans$Gr.Liv.Area <- log(ValData_x$Gr.Liv.Area)
x_ValData_trans$Full.Bath <- ValData_x$Full.Bath
x_ValData_trans$Bedroom.AbvGr <- ValData_x$Bedroom.AbvGr
x_ValData_trans$Bldg.Age <- (ValData_x$Bldg.Age)^(1/2)
y_ValData_trans <- log(ValData_y)

x_TestData_trans$Lot.Area <- log(TestData_x$Lot.Area)
#x_TestData_trans$Total.Bsmt.SF <- log(TestData_x$Total.Bsmt.SF)
x_TestData_trans$Total.Bsmt.SF <- (TestData_x$Total.Bsmt.SF)^(1/2)
x_TestData_trans$Gr.Liv.Area <- log(TestData_x$Gr.Liv.Area)
x_TestData_trans$Full.Bath <- TestData_x$Full.Bath
x_TestData_trans$Bedroom.AbvGr <- TestData_x$Bedroom.AbvGr
x_TestData_trans$Bldg.Age <- (TestData_x$Bldg.Age)^(1/2)
y_TestData_trans <- log(TestData_y)

#y_ValData_trans <- exp(y_ValData_trans)-1

df3 <- data.frame(k_value=integer(),
                  MSE=double(),
                  stringsAsFactors=FALSE)

for(k in seq(1,40,by=1)) {
  out <- knn.reg(train=x_TrainData_trans, test=x_ValData_trans, y=y_TrainData_trans, k=k)
  ypred <- matrix(out$pred, nrow=out$n)
  #ypred <- exp(ypred)-1
  df3[nrow(df3) + 1,] = c(k, calc_sqrt_MSE(ypred, y_ValData_trans) )
}
df3

# 8.Now determine the best kin question 7 and then determine the MSEcalculated using the test data. (You are repeating question 4 but using the transformed data.)

sprintf("Best transformed MSE = %s" , min(df3[2])) 
sprintf("where k is %s", df3[which.min(df3$MSE), 1])

# transformed test MSE
#y_TestData_trans <- exp(y_TestData_trans)-1

df3_test <- data.frame(k_value=integer(),
                  MSE=double(),
                  stringsAsFactors=FALSE)

for(k in seq(1,40,by=1)) {
  out <- knn.reg(train=x_TrainData_trans, test=x_TestData_trans, y=y_TrainData_trans, k=k)
  ypred <- matrix(out$pred, nrow=out$n)
  #ypred <- exp(ypred)-1
  df3_test[nrow(df3_test) + 1,] = c(k, calc_sqrt_MSE(ypred, y_TestData_trans) )
}
df3_test

sprintf("k=10, transformed MSE for testData = %s" , df3_test[10,]) 

# 9.Repeat question 7 using standardized transformed variables.Turn in the plot of the MSEvs. kas instructed below.

x_TrainData_both <- standardize(x_TrainData_trans, x_TrainData_trans)
x_ValData_both <- standardize(x_ValData_trans, x_TrainData_trans)
x_TestData_both <- standardize(x_TestData_trans, x_TrainData_trans)

y_TrainData_both <- y_TrainData_trans
y_ValData_both <- y_ValData_trans
y_TestData_both <- y_TestData_trans

df4 <- data.frame(k_value=integer(),
                  MSE=double(),
                  stringsAsFactors=FALSE)

for(k in seq(1,40,by=1)) {
  out <- knn.reg(train=x_TrainData_both, test=x_ValData_both, y=y_TrainData_both, k=k)
  ypred <- matrix(out$pred, nrow=out$n)
  #ypred<-ypred * sd(x_TrainData_both)
  #ypred<-ypred + mean(x_TrainData_both)
  #ypred <- exp(ypred)-1
  df4[nrow(df4) + 1,] = c(k, calc_sqrt_MSE(ypred, y_ValData_both) )
}
df4

# 10.Repeat question 8 using standardized transformed variables.

sprintf("Best both on validation MSE = %s" , min(df4[2])) 
sprintf("where k is %s", df4[which.min(df4$MSE), 1])

# both MSE on Test when k = 8
df4_test <- data.frame(k_value=integer(),
                  MSE=double(),
                  stringsAsFactors=FALSE)

for(k in seq(1,40,by=1)) {
  out <- knn.reg(train=x_TrainData_both, test=x_TestData_both, y=y_TrainData_both, k=k)
  ypred <- matrix(out$pred, nrow=out$n)
  #ypred <- exp(ypred*sd(x_TrainData_trans) + mean(x_TrainData_trans))-1
  df4_test[nrow(df4_test) + 1,] = c(k, calc_sqrt_MSE(ypred, y_TestData_both) )
}
df4_test

sprintf("k=8, both MSE for testData = %s" , df3_test[8,]) 

# 11.Which of the 4 "best" models that you determined in questions 4,6, 8, and 10 is the best overall model? Why?

plot(df$k_value, df$MSE, xlab = "k value", ylab = "sqrt(Mean Sq Error", main = "3.) sqrt(MSE) vs. k for Raw Validation")
plot(df2$k_value, df2$MSE, xlab = "k value", ylab = "sqrt(Mean Sq Error", main = "5.) sqrt(MSE) vs. k for Standardized Validation")
plot(df3$k_value, df3$MSE, xlab = "k value", ylab = "sqrt(Mean Sq Error", main = "7.) sqrt(MSE) vs. k for Transformed Validation")
plot(df4$k_value, df4$MSE, xlab = "k value", ylab = "sqrt(Mean Sq Error", main = "9.) sqrt(MSE) vs. k for Standardized + Transformed Validation")

