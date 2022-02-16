# ISOM674 - HW 3
# The following R script takes emails and classifies them as either spam or
# no spam using mixture of categorical and numerical features.  The ROC curve
# is used to evaluate model performance of a Neural Network, Random Forest, and
# stepwise Logistic Regression.

###################
set.seed(20201116)
setwd("C:\\Users\\BSim0\\Downloads")
DataOrig <- read.table("spambasedata-Orig.csv",sep=",",header=T,
                       stringsAsFactors=F)

ord <- sample(nrow(DataOrig))
DataOrig <- DataOrig[ord,]

# Change IsSpam to a factor

DataOrig$IsSpam <- factor(DataOrig$IsSpam)

# Doing a 60-20-20 split
TrainInd <- ceiling(nrow(DataOrig)*0.6)
TrainDF <- DataOrig[1:TrainInd,]
tmpDF <- DataOrig[-(1:TrainInd),]
ValInd <- ceiling(nrow(tmpDF)*0.5)
ValDF <- tmpDF[1:ValInd,]
TestDF <- tmpDF[-(1:ValInd),]

remove(TrainInd,tmpDF,ValInd,ord)

# Question 1 --------------------------------------------------------------
#
# Stepwise Logistic Regression

# I am setting up the formulas for you. You should examine how the BigFM
# is created, however.

SmallFm <- IsSpam ~ 1
Vars <- names(TrainDF)
BigFm <- paste(Vars[58],"~",paste(Vars[1:57],collapse=" + "),sep=" ")
BigFm <- formula(BigFm)

# Your code to do stepwise logistic regression and compute the predicted
# probabilities for the validation and test data goes here.
DataComp <- TrainDF[complete.cases(TrainDF),]

OutBig <- glm(BigFm,data=DataComp, family=binomial(link = "logit"))

OutSmall <- glm(SmallFm,data=DataComp, family=binomial(link = "logit"))
summary(OutSmall)

#Small - validation
sc <- list(lower=SmallFm,upper=BigFm)
step_small <- step(OutSmall,scope=sc,direction="both")
summary(step_small)
AIC(step_small)

LRValP <- predict(step_small,newdata=ValDF,type="response")

source("RocPlot.r")
ROCPlot(LRValP,ValDF$IsSpam)
title("1.) Small - validation", outer = TRUE)

#Big - validation
step_big <- step(OutBig,scope=sc,direction="both")
summary(step_big)
AIC(step_big)

LRValP <- predict(step_big,newdata=ValDF,type="response")

source("RocPlot.r")
ROCPlot(LRValP,ValDF$IsSpam)
title("1.) Big - validation", outer = TRUE)

#Big - Test
LRTestP <- predict(step_big,newdata=TestDF,type="response")

source("RocPlot.r")
ROCPlot(LRTestP,TestDF$IsSpam)
title("1.) Big - Test", outer = TRUE)

# Question 2 --------------------------------------------------------------

if(!require("randomForest")) { install.packages("randomForest"); require("randomForest") }
library(randomForest)
# Your code to compute the random forest and compute the predicted
# probabilities for the validation and test data goes here.
out_forest <- randomForest(BigFm, data=TrainDF, tree=1000, type = "classification")
summary(out_forest)

myPredRFfunction <- randomForest:::predict.randomForest
RFValP <- myPredRFfunction(out_forest, newdata=ValDF, type="prob")
RFValP <- RFValP[ , 2]

ROCPlot(RFValP,ValDF$IsSpam)
title("2.) Random Forest - Validation", outer = TRUE) 

# Question 3 Wide --------------------------------------------------------------

# Write out the data for the neural net models
write.table(TrainDF,file="NNHWTrain.csv",sep=",",row.names=F,col.names=T)
write.table(ValDF,file="NNHWVal.csv",sep=",",row.names=F,col.names=T)
write.table(TestDF,file="NNHWTest.csv",sep=",",row.names=F,col.names=T)

# -------------------------------------------------------------------------

# Read in the neural net output and compute the AUC for the validation data.
SpamNNWideTrainOutput <- read.table("SpamNNWideTrainDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)
SpamNNWideValOutput <- read.table("SpamNNWideValDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)
SpamNNWideTestOutput <- read.table("SpamNNWideTestDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)

names(SpamNNWideValOutput)
ROCPlot(SpamNNWideValOutput$ValP,SpamNNWideValOutput$IsSpam)
title("3.) Wide NN - Validation", outer = TRUE) 
# Question 4 Deep --------------------------------------------------------------
# 
# write.table(TrainDF,file="HWTrain.csv",sep=",",row.names=F,col.names=T)
# write.table(ValDF,file="HWVal.csv",sep=",",row.names=F,col.names=T)
# write.table(TestDF,file="HWTest.csv",sep=",",row.names=F,col.names=T)
# -------------------------------------------------------------------------

# Read in the neural net output and compute the AUC for the validation data.
SpamNNWideTrainOutput <- read.table("SpamNNWideTrainDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)
SpamNNWideValOutput <- read.table("SpamNNWideValDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)
SpamNNWideTestOutput <- read.table("SpamNNWideTestDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)

names(SpamNNWideValOutput)
ROCPlot(SpamNNWideValOutput$ValP,SpamNNWideValOutput$IsSpam)
title("4.) Deep NN - Validation", outer = TRUE) 
