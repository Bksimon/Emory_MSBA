#ISOM 674 ML HW2 Selfies
# This r script reads in 52 images of faces with eyes centered each with 451 by 451 pixels.
# Utilizing eigen-value / eigen-vector decomposition we had reduced the dimensionality of the
# image data while maintaining more than 85% of the variance in the data.  With the reduced 
# dimensions, we could then reconstruct the faces using k eigenvectors.

#########################
# Read in the data
setwd("C:\\Users\\BSim0\\Downloads")
# Note: Change Filename once the data is finalized
FileName <- "SelfieImageData-Final.csv"
Labs <- scan(file=FileName,what="xx",nlines=1,sep="|")
DataAsChars <- matrix(scan(file=FileName,what="xx",sep="|",skip=1),byrow=T,ncol=length(Labs))
colnames(DataAsChars) <- Labs
dim(DataAsChars)
# size in memory in MBs
as.double(object.size(DataAsChars)/1024/1024)

ImgData <- matrix(as.integer(DataAsChars[,-1]),nrow=nrow(DataAsChars))
colnames(ImgData) <- Labs[-1]
rownames(ImgData) <- DataAsChars[,1]
# size in memory in MBs
as.double(object.size(ImgData)/1024/1024)

# Take a look
ImgData[1:8,1:8]

# Free up some memory just in case
remove(DataAsChars)

# Show each Image
for(whImg in 1:nrow(ImgData)) {
  Img <- matrix(ImgData[whImg,],byrow=T,ncol=451)
  Img <- apply(Img,2,rev)
  par(pty="s",mfrow=c(1,1))
  image(z=t(Img),col = grey.colors(255),useRaster=T)
  Sys.sleep(1)
}
#Q1
nrow(ImgData)
ncol(ImgData)

#Q2 Make a plot of the "average" face. That is, average all of the values for each pixel and plot the resulting image. 
avgFace <- list()
for(pixel in 1:ncol(ImgData)) {
  avgFace[pixel] <- mean(ImgData[,pixel])
}
avgFace <- as.numeric(avgFace)
Img <- matrix(avgFace,byrow=T,ncol=451 )
Img <- apply(Img,2,rev)
par(pty="s",mfrow=c(1,1))
image(z=t(Img),col = grey.colors(255),useRaster=T)
title("Bksimon: Average Face")

#Q5 Upload a png format image of the screen plot for the eigenvalue-eigenvector decomposition of the sample variance-covariance matrix of the pixels for these images to the link provided for submitting images with this assignment.

max(pca$sdev^2)
length(pca$sdev)
sum(pca$sdev[1:25]) / sum(pca$sdev)

par(mfrow=c(1,1))
plot(pca$sdev^2, ylab = "values", main = "Bksimon: Scree Plot")
plot((pca$sdev)^(2/3), ylab = "values", main = "Square Root Eigenvalues")


#Q6 What is the value of the largest eigenvalue of the sample variance-covariance matrix? (Hint: Double check your thinking on this - the first number you think of may not be correct.) Enter your answer as a decimal number with 2 significant digits after the decimal (i.e., xxxxxxxxxx.xx).
pca <- prcomp(ImgData)

PCompTrain20d <- ImgData%*%pca$rotation[,1:20] 
dim(PCompTrain20d)
ReconTrain20d <- PCompTrain20d%*%t(pca$rotation[,1:20])

recon20 <- matrix(ReconTrain20d[5,], byrow=T, ncol = sqrt(ncol(ReconTrain20d)))
recon20 <- apply(recon20, 2, rev)
image(z=t(recon20), col=grey.colors(255), useRaster=T)
title("Bksimon: My Face 20D")


#eigen8
PCompTrain8 <- ImgData%*%pca$rotation[,8] 
dim(PCompTrain8)
ReconTrain8 <- PCompTrain8%*%t(pca$rotation[,8])

eigen8 <- matrix(ReconTrain8[8, ], byrow=T, ncol = sqrt(ncol(ImgData)))
eigen8 <- apply(eigen8, 2, rev)
image(z=t(eigen8), col=grey.colors(255), useRaster=T)
title("Bksimon: Eigenface 8")
