## @knitr part1

#import necessary libraries
library(matrixStats)
library(scatterplot3d)

#load the data into R
lab <- read.csv2("data/LabMeasurements-Color-Card.csv")
Master <- read.csv2("data/MasterColorCard.csv")

#transpose the Lab columns of the master matrix to have it one vector
MasterAsOneRow <- c(t(as.matrix(Master[,9:11])))

#create a matrix by recycling the master vector to fit the sample lab matrix for further computation 
MasterSameSizeAsLab <- matrix(rep(MasterAsOneRow, 13*42), byrow = TRUE,nrow = 13*42)

#cuts off row and col column from sample lab matrix
LabWithoutFirstColumns <- lab[,-c(1:2)]

#helper array that can be used to receive the column indexes of the L values from the ΔE matrix
columnIndexesInLabMatrix<-seq(from=1, to=192,by=3)

#create a matrx that contains the differences between the sample Lab and  master Lab values
DeltaMasterLab <- MasterSameSizeAsLab - LabWithoutFirstColumns

#square the differences
DeltaMasterSquared <- DeltaMasterLab^2

#create an empty SpotErrorMatrix that later contains Lab ΔE for each color spot (one row is one colour card)
SpotErrorMatrix <- matrix(NA, nrow = 546, ncol = 64, byrow = FALSE)

#fill the empty matrix with the computed ΔE's
for (i in 1:ncol(SpotErrorMatrix)) {
  SpotErrorMatrix[,i] <- sqrt(rowSums(DeltaMasterSquared[,(i-1)*3+(1:3)]))
}
#compute the minimum ΔE and maximum ΔE per color card
deltaERangePerCard  <- matrix(NA, nrow = nrow(SpotErrorMatrix), ncol = 4, byrow = FALSE)
rownames(deltaERangePerCard) <- c(1:nrow(SpotErrorMatrix))
colnames(deltaERangePerCard) <- c("Minimum","MinIndex","Maximum","MaxIndex")

for (i in 1:nrow(SpotErrorMatrix)) {
  deltaMinPerCard <- min(SpotErrorMatrix[i,])
  deltaMaxPerCard <- max(SpotErrorMatrix[i,])
  minIndex <- match(c(deltaMinPerCard),SpotErrorMatrix[i,])
  maxIndex <- match(c(deltaMaxPerCard),SpotErrorMatrix[i,])
  deltaERangePerCard[i,"Minimum"] <- deltaMinPerCard
  deltaERangePerCard[i,"MinIndex"]<-minIndex
  deltaERangePerCard[i,"Maximum"] <- max(SpotErrorMatrix[i,])
  deltaERangePerCard[i,"MaxIndex"]<-maxIndex
}
## @knitr deltaERangePerCardAsTable
deltaERangePerCardAsTable <- as.table(deltaERangePerCard)

#get the worst and best color spot compared to the master
maxCol<- deltaERangePerCard[match(colMaxs(deltaERangePerCard),deltaERangePerCard[,3])[3],4]
maxRow<- match(colMaxs(deltaERangePerCard),deltaERangePerCard[,3])[3]
minRow<- match(colMins(deltaERangePerCard),deltaERangePerCard)[1]
minCol<- deltaERangePerCard[match(colMins(deltaERangePerCard),deltaERangePerCard)[1],2]

worstColor <- c(LabWithoutFirstColumns[maxRow,columnIndexesInLabMatrix[maxCol]],LabWithoutFirstColumns[maxRow,columnIndexesInLabMatrix[maxCol]+1],LabWithoutFirstColumns[maxRow,columnIndexesInLabMatrix[maxCol]+2])
bestColor <- c(LabWithoutFirstColumns[minRow,columnIndexesInLabMatrix[minCol]],LabWithoutFirstColumns[minRow,columnIndexesInLabMatrix[minCol]+1],LabWithoutFirstColumns[minRow,columnIndexesInLabMatrix[minCol]+2])
worstColorMaster<- c(MasterAsOneRow[columnIndexesInLabMatrix[maxCol]],MasterAsOneRow[columnIndexesInLabMatrix[maxCol]+1],MasterAsOneRow[columnIndexesInLabMatrix[maxCol]+2])
bestColorMaster<- c(MasterAsOneRow[columnIndexesInLabMatrix[minCol]],MasterAsOneRow[columnIndexesInLabMatrix[minCol]+1],MasterAsOneRow[columnIndexesInLabMatrix[minCol]+2])

#convert the Lab values to rgb
bestColorRGB <- convertColor(bestColor,from = "Lab", to="sRGB")
worstColorRGB <-convertColor(worstColor,from = "Lab", to="sRGB")
worstColorMasterRGB <-convertColor(worstColorMaster,from = "Lab", to="sRGB")
bestColorMasterRGB <-convertColor(bestColorMaster,from = "Lab", to="sRGB")


#count how many spots belong into each visibility group (perception table)
VisLevelOne <- sum(SpotErrorMatrix<=1)
VisLevelTwo <- sum((SpotErrorMatrix>1) & (SpotErrorMatrix<2))
VisLevelThree <- sum((SpotErrorMatrix>2) & (SpotErrorMatrix<10))
VisLevelFour <- sum((SpotErrorMatrix>11) & (SpotErrorMatrix<49))
VisLevelFive <- sum(SpotErrorMatrix>50)

#visibility of color changes per card
meanErrorPerTarget <- rowMeans(SpotErrorMatrix)

#visibility of color changes per sample
meanErrorPerSample <- c(1:13)

#compute the mean  ΔE per sample
for (i in 1:13) {
  counter<-i;
  for (x in 1:42){
    meanErrorPerSample[i]<-meanErrorPerSample[i]+meanErrorPerTarget[counter]
    counter<-counter+13
  }
  meanErrorPerSample[i]<- meanErrorPerSample[i]/42
}

#recompute Lab values from the ΔE matrix
#get indexes of  ΔE's for the different perception tables
LumenIndexVisLvlOne <- which(SpotErrorMatrix<=1, arr.ind=TRUE)
LumenIndexVisLvlTwo <- which(((SpotErrorMatrix>1) & (SpotErrorMatrix<2)), arr.ind=TRUE)
LumenIndexVisLvlThree <- which(((SpotErrorMatrix>2) & (SpotErrorMatrix<10)), arr.ind=TRUE)
LumenIndexVisLvlFour <- which(((SpotErrorMatrix>11) & (SpotErrorMatrix<49)), arr.ind=TRUE)

#create helper to recompute indexes of L values ( from ΔE matrix)
columnIndexesInLabMatrix<-seq(from=1, to=192,by=3)
# for each perception group create a matrix to be filled
LabVisLevelOne <- matrix(NA, nrow = nrow(LumenIndexVisLvlOne), ncol = 3, byrow = FALSE)
LabVisLevelTwo <- matrix(NA, nrow = nrow(LumenIndexVisLvlTwo), ncol = 3, byrow = FALSE)
LabVisLevelThree <- matrix(NA, nrow = nrow(LumenIndexVisLvlThree), ncol = 3, byrow = FALSE)
LabVisLevelFour <- matrix(NA, nrow = nrow(LumenIndexVisLvlFour), ncol = 3, byrow = FALSE)

#get the lab values for each element of the index list
for(i in 1:nrow(LumenIndexVisLvlOne)){
  LabVisLevelOne[i,1]<-LabWithoutFirstColumns[LumenIndexVisLvlOne[i,1],columnIndexesInLabMatrix[LumenIndexVisLvlOne[i,2]]]
  LabVisLevelOne[i,2]<-LabWithoutFirstColumns[LumenIndexVisLvlOne[i,1],columnIndexesInLabMatrix[LumenIndexVisLvlOne[i,2]]+1]
  LabVisLevelOne[i,3]<-LabWithoutFirstColumns[LumenIndexVisLvlOne[i,1],columnIndexesInLabMatrix[LumenIndexVisLvlOne[i,2]]+2]
}

for(i in 1:nrow(LumenIndexVisLvlTwo)){
  LabVisLevelTwo[i,1]<-LabWithoutFirstColumns[LumenIndexVisLvlTwo[i,1],columnIndexesInLabMatrix[LumenIndexVisLvlTwo[i,2]]]
  LabVisLevelTwo[i,2]<-LabWithoutFirstColumns[LumenIndexVisLvlTwo[i,1],columnIndexesInLabMatrix[LumenIndexVisLvlTwo[i,2]]+1]
  LabVisLevelTwo[i,3]<-LabWithoutFirstColumns[LumenIndexVisLvlTwo[i,1],columnIndexesInLabMatrix[LumenIndexVisLvlTwo[i,2]]+2]
}

for(i in 1:nrow(LumenIndexVisLvlThree)){
  LabVisLevelThree[i,1]<-LabWithoutFirstColumns[LumenIndexVisLvlThree[i,1],columnIndexesInLabMatrix[LumenIndexVisLvlThree[i,2]]]
  LabVisLevelThree[i,2]<-LabWithoutFirstColumns[LumenIndexVisLvlThree[i,1],columnIndexesInLabMatrix[LumenIndexVisLvlThree[i,2]]+1]
  LabVisLevelThree[i,3]<-LabWithoutFirstColumns[LumenIndexVisLvlThree[i,1],columnIndexesInLabMatrix[LumenIndexVisLvlThree[i,2]]+2]
}

for(i in 1:nrow(LumenIndexVisLvlFour)){
  LabVisLevelFour[i,1]<-LabWithoutFirstColumns[LumenIndexVisLvlFour[i,1],columnIndexesInLabMatrix[LumenIndexVisLvlFour[i,2]]]
  LabVisLevelFour[i,2]<-LabWithoutFirstColumns[LumenIndexVisLvlFour[i,1],columnIndexesInLabMatrix[LumenIndexVisLvlFour[i,2]]+1]
  LabVisLevelFour[i,3]<-LabWithoutFirstColumns[LumenIndexVisLvlFour[i,1],columnIndexesInLabMatrix[LumenIndexVisLvlFour[i,2]]+2]
}

#convert Lab values in the perception matrixes to rgb in a seperate matrix
RgbForVisLevelOne <- matrix(NA, nrow = nrow(LabVisLevelOne), ncol = 3, byrow = FALSE)
for(i in 1:nrow(LabVisLevelOne)){
  RgbForVisLevelOne[i,1:3]<-convertColor(LabVisLevelOne[i,1:3],from = "Lab", to="sRGB")
}

RgbForVisLevelTwo <- matrix(NA, nrow = nrow(LabVisLevelTwo), ncol = 3, byrow = FALSE)
for(i in 1:nrow(LabVisLevelTwo)){
  RgbForVisLevelTwo[i,1:3]<-convertColor(LabVisLevelTwo[i,1:3],from = "Lab", to="sRGB")
}

RgbForVisLevelThree <- matrix(NA, nrow = nrow(LabVisLevelThree), ncol = 3, byrow = FALSE)
for(i in 1:nrow(LabVisLevelThree)){
  RgbForVisLevelThree[i,1:3]<-convertColor(LabVisLevelThree[i,1:3],from = "Lab", to="sRGB")
}

RgbForVisLevelFour <- matrix(NA, nrow = nrow(LabVisLevelFour), ncol = 3, byrow = FALSE)
for(i in 1:nrow(LabVisLevelFour)){
  RgbForVisLevelFour[i,1:3]<-convertColor(LabVisLevelFour[i,1:3],from = "Lab", to="sRGB")
}

#PLOTS
#plot the groups
## @knitr barplotVisLevel
barplot(c(VisLevelOne,VisLevelTwo,VisLevelThree,VisLevelFour,VisLevelFive),
        (1:5),
        names.arg = c("1","2","3","4","5"))
#plot the groups
## @knitr histMeanErrorPerTarget
hist(meanErrorPerTarget, breaks="Sturge", col="grey", labels = T,main="Colour Card Errors")
## @knitr densityMeanErrorPerTarget
plot(density(meanErrorPerTarget))
#plot meanErrorPerSample
## @knitr histMeanErrorPerSample
hist(meanErrorPerSample, breaks="Sturge", col="grey", labels = T,main="mean ΔE values per Sample")
#plot the color with the biggest and the smallest distance to the intended color
## @knitr compareWorseBestDelta
par(mfrow=c(1,2))
plot(0,0, xlim=c(-1,1), ylim=c(-1,1), axes=FALSE, xlab="",ylab="", main = "Color with min Δ")
rect(-1,-1,1,0, col=rgb(bestColorMasterRGB, alpha=1))
rect(-1,0,1,1, col=rgb(bestColorRGB, alpha=1))
text(0, -0.5, labels = "Master")
text(0, 0.5, labels = "Lab")
plot(0,0, xlim=c(-1,1), ylim=c(-1,1), axes=FALSE, xlab="",ylab="", main = "color with max Δ")
rect(-1,-1,1,0, col=rgb(worstColorMasterRGB, alpha=1))
rect(-1,0,1,1, col=rgb(worstColorRGB, alpha=1))
text(0, -0.5, labels = "Master")
text(0, 0.5, labels = "Lab")
#3d Plot
#par(mfrow=c(2,2))
## @knitr rgbForVisLevelOne
scatterplot3d(RgbForVisLevelOne, color = rgb(RgbForVisLevelOne, alpha=1),xlab = "red", ylab = "green",zlab = "blue", main="Level 1")
## @knitr rgbForVisLevelTwo
scatterplot3d(RgbForVisLevelTwo, color = rgb(RgbForVisLevelTwo, alpha=1),xlab = "red", ylab = "green",zlab = "blue", main="Level 2")
## @knitr rgbForVisLevelThree
scatterplot3d(RgbForVisLevelThree, color = rgb(RgbForVisLevelThree, alpha=1),xlab = "red", ylab = "green",zlab = "blue", main="Level 3")
## @knitr rgbForVisLevelFour
scatterplot3d(RgbForVisLevelFour, color = rgb(RgbForVisLevelFour, alpha=1),xlab = "red", ylab = "green",zlab = "blue", main="Level 4")