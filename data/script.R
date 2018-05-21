#load the data
setwd("datavis2018")
lab <- read.csv2("data/LabMeasurements-Color-Card.csv")
Master <- read.csv2("data/MasterColorCard.csv")

#t() tranposes the matrix  and c() puts it all into one row
MasterAsOneRow <- c(t(as.matrix(Master[,9:11])))

#creates a matrix by repeating the rows to fit the lab data 
MasterSameSizeAsLab <- matrix(rep(MasterAsOneRow, 13*42), byrow = TRUE,nrow = 13*42)

#cuts off row and col column from lab data
LabWithoutFirstColumns <- lab[,-c(1:2)]

#helper array to get back to lab values after errormatrix was build
columnIndexesInLabMatrix<-seq(from=1, to=192,by=3)

#create a matrx that contains the differences between the Lab values of master and lab
DeltaMasterLab <- MasterSameSizeAsLab - LabWithoutFirstColumns

#square the differences
DeltaMasterSquared <- DeltaMasterLab^2

getIndexInLab <- function(IndexFromErrorMatrix){
  return (((IndexFromErrorMatrix-1)*3)+1)
}


#SpotErrorMatrix contains Lab Errors for each spot (one row is one colour card)
SpotErrorMatrix <- matrix(NA, nrow = 546, ncol = 64, byrow = FALSE)
#fill the empty matrix with the computed errors
for (i in 1:ncol(SpotErrorMatrix)) {
  SpotErrorMatrix[,i] <- sqrt(rowSums(DeltaMasterSquared[,(i-1)*3+(1:3)]))
}
#compute min's and max's
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


# this part is to get the worst and best color spot compared to the master
library(matrixStats)
maxCol<- deltaERangePerCard[match(colMaxs(deltaERangePerCard),deltaERangePerCard[,3])[3],4]
maxRow<- match(colMaxs(deltaERangePerCard),deltaERangePerCard[,3])[3]
minRow<- match(colMins(deltaERangePerCard),deltaERangePerCard)[1]
minCol<- deltaERangePerCard[match(colMins(deltaERangePerCard),deltaERangePerCard)[1],2]

worstColor <- c(LabWithoutFirstColumns[maxRow,columnIndexesInLabMatrix[maxCol]],LabWithoutFirstColumns[maxRow,columnIndexesInLabMatrix[maxCol]+1],LabWithoutFirstColumns[maxRow,columnIndexesInLabMatrix[maxCol]+2])
bestColor <- c(LabWithoutFirstColumns[minRow,columnIndexesInLabMatrix[minCol]],LabWithoutFirstColumns[minRow,columnIndexesInLabMatrix[minCol]+1],LabWithoutFirstColumns[minRow,columnIndexesInLabMatrix[minCol]+2])
worstColorMaster<- c(MasterAsOneRow[columnIndexesInLabMatrix[maxCol]],MasterAsOneRow[columnIndexesInLabMatrix[maxCol]+1],MasterAsOneRow[columnIndexesInLabMatrix[maxCol]+2])
bestColorMaster<- c(MasterAsOneRow[columnIndexesInLabMatrix[minCol]],MasterAsOneRow[columnIndexesInLabMatrix[minCol]+1],MasterAsOneRow[columnIndexesInLabMatrix[minCol]+2])

bestColorRGB <- convertColor(bestColor,from = "Lab", to="sRGB")
worstColorRGB <-convertColor(worstColorRGB,from = "Lab", to="sRGB")
worstColorMasterRGB<-convertColor(worstColorMaster,from = "Lab", to="sRGB")
bestColorMasterRGB<-convertColor(bestColorMaster,from = "Lab", to="sRGB")

#count how many spots belong into each visibility group
VisLevelOne <- sum(SpotErrorMatrix<=1)
VisLevelTwo <- sum((SpotErrorMatrix>1) & (SpotErrorMatrix<2))
VisLevelThree <- sum((SpotErrorMatrix>2) & (SpotErrorMatrix<10))
VisLevelFour <- sum((SpotErrorMatrix>11) & (SpotErrorMatrix<49))
VisLevelFive <- sum(SpotErrorMatrix>50)

#plot the groups
barplot(c(VisLevelOne,VisLevelTwo,VisLevelThree,VisLevelFour,VisLevelFive),
        (1:5),
        names.arg = c("Not Perceptable","Hardly Perceptable","At Glance","very Perceptible","Opposite"))

#visibility of color changes per card
meanErrorPerTarget <- rowMeans(SpotErrorMatrix)

#plot the groups
hist(meanErrorPerTarget, breaks="Sturge", col="grey", labels = T,main="colour card errors")
plot(density(meanErrorPerTarget))

#visibility of color changes per sample
meanErrorPerSample <- c(1:13)

for (i in 1:13) {
  counter<-i;
  for (x in 1:42){
    meanErrorPerSample[i]<-meanErrorPerSample[i]+meanErrorPerTarget[counter]
    counter<-counter+13
  }
  meanErrorPerSample[i]<- meanErrorPerSample[i]/42
}


#try to get back from error to lab values in table
#get indexes of errors in certain level
LumenIndexVisLvlOne <- which(SpotErrorMatrix<=1, arr.ind=TRUE)
LumenIndexVisLvlTwo <- which(((SpotErrorMatrix>1) & (SpotErrorMatrix<2)), arr.ind=TRUE)
LumenIndexVisLvlThree <- which(((SpotErrorMatrix>2) & (SpotErrorMatrix<10)), arr.ind=TRUE)
LumenIndexVisLvlFour <- which(((SpotErrorMatrix>11) & (SpotErrorMatrix<49)), arr.ind=TRUE)
#create helper to retrieve indexes of L values
columnIndexesInLabMatrix<-seq(from=1, to=192,by=3)
#create matrix to fill
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

#convert lab matrix to rgb matrix
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

par(mfrow=c(2,2))
library(scatterplot3d)
scatterplot3d(RgbForVisLevelOne, color = rgb(RgbForVisLevelOne, alpha=1),xlab = "red", ylab = "green",zlab = "blue", main="Level 1")
scatterplot3d(RgbForVisLevelTwo, color = rgb(RgbForVisLevelTwo, alpha=1),xlab = "red", ylab = "green",zlab = "blue", main="Level 2")
scatterplot3d(RgbForVisLevelThree, color = rgb(RgbForVisLevelThree, alpha=1),xlab = "red", ylab = "green",zlab = "blue", main="Level 3")
scatterplot3d(RgbForVisLevelFour, color = rgb(RgbForVisLevelFour, alpha=1),xlab = "red", ylab = "green",zlab = "blue", main="Level 4")


