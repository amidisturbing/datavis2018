#load the data
setwd("datavis2018")
lab <- read.csv2("data/LabMeasurements-Color-Card.csv")
Master <- read.csv2("data/MasterColorCard.csv")

#set plot matrix
par(mfrow=c(2,2))
#t() tranposes the matrix  and c() puts it all into one row
MasterAsOneRow <- c(t(as.matrix(Master[,9:11])))

#creates a matrix by repeating the rows to fit the lab data 
MasterSameSizeAsLab <- matrix(rep(MasterAsOneRow, 13*42), byrow = TRUE,nrow = 13*42)

#cuts off row and col column from lab data
LabWithoutFirstColumns <- lab[,-c(1:2)]

#create a matrx that contains the differences between the Lab values of master and lab
DeltaMasterLab <- MasterSameSizeAsLab - LabWithoutFirstColumns

#square the differences
DeltaMasterSquared <- DeltaMasterLab^2

#SpotErrorMatrix contains Lab Errors for each spot (one row is one colour card)
SpotErrorMatrix <- matrix(NA, nrow = 546, ncol = 64, byrow = FALSE)
#fill the empty matrix with the computed errors
for (i in 1:ncol(SpotErrorMatrix)) {
  SpotErrorMatrix[,i] <- sqrt(rowSums(DeltaMasterSquared[,(i-1)*3+(1:3)]))
}
#compute min's and max's
deltaERangePerCard  <- matrix(NA, nrow = nrow(SpotErrorMatrix), ncol = 2, byrow = FALSE)
rownames(deltaERangePerCard) <- c(1:nrow(SpotErrorMatrix))
colnames(deltaERangePerCard) <- c("Minimum","Maximum")
#deltaERangePerCard <- as.table(deltaERangePerCard)
#deltaERangePerCard["Minimum"] 

for (i in 1:nrow(SpotErrorMatrix)) {
  rafMin <- min(SpotErrorMatrix[i,])
  rafMax <- max(SpotErrorMatrix[i,])
  minIndex <- match(c(rafMin),SpotErrorMatrix[i,])
  maxIndex <- match(c(rafMax),SpotErrorMatrix[i,])
  #print(c(i, rafMin, minIndex, rafMax, maxIndex))
  deltaERangePerCard[i,"Minimum"] <- min(SpotErrorMatrix[i,])
  deltaERangePerCard[i,"Maximum"] <- max(SpotErrorMatrix[i,])
}

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
indexMatrix <- which(SpotErrorMatrix<=1, arr.ind=TRUE)
#create helper to retrieve indexes of L values
columnIndexesInLabMatrix<-seq(from=1, to=192,by=3)
#create matrix to fill
LabForLevel <- matrix(NA, nrow = nrow(indexMatrix), ncol = 3, byrow = FALSE)

#get the lab values for each element of the index list
for(i in 1:nrow(indexMatrix)){
  LabForLevel[i,1]<-LabWithoutFirstColumns[indexMatrix[i,1],columnIndexesInLabMatrix[indexMatrix[i,2]]]
  LabForLevel[i,2]<-LabWithoutFirstColumns[indexMatrix[i,1],columnIndexesInLabMatrix[indexMatrix[i,2]]+1]
  LabForLevel[i,3]<-LabWithoutFirstColumns[indexMatrix[i,1],columnIndexesInLabMatrix[indexMatrix[i,2]]+2]
}

#convert lab matrx to rgb matrix
RgbForLevel <- matrix(NA, nrow = nrow(LabForLevel), ncol = 3, byrow = FALSE)
for(i in 1:nrow(LabForLevel)){
  RgbForLevel[i,1:3]<-convertColor(LabForLevel[i,1:3],from = "Lab", to="sRGB")
}

library(scatterplot3d)
scatterplot3d(RgbForLevel, color = rgb(RgbForLevel, alpha=1),xlab = "red", ylab = "green",zlab = "blue", main="Level 1")


