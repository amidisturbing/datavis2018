lab <- read.csv2("/home/alex/Downloads/LabMeasurements-Color-Card.csv")
Master <- read.csv2("/home/alex/Downloads/MasterColorCard.csv")

#t() tranposes the matrix  and c() puts it all into one row
MasterInOneRow <- c(t(as.matrix(Master[,9:11])))
MasterInOneRow
#creates a matrix by repeating the rows to fit the lab data 
MasterRepeated <- matrix(rep(MasterInOneRow, 13*42), byrow = TRUE,nrow = 13*42)
MasterRepeated
LabWithoutFirstColumns <- lab[,-c(1:2)]

deltaMatrix <- MasterRepeated - LabWithoutFirstColumns

print(deltaMatrix)
deltaMatrixPowerTwo <- deltaMatrix^2

deltaMatrixPowerTwo
dim(deltaMatrixPowerTwo)

deltEMatrix <- matrix(NA, nrow = 546, ncol = 64, byrow = FALSE)




for (i in 1:ncol(deltEMatrix)) {
  deltEMatrix[,i] <- sqrt(rowSums(deltaMatrixPowerTwo[,(i-1)*3+(1:3)]))
}


countLevel1 <- sum(deltEMatrix<=1)
countLevel1
countLevel2 <- sum((deltEMatrix>1) & (deltEMatrix<2))
countLevel2
countLevel3 <- sum((deltEMatrix>2) & (deltEMatrix<10))
countLevel3
countLevel4 <- sum((deltEMatrix>11) & (deltEMatrix<49))
countLevel4
countLevel5 <- sum(deltEMatrix>50)
countLevel5





