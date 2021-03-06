

path <- "~/Documents/machinelearning_R/datasets"
path <- "~/Documents/netmap/raw-data"

zones <- c(101, 102 ,103 ,104, 105, 106, 107, 108, 109, 110, 111, 112,113,114,115,116,117,118,119,120,121)

zones <- c(101,102,103,104,105)
#feed the location of the dataset to the function
datasets <- prepareUCIdata(path,zones)



trainedModels<-trainModels(datasets$train_s,datasets$train_pca,datasets$test_s)


temp<-NULL
for (i in 1:nrow(test_s)){
 temp<-rbind(temp,singleTest(dplyr::select(test_s[i,],-idZ),NeuralNet,SVM,KNN,Tree) == test_s[i,]$idZ)
}


print (mean(temp))

#apply(dplyr::select(test_s,-idZ),1,singleTest,NNmodel=NeuralNet,SVMmodel=SVM,KNNmodel=KNN)








#K-FOLD CROSS-VALIDATION NEURALNET
NNerrorList <- NULL
kNumber <- 10

flds <- createFolds(scaled$idZ, k = kNumber, list = TRUE, returnTrain = FALSE)

#flds[[1]] gets first fold indexes, etc
#200 neuronios parace ser bom
#neuronList <- c(50,80,100,120,150,200,300,350,400,450)
#entre 200 e 250 eh loco
neuronList <- c(200,210,220,230,240,250,260,270,280,290)


#1 neuron HL
for( i in 1:kNumber){
  NNerrorList <- rbind(NNerrorList,crossValidateNN(scaled[-flds[[i]],],scaled[flds[[i]],],neuronList[i]))
}

plot(neuronList,NNerrorList[,2],pch="Δ",ylab = "Erro de Validação",xlab="# neuronios na HL",main="Cross-Validation 10-Fold para Rede Neural")



















#SVM CROSS-VALIDATION 
SVMerrorList <- NULL
kNumber <- 4

flds <- createFolds(scaled$idZ, k = kNumber, list = TRUE, returnTrain = FALSE)


kernelList <- c("linear","polynomial","radial","sigmoid")

errorMean <- NULL


for (i in 1:100){
  flds <- createFolds(scaled$idZ, k = kNumber, list = TRUE, returnTrain = FALSE)
  SVMerrorList <- NULL
  for( i in 1:kNumber){
    SVMerrorList <- rbind(SVMerrorList,crossValidateSVM(scaled[-flds[[i]],],scaled[flds[[i]],],kernelList[i]))
  }
  
  errorMean <- cbind(errorMean,SVMerrorList[,2])
}

print(SVMerrorList)




meanList <- apply(errorMean,1,mean)
varList <- apply(errorMean,1,var)
grid.table(rbind(kernelList,meanE,meanvar),rows <- c("Kernel","Média do Erro","Variância do Erro"))













#test with incremental number of Zones



zones <- c(116,117,118,119,120,121,122,123,124,125,126,127,128,129)



bigTest <- NULL

for ( zNumber in 3:14) {
  
  
  
  datasets <-prepareUCIdata("~/Documents/netmap/datasets",sample(zones, zNumber, replace = FALSE, prob = NULL))
  
  trainedModels <- trainModels(datasets$train_s,datasets$train_pca)
  
  
  
  testVector <- NULL
  results <- NULL
  for (i in 1:nrow(datasets$test_s)){
    results <- rbind (results,singleTest(dplyr::select(datasets$test_s[i,],-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$KNN,trainedModels$Tree) == datasets$test_s[i,]$idZ)
  }
  
  
  print ( c("teste para ",zNumber, " zonas :" ,mean(results)  ))
  bigTest <- rbind(bigTest,c(zNumber,1-mean(results)))
  
  
}





# CROSS VALIDATE DECISION TREE




datasets <-prepareUCIdata("~/Documents/netmap/datasets",c(110,111))



crossValidateTree(datasets$train_s,datasets$test_s)







test <- dplyr::select(datasets$test_s,-idZ)


vote <- MatrixTestBayesianVote(test,trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree,datasets$train_s)



error <- mean(vote == dplyr::select(test_s,idZ))


###################################
####TESTS IN FLOORS################



datasets <- prepareUCIdata2(path,0,2)

trainedModels<-trainModels(datasets$train_s,datasets$train_pca,datasets$test_s)


simpleVote <- singleTestMatrix(dplyr::select(datasets$test_s,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree,datasets$train_s)
weightedVote <- MatrixTestBayesianVote(dplyr::select(datasets$test_s,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree,datasets$train_s)

NN <-  singleTestNN(dplyr::select(datasets$test_s,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$KNN,trainedModels$Tree) 
SVM <- singleTestSVM(dplyr::select(datasets$test_s,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$KNN,trainedModels$Tree) 
KNN <- singleTestKNN(dplyr::select(datasets$test_s,-idZ),trainedModels$NeuralNet, datasets$train_s)
Tree <-  singleTestTree(dplyr::select(datasets$test_s,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$KNN,trainedModels$Tree) 
correct <- as.numeric(dplyr::select(datasets$test_s,idZ)[[1]])
factors<- trainedModels$NeuralNet$model.list$response
factors <- gsub("`",'',factors)
correct <- as.numeric(factors[correct])




allResults <- rbind(allResults,cbind(simpleVote,NN,SVM,KNN,Tree,correct,weightedVote))




rateVote <- mean(allResults[,1]==allResults[,6])
rateNN <- mean(allResults[,2]==allResults[,6])
rateSVM <- mean(allResults[,3]==allResults[,6])
rateKNN <- mean(allResults[,4]==allResults[,6])
rateTree <- mean(allResults[,5]==allResults[,6])
rateWeight <- mean(allResults[,7]==allResults[,6])

bigResults <- rbind(bigResults,c(rateVote,rateWeight,rateNN,rateSVM,rateKNN,rateTree,zNumber))
print( c(rateVote,rateWeight,rateNN,rateSVM,rateKNN,rateTree))





####TESTS WITH INCREASING NUMBER OF TRAIN POINTS
datasets <- prepareUCIdata2(path,0,2)

for ( i in 11:nrow(datasets$train_s)){
  trainedModels<-trainModels(datasets$train_s[10:i,],datasets$train_pca,datasets$test_s)
  weightedVote <- MatrixTestBayesianVote(dplyr::select(datasets$test_s,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree,datasets$train_s[10:i,])
  correct <- as.numeric(dplyr::select(datasets$test_s,idZ)[[1]])
  factors<- trainedModels$NeuralNet$model.list$response
  factors <- gsub("`",'',factors)
  correct <- as.numeric(factors[correct])
  print(mean(correct==weightedVote))
  break
}

