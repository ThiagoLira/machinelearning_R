#teste
library(plotly)
#0.8 train-test split
dataPath <- "raw-data"

allResults <- NULL

#building: 0, 1 , 2
#floor : 0,1,2,3,4

#for building 0
floorsList <- c(0,1,2,3)

rates<-NULL

for (i in floorsList){
  
  datasets <- prepareUCIdata2(dataPath,0,i)
  test_s <- datasets$test_s
  train_s <- datasets$train_s
  
  trainedModels <- trainModels(train=datasets$train_s,datasets$train_pca,test)
  
  simpleVote <- singleTestMatrix(dplyr::select(test_s,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree,datasets$train_s)
  weightedVote <- MatrixTestBayesianVote(test=dplyr::select(datasets$test_s,-idZ),NNmodel=trainedModels$NeuralNet,SVMmodel=trainedModels$SVM,TreeModel=trainedModels$Tree,train=datasets$train_s)
  
  NN <-  singleTestNN(dplyr::select(test_s,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree) 
  SVM <- singleTestSVM(dplyr::select(test_s,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree) 
  KNN <- singleTestKNN(dplyr::select(test_s,-idZ),trainedModels$NeuralNet,train=datasets$train_s) 
  Tree <-  singleTestTree(dplyr::select(test_s,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree) 
  correct <- as.numeric(test_s[[1]])
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
  
  rates <- rbind(rates,c(rateVote,rateWeight,rateKNN,rateTree,rateSVM,rateNN,i) )   
  
  
  
}

rates<-data.frame(rates)
names(rates)<- c("rateVote","rateWeight","rateKNN","rateTree","rateSVM","rateNN","Floor")
rates <- t(rates)
rates$algoritmo <- c("Voto Simples","Bayes","KNN","Tree","SVM","NN")

grid.table(rates,rows<-c("0","1","2","3"),cols <- c("rateVote","rateWeight","rateKNN","rateTree","rateSVM","rateNN","Andar"))


ggplot(data=rates,aes(x=rates$Floor, y=dplyr::select(rates,-Floor))) + geom_bar(stat="identity", position=position_dodge())


