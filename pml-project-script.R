require(randomForest)
set.seed(1011)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

traind <- read.csv("pml-training.csv")
testd <- read.csv("pml-testing.csv")
traind <-traind[,7:ncol(traind)]
traind[is.na(traind)] <- 0
testd <-testd[,7:ncol(testd)]
testd[is.na(testd)] <- 0

for(i in 1:153){
  traind[,i] <- as.numeric(traind[,i])
  testd[,i] <- as.numeric(testd[,i])
}

fit <- randomForest(classe~.,data=traind,ntree=1000)
predicted_classe <- predict(fit,testd)

pml_write_files(predicted_classe)
