library(class) # for `knn`, and other classifiers
library(ggplot2)

directory <- "C:/Users/44790/Documents/assignment2_40263793/section2_features/section2_features.csv"

file2 <- read.csv(directory, header = TRUE)
file2$type <- "number"
for(i in 1:168){
  if(file2[i,1] == "a" | file2[i,1] == "b" | file2[i,1] == "c" | file2[i,1] == "d" | file2[i,1] == "e" | file2[i,1] == "f" | file2[i,1] == "g"){
    file2$type[i] <- "letter"
  }
  else if(file2[i,1] == "approxequal"| file2[i,1] == "equal" | file2[i,1] == "greater" | file2[i,1] == "greaterequal" | file2[i,1] == "less" | file2[i,1] == "lessequal" | file2[i,1] == "notequal"){
    file2$type[i] <- "symbol"
  }
}

ks <- c(1,3,5,7,9,11,13,15,17,19,21,23,25)

tester <- data.frame(file2$nr_pix,file2$rows_with_2,file2$cols_with_2,file2$rows_with_3p,file2$cols_with_3p,file2$height)
tester.labels <- cbind(tester,file2$type)

accuracies <- c()
for(x in ks){
  print(x)
  knn.pred=knn(train = tester,test = tester,file2$type,k=x)
  knn.pred
  
  file2$knn.pred <- knn.pred
  correct_items = file2[["type"]] == file2[["knn.pred"]]
  acc <- sum(correct_items)/168
  print(acc)
  accuracies <- append(accuracies,acc)
  
}
accuracies

#2.2 - cross validation KNN

tester <- data.frame(file2$nr_pix,file2$rows_with_2,file2$cols_with_2,file2$rows_with_3p,file2$cols_with_3p,file2$height)
tester$type <- file2$type

shuffled_data <- tester[sample(nrow(tester)),]
shuffled_data$folds <- cut(seq(1,nrow(shuffled_data)),breaks=5,labels=FALSE)

accuracies <- data.frame(matrix(nrow=0,ncol=3))
accuracies.names <- c("k","error_rate","type")
colnames(accuracies) <- accuracies.names

train_holder <- data.frame(matrix(nrow=0,ncol=3))
test_holder <- data.frame(matrix(nrow=0,ncol=3))
#train.temp <- data.frame(matrix(nrow=0,ncol=3))
#test.temp <- data.frame(matrix(nrow=0,ncol=3))
for(x in ks){
  for(y in 1:5){
    training_data <- shuffled_data[shuffled_data$folds != y,]
    test_data <- shuffled_data[shuffled_data$folds == y,]
    
    training_data2 = subset(training_data, select = -c(type))
    test_data2 = subset(test_data, select = -c(type))
    
    knn.pred1 <- knn(train = training_data2,test = training_data2,training_data$type,k=x)
    knn.pred2 <- knn(train = training_data2,test = test_data2,training_data$type,k=x)
    
    training_data$pred <- knn.pred1
    correct_items1 <- training_data[["type"]] == training_data[["pred"]]
    train.acc <- sum(correct_items1)/length(correct_items1)
    
    test_data$pred <- knn.pred2
    correct_items2 <- test_data[["type"]] == test_data[["pred"]]
    test.acc <- sum(correct_items2)/length(correct_items2)
    
    test.knn <- data.frame(1/x,(1-test.acc),"test")
    colnames(test.knn) <- accuracies.names
    train.knn <- data.frame(1/x,(1-train.acc),"train")
    colnames(train.knn) <- accuracies.names
    
    train_holder <- rbind(train_holder,train.knn)
    test_holder <-rbind(test_holder,test.knn)
    
    
    #test_data$knn.pred <- knn.pred
    #correct_items = test_data[["type"]] == test_data[["knn.pred"]]
    #tes.acc <- sum(correct_items)/length(correct_items)
    #print(acc)
    #accuracies <- rbind(accuracies,test.knn)
    #accuracies <- rbind(accuracies,train.knn)
    
  }
  train.temp <- data.frame(mean(train_holder$X1),
                           mean(train_holder$X2),
                           "train")
  test.temp <- data.frame(mean(test_holder$X1),
                          mean(test_holder$X2),
                          "test")
  
  accuracies <- rbind(accuracies,test.knn)
  accuracies <- rbind(accuracies,train.knn)
}
accuracies

ggplot(data=accuracies, aes(x=k, y=error_rate)) +
  geom_line(aes(color=type)) +
  xlab("1/k") + ylab("error rate")+
  geom_point()


