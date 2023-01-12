library(ggplot2)
library(cowplot)
library(randomForest)
library(caret)
library(class) # for `knn`, and other classifiers
library(rpart)
library(psych)
library(naivebayes)
library(e1071)
library(klaR)
library(nnet)


#getting data formatted correctly
directory <-"C:/Users/44790/Downloads/csc2062_a3data/0IP2qZ/all_features.csv"

features <- read.table(file = directory, sep = "\t", header = FALSE)
colnames(features) <- c("label","index","nr_pix","rows_with_two","cols_with_two","rows_with_three","cols_with_three","Height","Width","left2Tile","right2Tile","Verticalness","top2Tile","bottom2Tile","horizontillness","diagonal")
features[,1] <- as.factor(features[,1])
features<-features[,-2]

#random forest
trees.numbers <- c(25,50,75,100,125,150,175,200,225,250,275,300,325,350,375,400)
np <- c(2,4,6,8)

#3.1---------------------------------------------
#Trying the Train function to make the TuneGrid
#setting the train parameters
final.df <- data.frame(matrix(ncol = 4, nrow = 0))
final.df.names <- c("nt","np","accuracy","kappa")
colnames(final.df)  <- final.df.names

control <- trainControl(method='repeatedcv', 
                        number=5, 
                        search='grid')

tunegrid <- expand.grid(.mtry = np)

count<- 1
for(i in 1:length(trees.numbers)){
  print(count)
  count <- count+1
  
  train1 <- train(label~.,
        data = features,
        method = "rf",
        ntree = trees.numbers[i],
        tuneGrid = tunegrid,
        trControl = control)
  
  df <- data.frame(nt=rep(trees.numbers[i],times=nrow(tunegrid)),
                   np=train1$results$mtry,
                   accuracy=train1$results$Accuracy,
                   kappa=train1$results$Kappa)
  final.df <- rbind(final.df,df)
}
final.df$np <- as.factor(final.df$np)
features[,1] <- as.factor(features[,1])

ggplot(data=final.df, aes(x=nt, y=accuracy)) +
  geom_line(aes(color=np))




#3.2-----------------------------------
#TRY INCLUDE CONFUSION MATRIX
trees.numbers <- c(25,50,75,100,125,150,175,200,225,250,275,300,325,350,375,400)
final.df <- data.frame(matrix(ncol = 4, nrow = 0))
final.df.names <- c("nt","np","accuracy","kappa")
colnames(final.df)  <- final.df.names

control <- trainControl(method='repeatedcv', 
                        number=5, 
                        search='grid')

tunegrid <- expand.grid(.mtry = np)

count<- 1
bigCount <- 1
for(k in 1:15){
  print(bigCount)
  bigCount <- bigCount + 1
  for(i in 1:length(trees.numbers)){
    print(count)
    count <- count+1
    train1 <- train(label~.,
                    data = features,
                    method = "rf",
                    ntree = trees.numbers[i],
                    tuneGrid = tunegrid,
                    trControl = control)
    df <- data.frame(nt=rep(trees.numbers[i],times=nrow(tunegrid)),
                     np=train1$results$mtry,
                     accuracy=train1$results$Accuracy,
                     kappa=train1$results$Kappa)
    final.df <- rbind(final.df,df)
  }
}

average.final.df <- data.frame(matrix(ncol=4,nrow=0))
temp.average <- data.frame(matrix(ncol=4,nrow=0))
colnames(temp.average) <- final.df.names
colnames(average.final.df) <- final.df.names
index <- 1
for(i in 1:64){
  temp.average <- data.frame(matrix(ncol=4,nrow=0))
  for(j in 0:14){
    set <- 64*j
    pointer <- set + i
    temp.ave <- data.frame(final.df[pointer,])
    temp.average <- rbind(temp.average,temp.ave)
  }
  mean.nt <- mean(temp.average$nt)
  mean.np <- mean(temp.average$np)
  mean.accuracy <- mean(temp.average$accuracy)
  mean.kappa <- mean(temp.average$kappa)
  temptemp <- data.frame(nt=mean.nt,
                         np=mean.np,
                         accuracy=mean.accuracy,
                         kappa=mean.kappa)
  average.final.df <- rbind(average.final.df,temptemp)
}
average.final.df$np <- as.factor(average.final.df$np)

ggplot(data=average.final.df, aes(x=nt, y=accuracy)) +
  geom_line(aes(color=np))



#3.3-------------------------------------------------

features1 <- features
features2 <- features
features3 <- features

#USING KNN
results <- data.frame(matrix(nrow=0,ncol=2))
results.names <- c("k","accuracy")
temppp <- data.frame(matrix(nrow=0,ncol=2))
colnames(temppp) <- results.names
colnames(results) <- results.names


features1 <- features
features1 <- subset(features1, select = -c(diagonal) )
ks <- c(1,3,5,7,9,11,13,15,17,19,21,23,25)

shuffled_data <- features1[sample(nrow(features1)),]
shuffled_data$folds <- cut(seq(1,nrow(shuffled_data)),breaks=5,labels=FALSE)

#repeat 10 times cause still has a lot of randomness
for(f in 1:10){
  for(x in ks){
    temppp <- data.frame(matrix(nrow=0,ncol=2))
    colnames(temppp) <- results.names
    for(y in 1:5){
      training_data <- shuffled_data[shuffled_data$folds != y,]
      training_data2 = subset(training_data, select = -c(label))
      test_data <- shuffled_data[shuffled_data$folds == y,]
      test_data2 = subset(test_data, select = -c(label))
    
      knn.pred=knn(train = training_data2,test = test_data2,cl=training_data$label,k=x)
      #knn.pred
    
      test_data$knn.pred <- knn.pred
      correct_items = test_data[["label"]] == test_data[["knn.pred"]]
      acc <- sum(correct_items)/length(correct_items)
      tempp <- c(x,acc)
      #results <-rbind(results,tempp)
      temppp <- rbind(temppp,tempp)
    }
    mean.k <- mean(temppp[,1])
    mean.acc <- mean(temppp[,2])
    tempppp <- c(mean.k,mean.acc)
    results <- rbind(results,tempppp)
  }
}


plot(results)
#K=7 BEST CLASSIFIER
#FIND CODE TO USE BEST K CLASSIFIER
features1 <- features
final.knn.results <- c()

features1 <- features
features1 <- subset(features1, select = -c(diagonal) )
final.k <- 7

shuffled_data <- features1[sample(nrow(features1)),]
shuffled_data$folds <- cut(seq(1,nrow(shuffled_data)),breaks=5,labels=FALSE)

for(h in 1:10){
  for(i in 1:5){
    training_data <- shuffled_data[shuffled_data$folds != i,]
    training_data2 = subset(training_data, select = -c(label))
    test_data <- shuffled_data[shuffled_data$folds == i,]
    test_data2 = subset(test_data, select = -c(label))
  
    #training_data2 <- scale(training_data2)

    knn.pred=knn(train = training_data2,test = test_data2,cl=training_data$label,k=7)
    #knn.pred
  
    test_data$knn.pred <- knn.pred
    correct_items = test_data[["label"]] == test_data[["knn.pred"]]
    acc <- sum(correct_items)/length(correct_items)
    final.knn.results <- c(final.knn.results,acc)
  }
}

#final mean for KNN
mean(final.knn.results)


#USING RANDOM
#Peak for 4 pred at 275 trees

final.df2 <- data.frame(matrix(ncol = 4, nrow = 0))
final.df2.names <- c("nt","np","accuracy","kappa")
colnames(final.df2)  <- final.df2.names

control <- trainControl(method='repeatedcv', 
                        number=5, 
                        search='grid')
count<- 1
bigCount <- 1
for(i in 1:10){
  print(i)
  count <- count+1
  train1 <- train(label~.,
                  data = features2,
                  method = "rf",
                  ntree = 275,
                  np=4,
                  trControl = control)
  
  df <- data.frame(nt=275,
                   np=4,
                   accuracy=train1$results$Accuracy,
                   kappa=train1$results$Kappa)
  final.df2 <- rbind(final.df2,df)
}

mean(final.df2$accuracy)



#USING NAIVE BAYES
features3 <- features

#nbControl <- trainControl(method="cv", number=10)
shuffled_data_nb <- features3[sample(nrow(features3)),]
shuffled_data_nb$folds <- cut(seq(1,nrow(shuffled_data_nb)),breaks=5,labels=FALSE)
nb.accuracies <- c()


  for(j in 1:5){
    print(j)
  
    training_data_nb <- shuffled_data[shuffled_data$folds !=j,]
    training_data2_nb = subset(training_data_nb, select = -c(label))
    test_data_nb <- shuffled_data_nb[shuffled_data_nb$folds == j,]
    test_data2_nb = subset(test_data_nb, select = -c(label))
  
    naiveBayesModel = train(training_data2_nb,training_data_nb$label,'nb',trControl=trainControl(method='cv',number=5))
  
    test_data_nb$pred <- predict(naiveBayesModel,test_data_nb)
  
    correct_items = test_data_nb[["pred"]] == test_data_nb[["label"]]
    correct_items
    #sum(correct_items)
    acc <- sum(correct_items)/nrow(test_data_nb)
    print(acc)
    nb.accuracies <- rbind(nb.accuracies,acc)
  }

mean(nb.accuracies)











#using NNET
# features6 <- features
# 
# sizes <- c(2,4,6,8,10,12,14,16,18,20)
# maxits <- c(100,200,300,400,500,600,700,800,900)
# 
# nnet.accuracies <- data.frame(matrix(nrow=0,ncol=3))
# 
# count <- 0
# for(i in sizes){
#   for(j in maxits){
#     print(count)
#     count <- count + 1
#     
#     features6 <- features
#     
#     nnet.model <- nnet(label~.,data=features6, size = i, rang = 0.1,decay = 5e-4, maxit = j)
#     features6$pred <- predict(nnet.model,features6,type="class")
#     
#     correct_items = features6[["pred"]] == features6[["label"]]
#     correct_items
#     #sum(correct_items)
#     summ <- sum(correct_items)/nrow(features6)
#     
#     temp.nnet <- data.frame(i,j,summ)
#     
#     nnet.accuracies <- rbind(nnet.accuracies,temp.nnet)
#     
#   }
# }
# nnet.names <- c("size","iterations","accuracy")
# colnames(nnet.accuracies) <- nnet.names



#Final NNET
features7 <- features
sizes <- c(2,4,6,8,10,12,14,16,18,20)
maxits <- c(200,300,400,500,600,700,800,900)

shuffled_data_nnet <- features7[sample(nrow(features7)),]
shuffled_data_nnet$folds <- cut(seq(1,nrow(shuffled_data_nb)),breaks=5,labels=FALSE)
final.nnet.accuracies <- c()
temp_holder <- c()
final.results.nnet <- data.frame(matrix(nrow=0, ncol=3))

count <- 1

#Finding best size and maxit
for(i in sizes){
  for(j in maxits){
    for(k in 1:5){
      print(count)
      count <- count + 1
      training_data_nnet <- shuffled_data_nnet[shuffled_data_nnet$folds != k,]
      test_data_nnet <- shuffled_data_nnet[shuffled_data_nnet$folds == k,]
  
      nnet.model <- nnet(label~.,data=training_data_nnet, size = i, rang = 0.1,decay = 5e-4, maxit = j)
  
      test_data_nnet$pred <- predict(nnet.model,test_data_nnet, type="class")
  
      correct_items = test_data_nnet[["pred"]] == test_data_nnet[["label"]]
      nnet.acc <- sum(correct_items)/nrow(test_data_nnet)
      final.nnet.accuracies <- rbind(final.nnet.accuracies, nnet.acc)
    }
    mean(final.nnet.accuracies)
    temp_holder <- c(i,j,mean(final.nnet.accuracies))
    final.results.nnet <- rbind(final.results.nnet, temp_holder)
  }
}
nnet.names <- c("size","maxit","accuracy")
colnames(final.results.nnet) <- nnet.names
final.results.nnet

#best results with
#size = 20
#maxit = 900

#finding best decay
features8 <- features
decays <- c(0,0.1,0.5,1)
decay.acc <- c()
decay.decay <- c()
temp.decay <- c()

shuffled_data_nnet <- features8[sample(nrow(features8)),]
shuffled_data_nnet$folds <- cut(seq(1,nrow(shuffled_data_nnet)),breaks=5,labels=FALSE)
count <- 1
final.nnet.final <- c()

for(i in decays){
  temp.decay <- c()
    for(k in 1:5){
      print(count)
      count <- count + 1
      training_data_nnet <- shuffled_data_nnet[shuffled_data_nnet$folds != k,]
      test_data_nnet <- shuffled_data_nnet[shuffled_data_nnet$folds == k,]
      
      nnet.model <- nnet(label~.,data=training_data_nnet, size = 25, rang = 0.5,decay = i, maxit = 1000)
      
      test_data_nnet$pred <- predict(nnet.model,test_data_nnet, type="class")
      
      correct_items = test_data_nnet[["pred"]] == test_data_nnet[["label"]]
      
      #sum(correct_items)/nrow(test_data_nnet)
      print(sum(correct_items)/nrow(test_data_nnet))
      print(i)
      
      decay.acc <- sum(correct_items)/nrow(test_data_nnet)
      
      temp.decay <- rbind(temp.decay,decay.acc)
      
      #decay.acc <- rbind(decay.acc, sum(correct_items)/nrow(test_data_nnet))
      #decay.decay <- rbind(decay.decay,i)
      
    }
  
  decay.average <- mean(temp.decay)
  temp2 <- cbind(i,decay.average)
  final.nnet.final <- rbind(final.nnet.final,temp2)
  
}


test1 <- c("decay","acc")
colnames(final.nnet.final) <- test1
test1 <- rbind(test1,mean(decay.acc))


#NNET USING SCALED DATA






#Scaling the data
features5 <- features
maxval <- apply(features5[,2:14], 2, max)
minval <- apply(features5[,2:14], 2, min)
scaled.features <- data.frame(scale(features5[,c(2:14)], center = minval, scale= (maxval-minval)))
scaled.features.labels <- features5[,1]

scaled.data <- cbind(scaled.features.labels,scaled.features)

shuffled_data_nnet2 <- scaled.data[sample(nrow(scaled.data)),]
shuffled_data_nnet2$folds <- cut(seq(1,nrow(shuffled_data_nnet2)),breaks=5,labels=FALSE)



training_data_nnet2 <- shuffled_data_nnet2[shuffled_data_nnet2$folds != 1,]
training_data2_nnet2 = subset(training_data_nnet2, select = -c(scaled.features.labels))
test_data_nnet2 <- shuffled_data_nnet2[shuffled_data_nnet2$folds == 1,]
test_data2_nnet2 = subset(test_data_nnet2, select = -c(scaled.features.labels))

#All
nnet.model <- nnet(scaled.features.labels~.,data=training_data_nnet2, size = 25, rang = 0.5,decay = 1, maxit = 1000)
test_data_nnet2$pred <- predict(nnet.model,test_data_nnet2, type="class")

correct_items = test_data_nnet[["pred"]] == test_data_nnet[["label"]]
sum(correct_items)/nrow(test_data_nnet)






