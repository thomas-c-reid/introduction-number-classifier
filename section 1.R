install.packages('e1071', dependencies=TRUE)

library(ggplot2)
library(caret)
library(MLeval)
directory <- "C:/Users/44790/Documents/assignment2_40263793/section2_features/section2_features.csv"

file1 <- read.csv(directory, header = TRUE)
file1$type <- "number"
for(i in 1:168){
  if(file1[i,1] == "a" | file1[i,1] == "b" | file1[i,1] == "c" | file1[i,1] == "d" | file1[i,1] == "e" | file1[i,1] == "f" | file1[i,1] == "g"){
    file1$type[i] <- "letter"
  }
  else if(file1[i,1] == "approxequal"| file1[i,1] == "equal" | file1[i,1] == "greater" | file1[i,1] == "greaterequal" | file1[i,1] == "less" | file1[i,1] == "lessequal" | file1[i,1] == "notequal"){
    file1$type[i] <- "symbol"
  }
}

file1$binarytype <- 0
file1$binarytype[file1$type == "symbol"] <- 1
file1$is.symbol <- "not maths symbol"
file1$is.symbol[file1$type == "symbol"] <- "maths symbol"


#1.1-----------------------------
#creating the regression model
glmfit <- glm(binarytype ~ nr_pix, data = file1, family = 'binomial')


#creating the curve
x.range = range(file1[["nr_pix"]])
x.values = seq(x.range[1],x.range[2],length.out=1000)
fitted.curve <- data.frame(nr_pix = x.values)
fitted.curve[["binarytype"]] = predict(glmfit, fitted.curve, type="response")

#plotting the regression curve
plt <-ggplot(file1, aes(x=nr_pix, y=binarytype))+
      geom_point(aes(colour = factor(binarytype)), 
           show.legend = T, position="dodge")+
      geom_line(data=fitted.curve, colour="orange", size=1)
plt
summary(glmfit)




#1.2--------------------------------

tControl <- trainControl(method="cv", number= 5, 
                         savePredictions=TRUE, 
                         classProbs = TRUE)
  
lmFit <- train(binarytype ~ nr_pix, data = file1, 
               method = "glm",
               trControl = tControl, family = "binomial")

print(lmFit)

file1[["predicted_val"]] = predict(glmfit, file1, type="response")
file1[["predicted_class"]] = "not maths symbol"
file1[["predicted_class"]][file1[["predicted_val"]] > 0.5] = "math symbol"

correct_items = file1[["predicted_class"]] == file1[["is.symbol"]]
correct_items
sum(correct_items)

cm.accuracy <- table(file1$is.symbol,file1$predicted_class)
cm.accuracy


accuracy <- sum(correct_items)/168
precision1 <- 112/(112+0)
recall1 <- 112/(112+55)
F1 <- 2*((precision1*recall1)/(precision1+recall1))

accuracy
precision1
recall1
F1

#1.3-------------------------------------
res <- evalm(lmFit)
res$roc

