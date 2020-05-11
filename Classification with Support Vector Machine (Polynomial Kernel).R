# Import Data #
movie <- read.csv("E:/My Dictionary/Using R/Data/Movie_classification.csv")
View(movie)
str(movie)

# Data Preprocessing #
summary(movie) #there are missing values in variable Time_taken
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken,na.rm = TRUE) #imputasi with mean because it is numerical variable
movie$Start_Tech_Oscar <- as.factor(movie$Start_Tech_Oscar)

# Test-Train Split #
install.packages('caTools')
library(caTools)
set.seed(0)
split =sample.split(movie,SplitRatio = 0.8)
trainc = subset(movie,split == TRUE)
testc = subset(movie,split == FALSE)

#################################### POLYNOMIAL KERNEL SVM #######################################
install.packages('e1071')
library (e1071)

# Modeling #
svmfitp <- svm(Start_Tech_Oscar~., data=trainc, kernel="polynomial", cost=1, degree=2)
summary (svmfitp)
svmfitp$index #index of support vectors

# Predicting #
predp <- predict(svmfitp,testc)
cm <- table(predict = predp, truth = testc$Start_Tech_Oscar)
cm
Accuracy <- (cm[1,1]+cm[2,2])/sum(cm)
Accuracy

# Tuning Parameter
tunep=tune(svm ,Start_Tech_Oscar~.,data=trainc, cross = 4, kernel="polynomial", ranges =list(cost=c(0.001,0.1, 1,5,10),degree=c(0.5,1,2,3,5)))
bestmodp =tunep$best.model #cost = 5, degree = 1
summary (bestmodp)
predp = predict (bestmodp,testc) 
cm <- table(predict = predp, truth = testc$Start_Tech_Oscar)
cm
Accuracy <- (cm[1,1]+cm[2,2])/sum(cm)
Accuracy
