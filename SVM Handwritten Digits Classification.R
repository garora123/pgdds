#install.packages("caret")
#install.packages("kernlab")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("caTools")

library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(caTools)


#--------------------------------------------------------------------
# 1. Loading Data
#####################################################################

mnist_train <- read.csv("mnist_train.csv", stringsAsFactors = F, header = F)
mnist_test <- read.csv("mnist_test.csv", stringsAsFactors = F, header = F)


# Renaming column names
colnames(mnist_train)[1] <- c("Label")
colnames(mnist_test)[1] <- c("Label")

# Understanding Dimensions
dim(mnist_train) # 60000 rows, 785 cols
dim(mnist_test) # 10000 rows, 785 cols

# Structure of the data set
str(mnist_train)
str(mnist_test)

#Exploring the data
summary(mnist_train)
summary(mnist_test)

# Changing output variable "Label" to factor type 
mnist_train$Label <- as.factor(mnist_train$Label)
mnist_test$Label <- as.factor(mnist_test$Label)

# Checking missing value
sapply(mnist_train, function(x) sum(is.na(x))) # No missing values
sapply(mnist_test, function(x) sum(is.na(x))) # No missing values


#--------------------------------------------------------------------
# 2. Splitting the data between train and test
#####################################################################
# Checking zero variance variables and remove
combined <- rbind(mnist_train, mnist_test)
remove_cols <- nearZeroVar(combined, names = TRUE, freqCut = 95/5, uniqueCut = 5)
final <- combined[,!names(combined) %in% remove_cols]
dim(final) # 70000 rows, 251 cols

# Split into test and train data
set.seed(100)
indices_new <- sample.split(final$Label, SplitRatio = 0.15) # 15% is sufficient since data set is huge
svm.train <- final[indices_new,]
svm.test <- final[!indices_new,]
dim(svm.train) # 10500 rows, 251 cols
dim(svm.test) # 59500 rows, 251 cols

# 3. Model Building

#--------------------------------------------------------------------
# 3.1 Linear model - SVM
#####################################################################

model_linear<- ksvm(Label~ ., data = svm.train, scale = TRUE, kernel = "vanilladot")
predict_linear <- predict(model_linear, svm.test)
confusionMatrix(predict_linear, svm.test$Label)
# Accuracy - 88%

#--------------------------------------------------------------------
# 3.2 RBF model - SVM
#####################################################################
model_rbf <- ksvm(Label~ ., data = svm.train, scale = TRUE, kernel = "rbfdot")
predict_rbf <- predict(model_rbf, svm.test)
confusionMatrix(predict_rbf, svm.test$Label)
# Accuracy - 95%

#--------------------------------------------------------------------
# 3.3 Polynomial model - SVM
#####################################################################
model_poly <- ksvm(Label~., data = svm.train, scale = TRUE, kernel = "polydot")
predict_poly <- predict(model_poly, svm.test)
confusionMatrix(predict_poly, svm.test$Label)
# Accuracy - 88%
# Polynomial & Linear kernel is not as good as RBF kernel, so rejected

# We will proceed with RBF model cross-validation to tune the model now
# since its accuracy is better than linear and polynomial
grid <- expand.grid(.sigma=c(0.01,0.02), .C=c(1, 5, 10))
trainControl <- trainControl(method = "cv", number = 3)
metric <- "Accuracy"
fit.svm <- train(Label~.,data = svm.train,
                 method = "svmRadial", metric = metric,
                 tuneGrid = grid, trControl = trainControl)
print(fit.svm)

plot(fit.svm)

#------------------------------------------------------------------
# 4. RESULT
###################################################################
# With sigma = 0.01 and c=5, Accuracy = 95.6% is achieved
# which is the best, so we stop here!!!