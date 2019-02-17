#install.packages("Metrics")
#install.packages("psych")

library(tidyr)
library(dplyr)

# Generic function to trim csv files after spread operation
xyz = function (a,b) {
  
  x <- array(names(a))
  y <- array(names(b))
  d <- c(setdiff(y,x))
  c <- c(unique(b$Provider.ID))
  
  trimmed_df <- NULL
  
  for (i in c) {
    rows = filter(b, b$Provider.ID==i)
    #print(rows)
    for (j in d) {
      rows[j][1] = ifelse(length(which(!is.na(rows[j]),arr.ind = T))==0, NA, rows[j][which(!is.na(rows[j]),arr.ind = T)])
    }
    rows = rows[-c(2:length(rows)),]
    trimmed_df = rbind(trimmed_df, rows[1,])
    rows = NULL
  }
  return(trimmed_df)
}

# Hospital General Information
hospital <- read.csv("Hospital General Information.csv", header = T, stringsAsFactors = F)
names(hospital)
hospital$Hospital.overall.rating <- as.factor(hospital$Hospital.overall.rating)
df_master <- hospital[,c(1,9,13)]

# HCAHPS - Hospital
hcahps_hospital <- read.csv("HCAHPS - Hospital.csv", header = T, stringsAsFactors = F)
names(hcahps_hospital)
new_hcahps_hospital <- hcahps_hospital
new_hcahps_hospital$HCAHPS.Measure.ID <- as.factor(new_hcahps_hospital$HCAHPS.Measure.ID)
new_hcahps_hospital <- spread(new_hcahps_hospital, "HCAHPS.Measure.ID", "HCAHPS.Linear.Mean.Value")
hcahps_hospital_pruned <- xyz(hcahps_hospital, new_hcahps_hospital)
names(hcahps_hospital_pruned)
index_score <- grep("LINEAR_SCORE", names(hcahps_hospital_pruned))
hcahps_hospital_pruned <- hcahps_hospital_pruned[,c(1,index_score)]
names(hcahps_hospital_pruned)
df_master <- merge(df_master, hcahps_hospital_pruned, "Provider.ID", all.x = TRUE)


# Timely and Effective Care - Hospital
timely_effective_care <- read.csv("Timely and Effective Care - Hospital.csv", header = T, stringsAsFactors = F)
names(timely_effective_care)
new_timely_effective_care <- spread(timely_effective_care, "Measure.ID", "Score")
timely_effective_care_pruned <- xyz(timely_effective_care, new_timely_effective_care)
names(timely_effective_care_pruned)
timely_effective_care_pruned <- timely_effective_care_pruned[,c(1,15,16,18,19,22,
                                                                23,24,25,26,27,28,
                                                                29,30,32,33,
                                                                35,36,37,39,40,41,
                                                                43,44,
                                                                48,49,52,53,54,55,
                                                                57)]
df_master <- merge(df_master, timely_effective_care_pruned, "Provider.ID", all.x = T)


# Complications - Hospital
complications_hospital <- read.csv("Complications - Hospital.csv", header = T, stringsAsFactors = F)
dim(complications_hospital)
names(complications_hospital)
new_complications_hospital <- spread(complications_hospital, "Measure.ID", "Score")
names(new_complications_hospital)
complications_hospital_pruned <- xyz(complications_hospital, new_complications_hospital)
names(complications_hospital_pruned)
complications_hospital_pruned <- complications_hospital_pruned[,c(1,17:27)]
df_master <- merge(df_master, complications_hospital_pruned, "Provider.ID", all.x = T)


# Healthcare Associated Infections - Hospital
hcare_assoc_inf <- read.csv("Healthcare Associated Infections - Hospital.csv", header = T, stringsAsFactors = F)
dim(hcare_assoc_inf)
names(hcare_assoc_inf)
new_hcare_assoc_inf <- spread(hcare_assoc_inf, "Measure.ID", "Score")
dim(new_hcare_assoc_inf)
hcare_assoc_inf_pruned <- xyz(hcare_assoc_inf, new_hcare_assoc_inf)
dim(hcare_assoc_inf_pruned)
names(hcare_assoc_inf_pruned)
index <- c(grep("\\d\\_SIR",names(hcare_assoc_inf_pruned)))
hcare_assoc_inf_pruned <- hcare_assoc_inf_pruned[,c(1,index)]
df_master <- merge(df_master, hcare_assoc_inf_pruned, "Provider.ID", all.x = T)


# Readmissions and Deaths - Hospital
readm_deaths <- read.csv("Readmissions and Deaths - Hospital.csv", header = T, stringsAsFactors = F)
dim(readm_deaths)
names(readm_deaths)
new_readm_deaths <- spread(readm_deaths, "Measure.ID", "Score")
dim(new_readm_deaths)
readm_deaths_pruned <- xyz(readm_deaths, new_readm_deaths)
names(readm_deaths_pruned)
readm_deaths_pruned <- readm_deaths_pruned[,c(1,17:30)]
df_master <- merge(df_master, readm_deaths_pruned, "Provider.ID", all.x = T)



# Outpatient Imaging Efficiency - Hospital
outpat_imag_eff <- read.csv("Outpatient Imaging Efficiency - Hospital.csv", header = T, stringsAsFactors = F)
dim(outpat_imag_eff)
names(outpat_imag_eff)
new_outpat_imag_eff <- spread(outpat_imag_eff, "Measure.ID", "Score")
dim(new_outpat_imag_eff)
outpat_imag_eff_pruned <- xyz(outpat_imag_eff, new_outpat_imag_eff)
names(outpat_imag_eff_pruned)
outpat_imag_eff_pruned <- outpat_imag_eff_pruned[,c(1,13:18)]
df_master <- merge(df_master, outpat_imag_eff_pruned, "Provider.ID", all.x = T)


dim(df_master)
names(df_master)
temp_master <- df_master #-------!!!!SAVE DF_MASTER!!!!-------#
temp_master <- filter(temp_master, temp_master$Hospital.Type=="Acute Care Hospitals")
dim(temp_master)

# remove measures with no more than 100 hospitals reporting performance publicly
names(temp_master)

sapply(temp_master, function(x) (length(grep("Not Available", as.factor(x))) >= nrow(temp_master) - 100))
notenough <- c("AMI_7a", "OP_1", "OP_2") # 15,21,23
sapply(temp_master, function(x) (length(grep("Not Applicable", as.factor(x))) >= nrow(temp_master) - 100))
sapply(temp_master, function(x) (sum(is.na(x)) >= nrow(temp_master) - 100))
temp_master <- temp_master[,-c(15,21,23)]

# remove rows with no data-
x = temp_master[,c(1:3)]
y = temp_master[,-c(1:3)]
p=vector()
for (i in 1:nrow(y)) {
  p[i] = ifelse(length(grep("Not Available", y[i,])) == ncol(y), "1", "0")
}
index <- which(p=="1")
temp_master <- cbind(x, y)
temp_master <- temp_master[-index,]


# fix NAs
for (i in 1:ncol(temp_master)) {
  index = which(temp_master[[i]] == "Not Available")
  #print(index)
  temp_master[index,i] <- NA
}

names(temp_master)

# Part 1(1) - create 7 groups with measures
# 1. mortality
mortality <- temp_master[,c(1,3,48,59:64)] # 7 measures

# 2. Safety of care
safety_of_care <- temp_master[,c(1,42,52:58)] # 8 measures

# 3. readmission
readmission <- temp_master[,c(1,65:72)] # 8 measures

# 4. patient experience
patient_experience <- temp_master[,c(1,4:14)] # 11 measures 

# 5. effectiveness of care
effective_care <- temp_master[,c(1,15,18,23,24,25,28:41)] # 19 measures

# 6. timeliness of care
timely_care <- temp_master[,c(1,16,17,20,21,22,26,27)] # 7 measures

# 7. efficient use of medical imaging
efficient_imaging <- temp_master[,c(1,73:78)] # 6 measures

# !!--------------Total 65 measures-------------!!

# DATA SET W/ REQUIRED MEASURES
library(tidyr)
df_final <- merge(mortality, safety_of_care, "Provider.ID", all.x = T)
df_final <- merge(df_final, readmission, "Provider.ID", all.x = T)
df_final <- merge(df_final, patient_experience, "Provider.ID", all.x = T)
df_final <- merge(df_final, effective_care, "Provider.ID", all.x = T)
df_final <- merge(df_final, timely_care, "Provider.ID", all.x = T)
df_final <- merge(df_final, efficient_imaging, "Provider.ID", all.x = T)

dim(df_final)

# Fix the data types
str(df_final)
names(df_final)
df_final$Provider.ID <- as.character(df_final$Provider.ID)
x = df_final[,c(1:2)]
y = df_final[,-c(1:2)]
y = data.frame(sapply(y, function(x) as.numeric(x)))
df_final <- cbind(x,y)
names(df_final)[1] <- "Provider.ID"
df_final$Provider.ID <- as.character(df_final$Provider.ID)


# number of measures reported by each hospital under mortality
names(df_final)
x = as.vector(apply(df_final[,c(3:9)], 1, function(x) sum(is.na(x))))
num_measure <- ncol(df_final[,c(3:9)]) - x
mortality_index <- which(num_measure < 3)
q = cbind(df_final[,c(3:9)], num_measure)
xx <- barplot(table(q$num_measure), col = rainbow(20),main="Count of Hospitals by Number of Mortality Measures", xlab = "Number of 
     Mortality Measures", ylab = "Frequency")
freqs <- aggregate(q$num_measure, by=list(q$num_measure), FUN = length)
text(xx, 2,6, labels = freqs$x, cex = 1, pos = 3)


# number of measures reported by each hospital under safety_of_care
names(df_final)
x = as.vector(apply(df_final[,c(10:17)], 1, function(x) sum(is.na(x))))
num_measure <- ncol(df_final[,c(10:17)]) - x
safety_of_care_index <- which(num_measure < 3)
q = cbind(df_final[,c(10:15)], num_measure)
xx <-barplot(table(q$num_measure), col = rainbow(20),main="Count of Hospitals by Number of Safety of Care Measures", 
            xlab = "Number of Safety of Care Measures", ylab = "Frequency")
freqs <- aggregate(q$num_measure, by=list(q$num_measure), FUN = length)
text(xx, 2,6, labels = freqs$x, cex = 1, pos = 3)


# number of measures reported by each hospital under readmission
names(df_final)
x = as.vector(apply(df_final[,c(18:25)], 1, function(x) sum(is.na(x))))
num_measure <- ncol(df_final[,c(18:25)]) - x
readmission_index <- which(num_measure < 3)
q = cbind(df_final[,c(16:23)], num_measure)
xx <-barplot(table(q$num_measure), col = rainbow(20),main="Count of Hospitals by Number of Readmission Measures", 
             xlab = "Number of Readmission Measures", ylab = "Frequency")
freqs <- aggregate(q$num_measure, by=list(q$num_measure), FUN = length)
text(xx, 2,6, labels = freqs$x, cex = 1, pos = 3)


# number of measures reported by each hospital under patient_experience
names(df_final)
x = as.vector(apply(df_final[,c(26:36)], 1, function(x) sum(is.na(x))))
num_measure <- ncol(df_final[,c(26:36)]) - x
patient_experience_index <- which(num_measure < 3)
q = cbind(df_final[,c(24:34)], num_measure)
xx <-barplot(table(q$num_measure), col = rainbow(20),main="Count of Hospitals by Number of Patient Experience Measures", 
             xlab = "Number of Patient Experience Measures", ylab = "Frequency")
freqs <- aggregate(q$num_measure, by=list(q$num_measure), FUN = length)
text(xx, 2,6, labels = freqs$x, cex = 1, pos = 3)


# number of measures reported by each hospital under effective_care
names(df_final)
x = as.vector(apply(df_final[,c(37:55)], 1, function(x) sum(is.na(x))))
num_measure <- ncol(df_final[,c(37:55)]) - x
effective_care_index <- which(num_measure < 3)
q = cbind(df_final[,c(35:53)], num_measure)
xx <-barplot(table(q$num_measure), col = rainbow(20),main="Count of Hospitals by Number of Effectiveness of Care Measures", 
             xlab = "Number of Effectiveness of Care Measures", ylab = "Frequency")
freqs <- aggregate(q$num_measure, by=list(q$num_measure), FUN = length)
text(xx, 2,6, labels = freqs$x, cex = 1, pos = 3)



# number of measures reported by each hospital under timely_care
names(df_final)
x = as.vector(apply(df_final[,c(56:62)], 1, function(x) sum(is.na(x))))
num_measure <- ncol(df_final[,c(56:62)]) - x
timely_care_index <- which(num_measure < 3)
q = cbind(df_final[,c(54:60)], num_measure)
xx <-barplot(table(q$num_measure), col = rainbow(20),main="Count of Hospitals by Number of Timeliness of Care Measures", 
             xlab = "Number of Timeliness of Care Measures", ylab = "Frequency")
freqs <- aggregate(q$num_measure, by=list(q$num_measure), FUN = length)
text(xx, 2,6, labels = freqs$x, cex = 1, pos = 3)


# number of measures reported by each hospital under efficient_imaging
names(df_final)
x = as.vector(apply(df_final[,c(63:68)], 1, function(x) sum(is.na(x))))
num_measure <- ncol(df_final[,c(63:68)]) - x
efficient_imaging_index <- which(num_measure < 3)
q = cbind(df_final[,c(61:66)], num_measure)
xx <-barplot(table(q$num_measure), col = rainbow(20),main="Count of Hospitals by Number of Efficient Imaging Measures", 
             xlab = "Number of Efficient Imaging Measures", ylab = "Frequency")
freqs <- aggregate(q$num_measure, by=list(q$num_measure), FUN = length)
text(xx, 2,6, labels = freqs$x, cex = 1, pos = 3)


rating_na_index <- which(is.na(df_final$Hospital.overall.rating))

df_final <- df_final[-rating_na_index,]

names(df_final)
high_NA <- which(data.frame(sapply(df_final, function(x) sum(is.na(x)))) >= 0.8*nrow(df_final))
df_final <- df_final[,-high_NA]
dim(df_final)

df_final_trial <- df_final

# 58 measures - FINAL!!

# NA imputation - replace all NA with column median
sapply(df_final, function(x) sum(is.na(x)))
for (i in 3:ncol(df_final)) {
  df_final[which(is.na(df_final[[i]])),i] <- median(df_final[[i]], na.rm = T)
}

# Outlier Treatment
xx = data.frame(sapply(df_final[,-c(1:2)], function(x) list(quantile(x, c(0.00125,0.99875), na.rm = T))))
j=1
for (i in 3:ncol(df_final)) {
  low_index <- which(df_final[,i] < xx[1,j])
  df_final[low_index,i] <- xx[1,j]
  high_index <- which(df_final[,i] > xx[2,j])
  df_final[high_index,i] <- xx[2,j]
  j = j+1
}

# scale numeric variables
df_final[,c(3:ncol(df_final))] = data.frame(sapply(df_final[,c(3:ncol(df_final))], function(x) scale(x, center = T, scale = T)))

# Fix measure directionality
names(df_final)
indices_require_fix <- c(3:25,38,41,48:60)
#indices_require_fix <- c(3:25,38,41,46:48,54:66)
df_final[,indices_require_fix] <- data.frame(sapply(df_final[,indices_require_fix], function(x) -x))

###########################################
                                          #
final_data <- df_final # FINAL DATA SET   #
                                          #
###########################################


# MODEL SELECTION

# 1- DECISION TREE
library(rpart)
library(rpart.plot)
library(caret)

set.seed(111)
split.indices <- sample(nrow(final_data), nrow(final_data)*0.80, replace = F)
train <- final_data[split.indices, ]
test <- final_data[-split.indices, ]
train$Hospital.overall.rating <- factor(train$Hospital.overall.rating)
test$Hospital.overall.rating <- factor(test$Hospital.overall.rating)

<<<<<<< HEAD
=======

>>>>>>> re-commit
# set the number of folds in cross test to 5
tree.control = trainControl(method = "cv", number = 5)

# set the search space for CP
tree.grid = expand.grid(cp = seq(0, 0.02, 0.0025))

# train model
tree.model <- train(Hospital.overall.rating ~ .,
                    data = train[,!names(train) %in% c("Provider.ID")],
                    method = "rpart",
                    metric = "Accuracy",
                    trControl = tree.control,
                    tuneGrid = tree.grid,
                    control = rpart.control(minsplit = 50,
                                            minbucket = 20))

# make predictions on the test set
tree.predict <- predict.train(tree.model, test)

# evaluate the results
confusionMatrix(tree.predict, test$Hospital.overall.rating)  # Accuracy = 0.68

# plot CP vs Accuracy
library(ggplot2)
accuracy_graph <- data.frame(tree.model$results)
ggplot(data = accuracy_graph, aes(x = cp, y = Accuracy*100)) +
  geom_line() +
  geom_point() +
  labs(x = "Complexity Parameter (CP)", y = "Accuracy", title = "CP vs Accuracy")


# 2- LOGISTIC REGRESSION
library(nnet)

set.seed(222)
split.indices <- sample(nrow(final_data), nrow(final_data)*0.80, replace = F)
train <- final_data[split.indices, ]
test <- final_data[-split.indices, ]
train$Hospital.overall.rating <- factor(train$Hospital.overall.rating)
test$Hospital.overall.rating <- factor(test$Hospital.overall.rating)


summary(train$Hospital.overall.rating)
model <- multinom(Hospital.overall.rating ~ . , data = train[,!names(train) %in% c("Provider.ID")])

testPred <- predict(model, test[,!names(test) %in% c("Provider.ID")], type = "class")

table(factor(test$Hospital.overall.rating), testPred)
confusionMatrix(test$Hospital.overall.rating, testPred) # Accuracy = 87.7%


# Part 1(2) - Random Forest
library(randomForest)
library(caret)
library(e1071)
library(Metrics)

str(final_data)
final_data$Hospital.overall.rating <- as.character(final_data$Hospital.overall.rating)
final_data$Hospital.overall.rating <- as.factor(final_data$Hospital.overall.rating)


set.seed(111)
#shuffledata <- final_data[sample(nrow(final_data)), ]
shuffledata <- final_data


ntrain = as.integer(nrow(shuffledata)*0.75)
traindata = shuffledata[1:ntrain,-1]
testdata = shuffledata[(ntrain+1):nrow(shuffledata),-1]
traindata$Hospital.overall.rating <- factor(traindata$Hospital.overall.rating)
testdata$Hospital.overall.rating <- factor(testdata$Hospital.overall.rating)

#names(traindata)
control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"
tunegrid <- expand.grid(mtry=c(1:50))

<<<<<<< HEAD
# Commenting the tuning parameter code since it takes longer
# Proceeding with mtry = 35 as it yielded best OOB during trials

=======
>>>>>>> re-commit
#rating.cv <- train(Hospital.overall.rating ~ .,data = traindata, metric = metric,
#                   method = "rf", tuneGrid = tunegrid, trControl = control) # mtry = 41

#print(rating.cv)
#plot(rating.cv)

set.seed(222)
rating.rf <- randomForest(Hospital.overall.rating ~ ., data = traindata, importance = T, 
             proximity = F,ntree=500, mtry = 35, do.trace=T
              )

print(rating.rf)

round(importance(rating.rf),2)

# Most Important Predictor Variables
varImp(rating.rf, scale = T)
varImpPlot(rating.rf, sort = T, main = "Important Variables from RF", n.var = 5)


# Prediction with testdata
testPred = predict(rating.rf, testdata)
table(testPred)
table(testdata$Hospital.overall.rating)
confusionMatrix(testPred, factor(testdata$Hospital.overall.rating)) # Accuracy = 81.5%

#=========!!!Factor Analysis Starts Here!!!=========#
# Winsorize
winsorize = function(x) {
  x = x[,-c(1,2)]
  for (i in names(x)) {
    index_lower <- which(x[[i]] < -3)
    x[index_lower,i] <- -3
    index_upper <- which(x[[i]] > 3)
    x[index_upper, i] <- 3
  }
  x = cbind(df_final[,c(1,2)],x)
  #names(x[1]) <- "Provider.ID"
  return(x)
}
shuffledata <- winsorize(shuffledata)


# Measure Groups
dim(shuffledata)
names(shuffledata)
mortality <- shuffledata[,c(3:9)] # 7 measures
safety_of_care <- shuffledata[,c(10:17)] # 8 measures
readmission <- shuffledata[,c(18:25)] # 8 measures
patient_experience <- shuffledata[,c(26:36)] # 11 measures
effective_care <- shuffledata[,c(37:48)] # 12 measures
timely_care <- shuffledata[,c(49:54)] # 6 measures
efficient_imaging <- shuffledata[,c(55:60)] # 6 measures



# MORTALITY
library(corrplot)
library(stats)
library(psych)

# basic statistics
R = cor(mortality, use = "complete.obs")
lambda = eigen(R)$values
v = eigen(R)$vectors
P = cumsum(lambda)/sum(diag(R)) # 62% of variance explained by 3 factors
corrplot(R)

# let's run factor analysis for factors = 3
fa1_factanal <- factanal(mortality, factors = 3, rotation = "varimax", scores = "regression")
print(fa1_factanal, sort = T, digits = 2, cutoff = 0.2) # p-value > 0.05, so we fail to reject null hypothesis

# calculate loadings of each measure in grp
D = as.data.frame(fa1_factanal$loadings[,1:ncol(fa1_factanal$loadings)])
DD=data.frame()
for (i in 1:ncol(D)) {
  for (j in 1:nrow(D)) {
    DD[j,i] = round(D[j,i]/sum(D[i]),2)
  }
}
DD=as.matrix(DD)

plot(fa1_factanal$loadings, type = "n")
text(fa1_factanal$loadings, labels = names(mortality), cex = 0.7)


# get the single score for each hospital
cc = as.data.frame(fa1_factanal$scores %*% t(DD))
lvm_mortality = mortality
lvm_mortality$MORT_SCORE = rowSums(cc[,1:ncol(mortality)])

# get the weights of each measure in the grp
fa1_fa <- fa(mortality, nfactors = 3, scores = "regression")
fa1_fa$weights

# SAFETY_OF_CARE
# basic statistics
R = cor(safety_of_care)
lambda = eigen(R)$values
v = eigen(R)$vectors
P = cumsum(lambda)/sum(diag(R)) # 60% of variance explained by 4 factors
corrplot(R)

# let's run factor analysis for factors = 4
fa2_factanal <- factanal(safety_of_care, factors = 4, rotation = "varimax", scores = "regression")
print(fa2_factanal, sort = T, digits = 2, cutoff = 0.2) # p-value > 0.05, so we fail to reject null hypothesis

# calculate loadings of each measure in grp
D = as.data.frame(fa2_factanal$loadings[,1:ncol(fa2_factanal$loadings)])
DD=data.frame()
for (i in 1:ncol(D)) {
  for (j in 1:nrow(D)) {
    DD[j,i] = round(D[j,i]/sum(D[i]),2)
  }
}
DD=as.matrix(DD)

plot(fa2_factanal$loadings, type = "n")
text(fa2_factanal$loadings, labels = names(safety_of_care), cex = 0.7)

# get the single score for each hospital
cc = as.data.frame(fa2_factanal$scores %*% t(DD))
lvm_safetyofcare = safety_of_care
lvm_safetyofcare$SAFETYOFCARE_SCORE = rowSums(cc[,1:ncol(safety_of_care)])


# get the weights of each measure in the grp
fa2_fa <- fa(safety_of_care, nfactors = 4, scores = "regression")
fa2_fa$weights

# READMISSION
# basic statistics
R = cor(readmission)
lambda = eigen(R)$values
v = eigen(R)$vectors
P = cumsum(lambda)/sum(diag(R)) # 74% of variance explained by 4 factors
corrplot(R)

# let's run factor analysis for factors = 4
fa3_factanal <- factanal(readmission, factors = 4, rotation = "varimax", scores = "regression")
print(fa3_factanal, sort = T, digits = 2, cutoff = 0.2) # p-value > 0.05, so we fail to reject null hypothesis

# calculate loadings of each measure in grp
D = as.data.frame(fa3_factanal$loadings[,1:ncol(fa3_factanal$loadings)])
DD=data.frame()
for (i in 1:ncol(D)) {
  for (j in 1:nrow(D)) {
    DD[j,i] = round(D[j,i]/sum(D[i]),2)
  }
}
DD=as.matrix(DD)


plot(fa3_factanal$loadings, type = "n")
text(fa3_factanal$loadings, labels = names(readmission), cex = 0.7)

# get the single score for each hospital
cc = as.data.frame(fa3_factanal$scores %*% t(DD))
lvm_readmission = readmission
lvm_readmission$READMISSION_SCORE = rowSums(cc[,1:ncol(readmission)])

# get the weights of each measure in the grp
fa3_fa <- fa(safety_of_care, nfactors = 4, scores = "regression")
fa3_fa$weights

# PATIENT EXPERIENCE
# basic statistics
R = cor(patient_experience)
lambda = eigen(R)$values
v = eigen(R)$vectors
P = cumsum(lambda)/sum(diag(R)) # 92% of variance explained by 6 factors
corrplot(R)

# let's run factor analysis for factors = 6
fa4_factanal <- factanal(patient_experience, factors = 6, rotation = "varimax", scores = "regression")
print(fa4_factanal, sort = T, digits = 2, cutoff = 0.2) # p-value > 0.05, so we fail to reject null hypothesis

# calculate weights of each measure in grp
D = as.data.frame(fa4_factanal$loadings[,1:ncol(fa4_factanal$loadings)])
DD=data.frame()
for (i in 1:ncol(D)) {
  for (j in 1:nrow(D)) {
    DD[j,i] = round(D[j,i]/sum(D[i]),2)
  }
}
DD=as.matrix(DD)


plot(fa4_factanal$loadings, type = "n")
text(fa4_factanal$loadings, labels = names(patient_experience), cex = 0.7)

# get the single score for each hospital
cc = as.data.frame(fa4_factanal$scores %*% t(DD))
lvm_patientexperience = patient_experience
lvm_patientexperience$PATIENTEXPERIENCE_SCORE = rowSums(cc[,1:ncol(patient_experience)])


# get the weights of each measure in the grp
fa4_fa <- fa(patient_experience, nfactors = 6, scores = "regression")
fa4_fa$weights

# EFFECTIVE_CARE
# basic statistics
R = cor(effective_care)
lambda = eigen(R)$values
v = eigen(R)$vectors
P = cumsum(lambda)/sum(diag(R)) # 64.5% of variance explained by 6 factors
corrplot(R)

# let's run factor analysis for factors = 6
fa5_factanal <- factanal(effective_care, factors = 6, rotation = "varimax", scores = "regression")
print(fa5_factanal, sort = T, digits = 2, cutoff = 0.2) # p-value > 0.05, so we fail to reject null hypothesis

# calculate weights of each measure in grp
D = as.data.frame(fa5_factanal$loadings[,1:ncol(fa5_factanal$loadings)])
DD=data.frame()
for (i in 1:ncol(D)) {
  for (j in 1:nrow(D)) {
    DD[j,i] = round(D[j,i]/sum(D[i]),2)
  }
}
DD=as.matrix(DD)


plot(fa5_factanal$loadings, type = "n")
text(fa5_factanal$loadings, labels = names(effective_care), cex = 0.7)

# get the single score for each hospital
cc = as.data.frame(fa5_factanal$scores %*% t(DD))
lvm_effectivecare = effective_care
lvm_effectivecare$EFFECTIVECARE_SCORE = rowSums(cc[,1:ncol(effective_care)])

# get the weights of each measure in the grp
fa5_fa <- fa(effective_care, nfactors = 6, scores = "regression")
fa5_fa$weights


# TIMELY_CARE
# basic statistics
R = cor(timely_care)
lambda = eigen(R)$values
v = eigen(R)$vectors
P = cumsum(lambda)/sum(diag(R)) # 84% of variance explained by 3 factors
corrplot(R)

# let's run factor analysis for factors = 3
fa6_factanal <- factanal(timely_care, factors = 3, rotation = "varimax", scores = "regression")
print(fa6_factanal, sort = T, digits = 2, cutoff = 0.2) # p-value > 0.05, so we fail to reject null hypothesis

# calculate weights of each measure in grp
D = as.data.frame(fa6_factanal$loadings[,1:ncol(fa6_factanal$loadings)])
DD=data.frame()
for (i in 1:ncol(D)) {
  for (j in 1:nrow(D)) {
    DD[j,i] = round(D[j,i]/sum(D[i]),2)
  }
}
DD=as.matrix(DD)


plot(fa6_factanal$loadings, type = "n")
text(fa6_factanal$loadings, labels = names(timely_care), cex = 0.7)

# get the single score for each hospital
cc = as.data.frame(fa6_factanal$scores %*% t(DD))
lvm_timelycare = timely_care
lvm_timelycare$TIMELYCARE_SCORE = rowSums(cc[,1:ncol(timely_care)])


# get the weights of each measure in the grp
fa6_fa <- fa(timely_care, nfactors = 3, scores = "regression")
fa6_fa$weights

# EFFICIENT_IMAGING
# basic statistics
R = cor(efficient_imaging)
lambda = eigen(R)$values
v = eigen(R)$vectors
P = cumsum(lambda)/sum(diag(R)) # 60% of variance explained by 3 factors
corrplot(R)

# let's run factor analysis for factors = 3
fa7_factanal <- factanal(efficient_imaging, factors = 3, rotation = "varimax", scores = "regression")
print(fa7_factanal, sort = T, digits = 2, cutoff = 0.2) # p-value > 0.05, so we fail to reject null hypothesis

# calculate weights of each measure in grp
D = as.data.frame(fa7_factanal$loadings[,1:ncol(fa7_factanal$loadings)])
DD=data.frame()
for (i in 1:ncol(D)) {
  for (j in 1:nrow(D)) {
    DD[j,i] = round(D[j,i]/sum(D[i]),2)
  }
}
DD=as.matrix(DD)


plot(fa7_factanal$loadings, type = "n")
text(fa7_factanal$loadings, labels = names(timely_care), cex = 0.7)

# get the single score for each hospital
cc = as.data.frame(fa7_factanal$scores %*% t(DD))
lvm_efficientimaging = efficient_imaging
lvm_efficientimaging$EFFICIENTIMAGING_SCORE = rowSums(cc[,1:ncol(efficient_imaging)])


# get the weights of each measure in the grp
fa7_fa <- fa(efficient_imaging, nfactors = 3, scores = "regression")
fa7_fa$weights

# TOTAL SCORE
df_score <- data.frame(cbind(shuffledata[,1], lvm_mortality$MORT_SCORE, lvm_safetyofcare$SAFETYOFCARE_SCORE,
                             lvm_readmission$READMISSION_SCORE, lvm_patientexperience$PATIENTEXPERIENCE_SCORE,
                             lvm_effectivecare$EFFECTIVECARE_SCORE,
                             lvm_timelycare$TIMELYCARE_SCORE, lvm_efficientimaging$EFFICIENTIMAGING_SCORE))
colnames(df_score) <- c("Provider.ID","MORT_SCORE", "SAFETYOFCARE_SCORE", "READMISSION_SCORE",
                        "PATIENTEXPERIENCE_SCORE", "EFFECTIVECARE_SCORE", "TIMELYCARE_SCORE",
                        "EFFICIENTIMAGING_SCORE")
df_score[,c(2:8)] <- data.frame(sapply(df_score[,c(2:8)], function(x) as.numeric(as.character(x))))
df_score[,1] <- as.character(df_score[,1])


# WEIGHTED SCORES
df_score$MORT_SCORE <- 0.22*df_score$MORT_SCORE
df_score$SAFETYOFCARE_SCORE <- 0.22*df_score$SAFETYOFCARE_SCORE
df_score$READMISSION_SCORE <- 0.22*df_score$READMISSION_SCORE
df_score$PATIENTEXPERIENCE_SCORE <- 0.22*df_score$PATIENTEXPERIENCE_SCORE
df_score$EFFECTIVECARE_SCORE <- 0.04*df_score$EFFECTIVECARE_SCORE
df_score$TIMELYCARE_SCORE <- 0.04*df_score$TIMELYCARE_SCORE
df_score$EFFICIENTIMAGING_SCORE <- 0.04*df_score$EFFICIENTIMAGING_SCORE


# TOTAL SCORE
df_score <- mutate(df_score, TOTAL_SCORE=rowSums(df_score[,2:8]))

# Winsorization of TOTAL_SCORE
summary(df_score$TOTAL_SCORE)
quantile(df_score$TOTAL_SCORE, seq(0,1,0.005))
yy = quantile(df_score$TOTAL_SCORE, c(0.005))[[1]]
zz = quantile(df_score$TOTAL_SCORE, c(0.995))[[1]]
df_score$TOTAL_SCORE[which(df_score$TOTAL_SCORE < yy)] <- yy
df_score$TOTAL_SCORE[which(df_score$TOTAL_SCORE > zz)] <- zz

# CLUSTERING

set.seed(11)
clus <- kmeans(df_score$TOTAL_SCORE, centers = 5, nstart = 50, iter.max = 50)
clus # 92.5% of total variance can be explained 

table(factor(shuffledata$Hospital.overall.rating), clus$cluster)
names(df_score)
df_score <- cbind(df_score, clus$cluster)
colnames(df_score)[10] <- "Cluster.ID"

names(df_score)
# ASSIGN CORRECT STAR RATINGS
bb = boxplot(TOTAL_SCORE~Cluster.ID, data = df_score)
d = data.frame(sapply(data.frame(bb$conf), function(x) mean(x)))
d
names(d) <- c("Mean")
e = as.vector(order(d$Mean, decreasing = F))
e
rating_1 <- filter(df_score, df_score$Cluster.ID==e[1])
rating_1$Cluster.ID <- '1'

rating_2 <- filter(df_score, df_score$Cluster.ID==e[2])
rating_2$Cluster.ID <- '2'

rating_3 <- filter(df_score, df_score$Cluster.ID==e[3])
rating_3$Cluster.ID <- '3'

rating_4 <- filter(df_score, df_score$Cluster.ID==e[4])
rating_4$Cluster.ID <- '4'

rating_5 <- filter(df_score, df_score$Cluster.ID==e[5])
rating_5$Cluster.ID <- '5'

FINAL_MODEL <- rbind(rating_1, rating_2, rating_3, rating_4, rating_5)
FINAL_MODEL <- FINAL_MODEL[order(as.numeric(FINAL_MODEL$Provider.ID), decreasing = F),]

boxplot(TOTAL_SCORE~Cluster.ID, data = FINAL_MODEL)

confusionMatrix(factor(shuffledata$Hospital.overall.rating), factor(FINAL_MODEL$Cluster.ID))

# Accuracy = 51%

# MODEL EVALUATION

# plot the rating distribution - original(shuffledata), derived(FINAL_MODEL)
library(graphics)

table(final_data$Hospital.overall.rating)
A <- round(100*length(which(final_data$Hospital.overall.rating=='1'))/nrow(final_data),0)
B <- round(100*length(which(final_data$Hospital.overall.rating=='2'))/nrow(final_data),0)
C <- round(100*length(which(final_data$Hospital.overall.rating=='3'))/nrow(final_data),0)
D <- round(100*length(which(final_data$Hospital.overall.rating=='4'))/nrow(final_data),0)
E <- round(100*length(which(final_data$Hospital.overall.rating=='5'))/nrow(final_data),0)
p <- c(1:5)
q <- c(A,B,C,D,E)
M <- as.data.frame(as.matrix(table(final_data$Hospital.overall.rating)))
X <- NULL
X <- data.frame(Number = p, Percent = q)
X <- cbind(X,M$V1)
names(X)[3] <- "Count"
X$Percent <- paste(X$Percent,"%",sep = "")

table(factor(shuffledata$Hospital.overall.rating), FINAL_MODEL$Cluster.ID)
G <- round(100*length(which(FINAL_MODEL$Cluster.ID=='1'))/nrow(FINAL_MODEL),0)
H <- round(100*length(which(FINAL_MODEL$Cluster.ID=='2'))/nrow(FINAL_MODEL),0)
I <- round(100*length(which(FINAL_MODEL$Cluster.ID=='3'))/nrow(FINAL_MODEL),0)
J <- round(100*length(which(FINAL_MODEL$Cluster.ID=='4'))/nrow(FINAL_MODEL),0)
K <- round(100*length(which(FINAL_MODEL$Cluster.ID=='5'))/nrow(FINAL_MODEL),0)
s <- c(1:5)
t <- c(G,H,I,J,K)
N <- as.data.frame(as.matrix(table(FINAL_MODEL$Cluster.ID)))
Y <- NULL
Y <- data.frame(Number = s, Percent = t)
Y <- cbind(Y,N$V1)
names(Y)[3] <- "Count"
Y$Percent <- paste(Y$Percent,"%",sep = "")

# Plot now
par(mfrow = c(1:2))

my_bar_1 <- barplot(table(final_data$Hospital.overall.rating), col = rainbow(20),xlab = "Star Rating", 
                    ylab = "Count of Hospitals", main = "Number of Hospitals With Star Ratings - Original", cex.main = 0.7, cex.names = 0.7)
text(my_bar_1, 2,6,labels = paste(X$Count,X$Percent,sep = ","),cex = 0.55,pos = 3)

my_bar_2 <- barplot(table(FINAL_MODEL$Cluster.ID), col = rainbow(20),xlab = "Star Rating", 
                    ylab = "Count of Hospitals", main = "Number of Hospitals With Star Ratings - Derived", cex.main = 0.7, cex.names = 0.7)
text(my_bar_2, 2,6,labels = paste(Y$Count,Y$Percent,sep = ","),cex = 0.55,pos = 3)



# Provider Analysis
evanston <- subset(df_final_trial, df_final_trial$Provider.ID=='140010')
rating_four <- subset(df_final_trial, df_final_trial$Hospital.overall.rating=='4')
dummy <- data.frame(sapply(rating_four[,-c(1:2)], function(x) mean(x, na.rm = T)))
dummy <- as.data.frame(t(dummy))
compare <- rbind(evanston[,-c(1:2)], dummy)
rownames(compare) <- c("Original", 'Recommended')
compare[is.na(compare)] <- 0

# Plot graphs for each grp
library(ggplot2)
library(cowplot)
names(compare)

mortality_grp <- compare[,c(1:7)]
names(mortality_grp)
mortality_grp <- as.data.frame(sapply(mortality_grp, function(x) round(x,2)))
rownames(mortality_grp) <- c("Orig", 'Recom')
plot_grid(ggplot(mortality_grp, aes(rownames(mortality_grp), mortality_grp$PSI_4_SURG_COMP, fill = rownames(mortality_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = mortality_grp$PSI_4_SURG_COMP), colour = "black", vjust = 1.5) + labs(x="Type", y="PSI_4_SURG_COMP", fill = "Type"),
          ggplot(mortality_grp, aes(rownames(mortality_grp), mortality_grp$MORT_30_AMI, fill = rownames(mortality_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = mortality_grp$MORT_30_AMI), colour = "black", vjust = 1.5) + labs(x="Type", y="MORT_30_AMI", fill = "Type"),
          ggplot(mortality_grp, aes(rownames(mortality_grp), mortality_grp$MORT_30_CABG, fill = rownames(mortality_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = mortality_grp$MORT_30_CABG), colour = "black", vjust = 1.5) + labs(x="Type", y="MORT_30_CABG", fill = "Type"),
          ggplot(mortality_grp, aes(rownames(mortality_grp), mortality_grp$MORT_30_COPD, fill = rownames(mortality_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = mortality_grp$MORT_30_COPD), colour = "black", vjust = 1.5) + labs(x="Type", y="MORT_30_COPD", fill = "Type"),
          ggplot(mortality_grp, aes(rownames(mortality_grp), mortality_grp$MORT_30_HF, fill = rownames(mortality_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = mortality_grp$MORT_30_HF), colour = "black", vjust = 1.5) + labs(x="Type", y="MORT_30_HF", fill = "Type"),
          ggplot(mortality_grp, aes(rownames(mortality_grp), mortality_grp$MORT_30_PN, fill = rownames(mortality_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = mortality_grp$MORT_30_PN), colour = "black", vjust = 1.5) + labs(x="Type", y="MORT_30_PN", fill = "Type"),
          ggplot(mortality_grp, aes(rownames(mortality_grp), mortality_grp$MORT_30_STK, fill = rownames(mortality_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = mortality_grp$MORT_30_STK), colour = "black", vjust = 1.5) + labs(x="Type", y="MORT_30_STK", fill = "Type"),
          scale = 0.8 ,ncol = 4,labels = "MORTALITY", label_size = 12, label_x = 1.8
)

# Recommendation - Mortality figures better than mean value of rating 4 hospital. 
# No recommendation in this grp

safetyofcare_grp <- compare[,c(8:15)]
names(safetyofcare_grp)
safetyofcare_grp <- as.data.frame(sapply(safetyofcare_grp, function(x) round(x,2)))
rownames(safetyofcare_grp) <- c("Orig", 'Recom')
plot_grid(ggplot(safetyofcare_grp, aes(rownames(safetyofcare_grp), safetyofcare_grp$COMP_HIP_KNEE, fill = rownames(safetyofcare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = safetyofcare_grp$COMP_HIP_KNEE), colour = "black", vjust = 1.5) + labs(x="Type", y="COMP_HIP_KNEE", fill = "Type"),
          ggplot(safetyofcare_grp, aes(rownames(safetyofcare_grp), safetyofcare_grp$PSI_90_SAFETY, fill = rownames(safetyofcare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = safetyofcare_grp$PSI_90_SAFETY), colour = "black", vjust = 1.5) + labs(x="Type", y="PSI_90_SAFETY", fill = "Type"),
          ggplot(safetyofcare_grp, aes(rownames(safetyofcare_grp), safetyofcare_grp$HAI_1_SIR, fill = rownames(safetyofcare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = safetyofcare_grp$HAI_1_SIR), colour = "black", vjust = 1.5) + labs(x="Type", y="HAI_1_SIR", fill = "Type"),
          ggplot(safetyofcare_grp, aes(rownames(safetyofcare_grp), safetyofcare_grp$HAI_2_SIR, fill = rownames(safetyofcare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = safetyofcare_grp$HAI_2_SIR), colour = "black", vjust = 1.5) + labs(x="Type", y="HAI_2_SIR", fill = "Type"),
          ggplot(safetyofcare_grp, aes(rownames(safetyofcare_grp), safetyofcare_grp$HAI_3_SIR, fill = rownames(safetyofcare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = safetyofcare_grp$HAI_3_SIR), colour = "black", vjust = 1.5) + labs(x="Type", y="HAI_3_SIR", fill = "Type"),
          ggplot(safetyofcare_grp, aes(rownames(safetyofcare_grp), safetyofcare_grp$HAI_4_SIR, fill = rownames(safetyofcare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = safetyofcare_grp$HAI_4_SIR), colour = "black", vjust = 1.5) + labs(x="Type", y="HAI_4_SIR", fill = "Type"),
          ggplot(safetyofcare_grp, aes(rownames(safetyofcare_grp), safetyofcare_grp$HAI_5_SIR, fill = rownames(safetyofcare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = safetyofcare_grp$HAI_5_SIR), colour = "black", vjust = 1.5) + labs(x="Type", y="HAI_5_SIR", fill = "Type"),
          ggplot(safetyofcare_grp, aes(rownames(safetyofcare_grp), safetyofcare_grp$HAI_6_SIR, fill = rownames(safetyofcare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = safetyofcare_grp$HAI_6_SIR), colour = "black", vjust = 1.5) + labs(x="Type", y="HAI_6_SIR", fill = "Type"),
          scale = 0.8 ,ncol = 4,labels = "SAFETY OF CARE", label_size = 12, label_x = 1.8
)

# Recommendation - All measures except 'HAI_6_SIR' exceed the mean value of 
# rating 4 hospitals. So, the hospital should improve the figures in this grp


readmission_grp <- compare[,c(16:23)]
names(readmission_grp)
readmission_grp <- as.data.frame(sapply(readmission_grp, function(x) round(x,2)))
rownames(readmission_grp) <- c("Orig", 'Recom')
plot_grid(ggplot(readmission_grp, aes(rownames(readmission_grp), readmission_grp$READM_30_AMI, fill = rownames(readmission_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = readmission_grp$READM_30_AMI), colour = "black", vjust = 1.5) + labs(x="Type", y="READM_30_AMI", fill = "Type"),
          ggplot(readmission_grp, aes(rownames(readmission_grp), readmission_grp$READM_30_CABG, fill = rownames(readmission_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = readmission_grp$READM_30_CABG), colour = "black", vjust = 1.5) + labs(x="Type", y="READM_30_CABG", fill = "Type"),
          ggplot(readmission_grp, aes(rownames(readmission_grp), readmission_grp$READM_30_COPD, fill = rownames(readmission_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = readmission_grp$READM_30_COPD), colour = "black", vjust = 1.5) + labs(x="Type", y="READM_30_COPD", fill = "Type"),
          ggplot(readmission_grp, aes(rownames(readmission_grp), readmission_grp$READM_30_HF, fill = rownames(readmission_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = readmission_grp$READM_30_HF), colour = "black", vjust = 1.5) + labs(x="Type", y="READM_30_HF", fill = "Type"),
          ggplot(readmission_grp, aes(rownames(readmission_grp), readmission_grp$READM_30_HIP_KNEE, fill = rownames(readmission_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = readmission_grp$READM_30_HIP_KNEE), colour = "black", vjust = 1.5) + labs(x="Type", y="READM_30_HIP_KNEE", fill = "Type"),
          ggplot(readmission_grp, aes(rownames(readmission_grp), readmission_grp$READM_30_HOSP_WIDE, fill = rownames(readmission_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = readmission_grp$READM_30_HOSP_WIDE), colour = "black", vjust = 1.5) + labs(x="Type", y="READM_30_HOSP_WIDE", fill = "Type"),
          ggplot(readmission_grp, aes(rownames(readmission_grp), readmission_grp$READM_30_PN, fill = rownames(readmission_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = readmission_grp$READM_30_PN), colour = "black", vjust = 1.5) + labs(x="Type", y="READM_30_PN", fill = "Type"),
          ggplot(readmission_grp, aes(rownames(readmission_grp), readmission_grp$READM_30_STK, fill = rownames(readmission_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = readmission_grp$READM_30_STK), colour = "black", vjust = 1.5) + labs(x="Type", y="READM_30_STK", fill = "Type"),
          scale = 0.8 ,ncol = 4,labels = "READMISSION", label_size = 12, label_x = 1.8
)

# Recommendation - The measures in this grp except READM_30_HIP_KNEE are marginally more 
# than rating 4 hospitals. So, the hospital has some scope to improve in this grp

patientexperience_grp <- compare[,c(24:34)]
names(patientexperience_grp)
patientexperience_grp <- as.data.frame(sapply(patientexperience_grp, function(x) round(x,2)))
rownames(patientexperience_grp) <- c("Orig", 'Recom')
plot_grid(ggplot(patientexperience_grp, aes(rownames(patientexperience_grp), patientexperience_grp$H_CLEAN_LINEAR_SCORE, fill = rownames(patientexperience_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = patientexperience_grp$H_CLEAN_LINEAR_SCORE), colour = "black", vjust = 1.5) + labs(x="Type", y="H_CLEAN_LINEAR_SCORE", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(patientexperience_grp, aes(rownames(patientexperience_grp), patientexperience_grp$H_COMP_1_LINEAR_SCORE, fill = rownames(patientexperience_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = patientexperience_grp$H_COMP_1_LINEAR_SCORE), colour = "black", vjust = 1.5) + labs(x="Type", y="H_COMP_1_LINEAR_SCORE", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(patientexperience_grp, aes(rownames(patientexperience_grp), patientexperience_grp$H_COMP_2_LINEAR_SCORE, fill = rownames(patientexperience_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = patientexperience_grp$H_COMP_2_LINEAR_SCORE), colour = "black", vjust = 1.5) + labs(x="Type", y="H_COMP_2_LINEAR_SCORE", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(patientexperience_grp, aes(rownames(patientexperience_grp), patientexperience_grp$H_COMP_3_LINEAR_SCORE, fill = rownames(patientexperience_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = patientexperience_grp$H_COMP_3_LINEAR_SCORE), colour = "black", vjust = 1.5) + labs(x="Type", y="H_COMP_3_LINEAR_SCORE", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(patientexperience_grp, aes(rownames(patientexperience_grp), patientexperience_grp$H_COMP_4_LINEAR_SCORE, fill = rownames(patientexperience_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = patientexperience_grp$H_COMP_4_LINEAR_SCORE), colour = "black", vjust = 1.5) + labs(x="Type", y="H_COMP_4_LINEAR_SCORE", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(patientexperience_grp, aes(rownames(patientexperience_grp), patientexperience_grp$H_COMP_5_LINEAR_SCORE, fill = rownames(patientexperience_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = patientexperience_grp$H_COMP_5_LINEAR_SCORE), colour = "black", vjust = 1.5) + labs(x="Type", y="H_COMP_5_LINEAR_SCORE", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(patientexperience_grp, aes(rownames(patientexperience_grp), patientexperience_grp$H_COMP_6_LINEAR_SCORE, fill = rownames(patientexperience_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = patientexperience_grp$H_COMP_6_LINEAR_SCORE), colour = "black", vjust = 1.5) + labs(x="Type", y="H_COMP_6_LINEAR_SCORE", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(patientexperience_grp, aes(rownames(patientexperience_grp), patientexperience_grp$H_COMP_7_LINEAR_SCORE, fill = rownames(patientexperience_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = patientexperience_grp$H_COMP_7_LINEAR_SCORE), colour = "black", vjust = 1.5) + labs(x="Type", y="H_COMP_7_LINEAR_SCORE", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(patientexperience_grp, aes(rownames(patientexperience_grp), patientexperience_grp$H_HSP_RATING_LINEAR_SCORE, fill = rownames(patientexperience_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = patientexperience_grp$H_HSP_RATING_LINEAR_SCORE), colour = "black", vjust = 1.5) + labs(x="Type", y="H_HSP_RATING_LINEAR_SCORE", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(patientexperience_grp, aes(rownames(patientexperience_grp), patientexperience_grp$H_QUIET_LINEAR_SCORE, fill = rownames(patientexperience_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = patientexperience_grp$H_QUIET_LINEAR_SCORE), colour = "black", vjust = 1.5) + labs(x="Type", y="H_QUIET_LINEAR_SCORE", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(patientexperience_grp, aes(rownames(patientexperience_grp), patientexperience_grp$H_RECMND_LINEAR_SCORE, fill = rownames(patientexperience_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = patientexperience_grp$H_RECMND_LINEAR_SCORE), colour = "black", vjust = 1.5) + labs(x="Type", y="H_RECMND_LINEAR_SCORE", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          scale = 0.8 ,ncol = 4,labels = "PATIENT EXPERIENCE", label_size = 12, label_x = 1.8
)

# Recommendation - The scores for all measures are less compared to rating 4 hospitals. 
# So, the hospital must improve on patient experience measures especially in staff 
# responsiveness (H_COMP_3) and discharge information (H_COMP_6)

names(compare)
effectivecare_grp <- compare[,c(35:46)]
names(effectivecare_grp)
effectivecare_grp <- as.data.frame(sapply(effectivecare_grp, function(x) round(x,2)))
rownames(effectivecare_grp) <- c("Orig", 'Recom')
plot_grid(ggplot(effectivecare_grp, aes(rownames(effectivecare_grp), effectivecare_grp$IMM_2, fill = rownames(effectivecare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = effectivecare_grp$IMM_2), colour = "black", vjust = 1.5) + labs(x="Type", y="IMM_2", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(effectivecare_grp, aes(rownames(effectivecare_grp), effectivecare_grp$OP_22, fill = rownames(effectivecare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = effectivecare_grp$OP_22), colour = "black", vjust = 1.5) + labs(x="Type", y="OP_22", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(effectivecare_grp, aes(rownames(effectivecare_grp), effectivecare_grp$OP_23, fill = rownames(effectivecare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = effectivecare_grp$OP_23), colour = "black", vjust = 1.5) + labs(x="Type", y="OP_23", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(effectivecare_grp, aes(rownames(effectivecare_grp), effectivecare_grp$OP_30, fill = rownames(effectivecare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = effectivecare_grp$OP_30), colour = "black", vjust = 1.5) + labs(x="Type", y="OP_30", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(effectivecare_grp, aes(rownames(effectivecare_grp), effectivecare_grp$PC_01, fill = rownames(effectivecare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = effectivecare_grp$PC_01), colour = "black", vjust = 1.5) + labs(x="Type", y="PC_01", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(effectivecare_grp, aes(rownames(effectivecare_grp), effectivecare_grp$STK_1, fill = rownames(effectivecare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = effectivecare_grp$STK_1), colour = "black", vjust = 1.5) + labs(x="Type", y="STK_1", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(effectivecare_grp, aes(rownames(effectivecare_grp), effectivecare_grp$STK_4, fill = rownames(effectivecare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = effectivecare_grp$STK_4), colour = "black", vjust = 1.5) + labs(x="Type", y="STK_4", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(effectivecare_grp, aes(rownames(effectivecare_grp), effectivecare_grp$STK_5, fill = rownames(effectivecare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = effectivecare_grp$STK_5), colour = "black", vjust = 1.5) + labs(x="Type", y="STK_5", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(effectivecare_grp, aes(rownames(effectivecare_grp), effectivecare_grp$VTE_1, fill = rownames(effectivecare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = effectivecare_grp$VTE_1), colour = "black", vjust = 1.5) + labs(x="Type", y="VTE_1", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(effectivecare_grp, aes(rownames(effectivecare_grp), effectivecare_grp$VTE_2, fill = rownames(effectivecare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = effectivecare_grp$VTE_2), colour = "black", vjust = 1.5) + labs(x="Type", y="VTE_2", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(effectivecare_grp, aes(rownames(effectivecare_grp), effectivecare_grp$VTE_3, fill = rownames(effectivecare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = effectivecare_grp$VTE_3), colour = "black", vjust = 1.5) + labs(x="Type", y="VTE_3", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(effectivecare_grp, aes(rownames(effectivecare_grp), effectivecare_grp$VTE_6, fill = rownames(effectivecare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = effectivecare_grp$VTE_6), colour = "black", vjust = 1.5) + labs(x="Type", y="VTE_6", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          scale = 0.8 ,ncol = 4,labels = "EFFECTIVE CARE", label_size = 12, label_x = 1.8
)

# Recommendation - Patients who developed a blood clot while in the 
# hospital who did not get treatment that could have prevented it (VTE_6)
# that needs massive improvement


timelycare_grp <- compare[,c(47:52)]
names(timelycare_grp)
timelycare_grp <- as.data.frame(sapply(timelycare_grp, function(x) round(x,2)))
rownames(timelycare_grp) <- c("Orig", 'Recom')
plot_grid(ggplot(timelycare_grp, aes(rownames(timelycare_grp), timelycare_grp$ED_1b, fill = rownames(timelycare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = timelycare_grp$ED_1b), colour = "black", vjust = 1.5) + labs(x="Type", y="ED_1b", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(timelycare_grp, aes(rownames(timelycare_grp), timelycare_grp$ED_2b, fill = rownames(timelycare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = timelycare_grp$ED_2b), colour = "black", vjust = 1.5) + labs(x="Type", y="ED_2b", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(timelycare_grp, aes(rownames(timelycare_grp), timelycare_grp$OP_18b, fill = rownames(timelycare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = timelycare_grp$OP_18b), colour = "black", vjust = 1.5) + labs(x="Type", y="OP_18b", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(timelycare_grp, aes(rownames(timelycare_grp), timelycare_grp$OP_20, fill = rownames(timelycare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = timelycare_grp$OP_20), colour = "black", vjust = 1.5) + labs(x="Type", y="OP_20", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(timelycare_grp, aes(rownames(timelycare_grp), timelycare_grp$OP_21, fill = rownames(timelycare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = timelycare_grp$OP_21), colour = "black", vjust = 1.5) + labs(x="Type", y="OP_21", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(timelycare_grp, aes(rownames(timelycare_grp), timelycare_grp$OP_5, fill = rownames(timelycare_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = timelycare_grp$OP_5), colour = "black", vjust = 1.5) + labs(x="Type", y="OP_5", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          scale = 0.8 ,ncol = 4,labels = "TIMELY CARE", label_size = 12, label_x = 1.8
)
          
# Recommendation - OP_18b, OP_20, OP_21 need improvement

outpatientimaging_grp <- compare[,c(53:58)]
names(outpatientimaging_grp)
outpatientimaging_grp <- as.data.frame(sapply(outpatientimaging_grp, function(x) round(x,2)))
rownames(outpatientimaging_grp) <- c("Orig", 'Recom')
plot_grid(ggplot(outpatientimaging_grp, aes(rownames(outpatientimaging_grp), outpatientimaging_grp$OP_10, fill = rownames(outpatientimaging_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = outpatientimaging_grp$OP_10), colour = "black", vjust = 1.5) + labs(x="Type", y="OP_10", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(outpatientimaging_grp, aes(rownames(outpatientimaging_grp), outpatientimaging_grp$OP_11, fill = rownames(outpatientimaging_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = outpatientimaging_grp$OP_11), colour = "black", vjust = 1.5) + labs(x="Type", y="OP_11", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(outpatientimaging_grp, aes(rownames(outpatientimaging_grp), outpatientimaging_grp$OP_13, fill = rownames(outpatientimaging_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = outpatientimaging_grp$OP_13), colour = "black", vjust = 1.5) + labs(x="Type", y="OP_13", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(outpatientimaging_grp, aes(rownames(outpatientimaging_grp), outpatientimaging_grp$OP_14, fill = rownames(outpatientimaging_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = outpatientimaging_grp$OP_14), colour = "black", vjust = 1.5) + labs(x="Type", y="OP_14", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(outpatientimaging_grp, aes(rownames(outpatientimaging_grp), outpatientimaging_grp$OP_8, fill = rownames(outpatientimaging_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = outpatientimaging_grp$OP_8), colour = "black", vjust = 1.5) + labs(x="Type", y="OP_8", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          ggplot(outpatientimaging_grp, aes(rownames(outpatientimaging_grp), outpatientimaging_grp$OP_9, fill = rownames(outpatientimaging_grp))) + geom_bar(stat = "identity") + geom_col(position = "dodge", colour = "black") + geom_text(aes(label = outpatientimaging_grp$OP_9), colour = "black", vjust = 1.5) + labs(x="Type", y="OP_9", fill = "Type") + theme(axis.title = element_text(size = 10, hjust = 0.5, vjust = 0.5)),
          scale = 0.8 ,ncol = 4,labels = "OUTPATIENT IMAGING", label_size = 12, label_x = 1.8
 
)

# Recommendation - OP_13 needs improvement

# Procedure to derive list of KPIs that need improvement
# Identify the difference from the mean value of rating '4' hospitals
# Diff * Loading * Weight of that grp

gap=vector()
for (i in 1:ncol(compare)) {
  gap[i] = compare[2,i] - compare[1,i] # recommended - original
}

df_gap = data.frame(Measure = names(compare), Diff = gap)
df_gap

# fix the sign of Diff
# there are 4 cases now:
# 1. higher value is better (negative/ lower diff is good)
# a. if +ve, take action 
# b. if -ve, nothing to be done

# 2. lower value is better (positive/ higher diff is good)
# a. if +ve, nothing to be done
# b. if -ve, take action

# meaures for which lower value is better:
#  PSI_4_SURG_COMP, MORT_30_AMI, MORT_30_CABG, MORT_30_COPD, MORT_30_HF, MORT_30_PN, MORT_30_STK
#  COMP_HIP_KNEE, PSI_90_SAFETY, HAI_1_SIR, HAI_2_SIR, HAI_3_SIR, HAI_4_SIR, HAI_5_SIR, HAI_6_SIR
#  READM_30_AMI, READM_30_CABG, READM_30_COPD, READM_30_HF, READM_30_HIP_KNEE, READM_30_HOSP_WIDE, READM_30_PN, READM_30_STK
#  OP_22, PC_01, VTE_6
#  ED_1b, ED_2b, OP_18b, OP_20, OP_21, OP_3b, OP_5
#  OP_10, OP_11, OP_13, OP_14, OP_8, OP_9

# reverse the signs in order to align diffs of all measures
# mortality grp
df_gap[1:7,2] <- -df_gap[1:7,2]

# safety of care grp
df_gap[8:15,2] <- -df_gap[8:15,2]

# readmissions grp
df_gap[16:23,2] <- -df_gap[16:23,2]

# effective care grp
df_gap[c(36,39,46),2] <- -df_gap[c(36,39,46),2]

# timely care grp
df_gap[47:52,2] <- -df_gap[47:52,2]

# outpatient imaging grp
df_gap[53:58,2] <- -df_gap[53:58,2]

# assign groups
group <- c(rep("mortality", 7), rep("safety of care",8), rep("readmission", 8),
           rep("patient experience", 11), rep("effective care",12), 
          rep("timely care",6)  ,rep("outpatient imaging",6))
df_gap <- cbind(df_gap, group)

# add weight column
aa <- filter(df_gap, df_gap$group=="mortality")
aa$weight <- 0.22

bb <- filter(df_gap, df_gap$group=="safety of care")
bb$weight <- 0.22

cc <- filter(df_gap, df_gap$group=="readmission")
cc$weight <- 0.22

dd <- filter(df_gap, df_gap$group=="patient experience")
dd$weight <- 0.22

ee <- filter(df_gap, df_gap$group=="effective care")
ee$weight <- 0.04

ff <- filter(df_gap, df_gap$group=="timely care")
ff$weight <- 0.04

gg <- filter(df_gap, df_gap$group=="outpatient imaging")
gg$weight <- 0.04

df_gap <- rbind(aa,bb,cc,dd,ee,ff,gg)

# add loadings from factor analysis for each row
mort_load <- data.frame(rowSums(fa1_factanal$loadings))
safety_load <- data.frame(rowSums(fa2_factanal$loadings))
readmission_load <- data.frame(rowSums(fa3_factanal$loadings))
patientexp_load <- data.frame(rowSums(fa4_factanal$loadings))
effective_load <- data.frame(rowSums(fa5_factanal$loadings))
timely_load <- data.frame(rowSums(fa6_factanal$loadings))
outpatient_load <- data.frame(rowSums(fa7_factanal$loadings))


loading <- c(mort_load[,1], safety_load[,1],readmission_load[,1],
              patientexp_load[,1],effective_load[,1],timely_load[,1],
              outpatient_load[,1])

df_gap <- cbind(df_gap, loading)

# final score
df_gap <- mutate(df_gap, score = Diff*weight*loading)

# Key KPIs for consideration (higher scores are bad)
df_gap <- df_gap[order(df_gap$score, decreasing = T),]

# CONCLUSION - TOP 5 KPIs FOR IMPROVEMENT
head(df_gap$Measure) # ignore OP_23 since there are no values reported by hospital
# 1. H_COMP_5_LINEAR_SCORE
# 2. H_COMP_6_LINEAR_SCORE
# 3. OP_20
# 4. H_COMP_3_LINEAR_SCORE
# 5. OP_18b


#--------!!!END OF ANALYSIS!!!---------#
