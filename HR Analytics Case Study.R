#install.packages("GGally", dependencies = TRUE)
#install.packages("MASS", dependencies = TRUE)
#install.packages("car", dependencies = TRUE)
#install.packages("e1071", dependencies = TRUE)
#install.packages("caret", dependencies = TRUE)
#install.packages("caTools", dependencies = TRUE)
#install.packages("pROC", dependencies = TRUE)
#install.packages("ROCR", dependencies = TRUE)
library(dplyr)
library(cowplot)
library(ggplot2)
library(GGally)
library(MASS)
library(car)
library(e1071)
library(caret)
library(caTools)
library(pROC)
library(ROCR)


# AIM:
# Aim is to predict whether an employee will quit the company and to find the factors
# that lead to employee attrition

####################################################

# DATA IMPORT

####################################################
general_data <- read.csv("general_data.csv", stringsAsFactors = F,header = T)
employee_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = F, header = T)
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = F, header = T)
in_time <- read.csv("in_time.csv", stringsAsFactors = F)
out_time <- read.csv("out_time.csv", stringsAsFactors = F)

str(general_data)
str(employee_survey_data)
str(manager_survey_data)

####################################################

# DATA PREPARATION AND CLEANING

####################################################

# EmployeeID is common field
# let's find out if there are any duplicates
length(unique(general_data$EmployeeID)) == nrow(general_data)
length(unique(employee_survey_data$EmployeeID)) == nrow(employee_survey_data)
length(unique(manager_survey_data$EmployeeID)) == nrow(manager_survey_data)
# no duplicates

# Let's find out if there are any missing rows
setdiff(general_data$EmployeeID, employee_survey_data$EmployeeID)
setdiff(general_data$EmployeeID, manager_survey_data$EmployeeID)
employee <- merge(general_data, employee_survey_data, by = "EmployeeID", all = F)
employee <- merge(employee, manager_survey_data, by = "EmployeeID", all = F)
str(employee)

# Rows with constant values for all employess will not impact regression, so remove them
summary(employee$EmployeeCount)
summary(employee$StandardHours)
names(employee)
employee <- employee[, -c(9,16,18)]

#################################################

# HANDLING IN_TIME & OUT_TIME

#################################################
# Update column name
colnames(in_time)[1] <- "EmployeeID"
colnames(out_time)[1] <- "EmployeeID"

# Remove columns that have 'NA' values as they are designamted holidays
in_time1 <- in_time
in_time1 <- in_time1[ , ! apply( in_time1 , 2 , function(x) all(is.na(x)) ) ]

out_time1 <- out_time
out_time1 <- out_time1[, ! apply(out_time1, 2, function(x) all(is.na(x)))]

# Convert to date objects
out_time2 <- out_time1[,-1]
out_time2 <- sapply(out_time2, function(x) strptime(x, "%m/%d/%Y %H:%M"))
out_time2 <- as.data.frame(out_time2)


in_time2 <- in_time1[,-1]
in_time2 <- sapply(in_time2, function(x) strptime(x, "%m/%d/%Y %H:%M"))
in_time2 <- as.data.frame(in_time2)

# Check if number of columns are same in in_time2 and out_time2
ncol(in_time2) == ncol(out_time2) # number of columns are same

# Check if number of rows are same in in_time2 and out_time2
nrow(in_time2) == nrow(out_time2) # number of rows are same

# Check if names of columns are same in in_time2 and out_time2
colnames(in_time2) == colnames(out_time2) # all columns are identical

# Create a data frame containing the difference in timings
diff_time <- function(x,y) {
  df <- data.frame(matrix("", nrow=nrow(x), ncol = ncol(x)))
    for (j in 1:ncol(x)) {
      df[,j] <- difftime(y[,j], x[,j], units = c("hours"))
      df[,j] <- as.numeric(df[,j])
    }
  return(df)
}

time_diff <- diff_time(in_time2, out_time2)
summary(time_diff)
str(time_diff)
# Find avg working hours for each employee
avg_time <- as.data.frame(rowMeans(time_diff,na.rm = TRUE, dims = 1))

# Merge avg working hours with master data set
employee <- cbind(employee, avg_time)
names(employee)
colnames(employee)[27] <- "Avg_Working_Hrs"

# Count number of leaves
in_time2$num_leaves_InTime <- rowSums(is.na(in_time2),na.rm = FALSE, dims = 1)
names(in_time2)
colnames(in_time2)[250] <- "Num_Leaves_InTime"
summary(in_time2$Num_Leaves_InTime)

out_time2$num_leaves_OutTime <- rowSums(is.na(out_time2),na.rm = FALSE, dims = 1)
names(out_time2)
colnames(out_time2)[250] <- "Num_Leaves_OutTime"
summary(out_time2$Num_Leaves_OutTime)

# Check if any difference in leaves based on in_time and out_time
in_time2$Num_Leaves_InTime == out_time2$Num_Leaves_OutTime
# no difference

# Merge number of leaves with master data set
employee <- cbind(employee, in_time2$Num_Leaves_InTime)
names(employee)
colnames(employee)[28] <- "Number_Of_Leaves"

######################################################

# EXPLORATORY DATA ANALYSIS

######################################################

# Barcharts for categorical features
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(employee, aes(x=BusinessTravel,fill=Attrition))+ geom_bar() + bar_theme1, 
          ggplot(employee, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee, aes(x=factor(Education),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee, aes(x=factor(JobLevel),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")

plot_grid(ggplot(employee, aes(x=JobRole,fill=Attrition))+ geom_bar() + bar_theme1, 
          ggplot(employee, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee, aes(x=factor(JobInvolvement),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee, aes(x=factor(JobSatisfaction),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")

plot_grid(ggplot(employee, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar() + bar_theme1, 
          ggplot(employee, aes(x=factor(WorkLifeBalance),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")

# Correlation matrix for numeric variables
names(employee)
str(employee)
ggpairs(employee[,c(2,6,14,15,17,18,19,20,21,27,28)])

# Following top variables are positively correlated:
# YearsWithCurrentManager and YearsAtCompany (0.769)
# TotalWorkingYears and Age (0.681)
# YearsAtCompmay and TotalWorkingYears (0.628)
# YearsSinceLastPromotion and YearsAtCompany (0.618)


# Following top variables are negatively correlated, though feebly:
# Number_Of_Leaves and Avg_Working_Hrs (-0.25)
# YearsAtCompany and NumCompaniesWorked (-0.118)
# YearsWithCurrentManager and NumCompaniesWorked (-0.11)


#####################################################

# OUTLIER TREATMENT AND BINNING VARIABLES

#####################################################
# Age
quantile(employee$Age, seq(0,1,0.1))
# DistanceFromHome
quantile(employee$DistanceFromHome, seq(0,1,0.01))
# MonthlyIncome
quantile(employee$MonthlyIncome,seq(0,1,0.1),na.rm = TRUE)
# Bin Salary
employee$MonthlyIncome <- ifelse(employee$MonthlyIncome <= 30000, "very low",
                                 ifelse(employee$MonthlyIncome > 30000 & employee$MonthlyIncome <= 50000, "low",
                                        ifelse(employee$MonthlyIncome > 50000 & employee$MonthlyIncome <= 100000, "medium", 
                                               ifelse(employee$MonthlyIncome > 100000 & employee$MonthlyIncome <= 150000,"high", "very high"))))



# NumCompaniesWorked
quantile(employee$NumCompaniesWorked,seq(0,1,0.01), na.rm = TRUE)
# PercentSalaryHike
quantile(employee$PercentSalaryHike,seq(0,1,0.01), na.rm = TRUE)
# TotalWorkingYears
quantile(employee$TotalWorkingYears,seq(0,1,0.01), na.rm = TRUE)
# TrainingTimesLastYear
quantile(employee$TrainingTimesLastYear,seq(0,1,0.01), na.rm = TRUE)
# YearsAtcompany
quantile(employee$YearsAtCompany,seq(0,1,0.01), na.rm = TRUE)
ggplot(employee,aes(factor(TotalWorkingYears),YearsAtCompany)) + geom_boxplot()
# YearsSinceLastPromotion
quantile(employee$YearsSinceLastPromotion,seq(0,1,0.01), na.rm = TRUE)
# YearsWithCurrManager
quantile(employee$YearsWithCurrManager,seq(0,1,0.01), na.rm = TRUE)
# Avg_Working_Hrs
quantile(employee$Avg_Working_Hrs,seq(0,1,0.1), na.rm = TRUE)
# Number_Of_Leaves
quantile(employee$Number_Of_Leaves,seq(0,1,0.1), na.rm = TRUE)

# overall, no outliers as such, though there are few exceptional cases that show signs of outliers but would ignore since that's quite possible in a company
# leaving the data as is

####################################################

# MISSING VALUE/ NA TREATMENT

####################################################
summary(employee)
sapply(employee, function(x) sum(is.na(x)))
# Following columns have NAs:
# NumCompaniesWorked(19), TotalWorkingYears(9), EnvironmentSatisfaction(25),
# JobSatisfaction(20), WorkLifeBalance(38)

quantile(employee$NumCompaniesWorked, seq(0,1,0.1), na.rm = TRUE)

quantile(employee$TotalWorkingYears, seq(0,1,0.1), na.rm = TRUE)


# The NAs are spread across rows
# If we remove rows with NA values, 111 rows which is 111/4410 = 2.5% of total data
# let's remove rows with NA
employee <- employee[!is.na(employee$NumCompaniesWorked),]
employee <- employee[!is.na(employee$TotalWorkingYears),]
employee <- employee[!is.na(employee$EnvironmentSatisfaction),]
employee <- employee[!is.na(employee$JobSatisfaction),]
employee <- employee[!is.na(employee$WorkLifeBalance),]

length(which(is.na(employee)))
# No NAs now


#####################################################

# FEATURE STANDARDIZATION

#####################################################

# Normalizing Continuous Features
employee1 <- employee
employee1$DistanceFromHome <- scale(employee1$DistanceFromHome)
employee1$NumCompaniesWorked <- scale(employee1$NumCompaniesWorked)
employee1$PercentSalaryHike <- scale(employee1$PercentSalaryHike)
employee1$TotalWorkingYears <- scale(employee1$TotalWorkingYears)
employee1$TrainingTimesLastYear <- scale(employee1$TrainingTimesLastYear)
employee1$YearsAtCompany <- scale(employee1$YearsAtCompany)
employee1$YearsSinceLastPromotion <- scale(employee1$YearsSinceLastPromotion)
employee1$YearsWithCurrManager <- scale(employee1$YearsWithCurrManager)

# Converting dependent variable Attrition from No/Yes character to factor with levels 0/1 
employee1$Attrition<- ifelse(employee1$Attrition=="Yes",1,0)
str(employee1$Attrition)

# Checking attrition rate of employee
Attrition <- sum(employee1$Attrition)/nrow(employee1)
Attrition # 16.16% attrition rate

############################################

# DUMMY VARIABLE CREATION

############################################

# Creating a data frame of categorical variables
names(employee1)
employee1_chr <- data.frame(employee1[,-c(1,2,6,14,15,17,18,19,20,21,27,28)])
names(employee1_chr)
# convert to factors
employee1_fact <- data.frame(sapply(employee1_chr, function(x) factor(x)))
# create dummy variables
dummies <- data.frame(sapply(employee1_fact, function(x) data.frame(model.matrix(~x, data = employee1_fact))[,-1]))

##############################################

# FINAL DATA SET

#############################################
# Create final data set
employee_final <- cbind(employee1[,c(2,6,14,15,17,18,19,20,21,27,28)], dummies)
names(employee_final)

#############################################

# SPLIT TRAIN AND TEST DATA SETS

############################################
set.seed(100)

indices = sample.split(employee_final$Attrition, SplitRatio = 0.7)
train = employee_final[indices,]
test = employee_final[!indices,]

##############################################

# MODEL BUILDING USING LOGISTIC REGRESSION

##############################################
model_1 <- glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)

model_2 <- stepAIC(model_1, direction = "both")

summary(model_2)

# Removing multicollinearity through VIF check
vif(model_2)

# Excluding JobLevel.x5
model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_Working_Hrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Education.x5 + EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_3)
vif(model_3)

# Excluding MaritalStatus.xMarried
model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_Working_Hrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Education.x5 + EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_4)
vif(model_4)

# Excluding Education.x5
model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_Working_Hrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_5)
vif(model_5)

# Excluding JobRole.xHuman.Resources
model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_Working_Hrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_6)
vif(model_6)

# Excluding JobLevel.x2 
model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_Working_Hrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_7)
vif(model_7)

# Excluding JobRole.xManager
model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_Working_Hrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)


summary(model_8)
vif(model_8)

# Excluding JobInvolvement.x3
model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_Working_Hrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4, family = "binomial", 
               data = train)

summary(model_9)
vif(model_9)

# Excluding StockOptionLevel.x1
model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_Working_Hrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  EducationField.xLife.Sciences + EducationField.xMarketing + 
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4, family = "binomial", 
                data = train)

summary(model_10)
vif(model_10)

# Excluding JobRole.xSales.Executive
model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_Working_Hrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  EducationField.xLife.Sciences + EducationField.xMarketing + 
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4, family = "binomial", 
                data = train)

summary(model_11)
vif(model_11)

# Excluding JobRole.xResearch.Director
model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_Working_Hrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  EducationField.xLife.Sciences + EducationField.xMarketing + 
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4, family = "binomial", 
                data = train)

summary(model_12)
vif(model_12)

# Excluding BusinessTravel.xTravel_Rarely
model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_Working_Hrs + BusinessTravel.xTravel_Frequently + 
                  EducationField.xLife.Sciences + EducationField.xMarketing + 
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4, family = "binomial", 
                data = train)

summary(model_13)
vif(model_13)

################################################################################################################
# With 24 significant variables in the model
# BusinessTravel.xTravel_Frequently & Avg_Working_Hrs have large co-efficients and impact attrition positively
# while EducationField.xOther & EducationField.xTechnical.Degree have co-efficients that impact 
# attrition negatively that is people having education in these domains
# don't quit

final_model <- model_13

###############################################################################################################

# Next we will test this model against, test data set
test_pred <- predict(final_model, type = "response", newdata = test)
summary(test_pred)
test$prob <- test_pred

# Let's use the probability cutoff of 50%
test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_pred_attrition, test_actual_attrition)

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")

test_conf
# Accuracy(0.860), Sensitivity(0.325), Specificity(0.963)
# Sensitivity is rock bottom

# Let's use the probability cutoff of 40%
test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_pred_attrition, test_actual_attrition)

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")

test_conf

# Accuracy(0.852), Sensitivity(0.411), Specificity(0.938)
# Sensitivity improved, but let's try further

# Let's use the probability cutoff of 30%
test_pred_attrition <- factor(ifelse(test_pred >= 0.30, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_pred_attrition, test_actual_attrition)

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")

test_conf

# Accuracy(0.838), Sensitivity(0.612), Specificity(0.882)
# Thus, the power of the model correctly predicting the probability of employee quitting is less compared to the power of the model correctly predicting the probability that the employee will not quit


# Let's plot the ROC curve
g <- roc(Attrition ~ prob, data = test)
plot(g)
auc(test$Attrition, test_pred)
# Area under the curve: 0.8258
# It's fairly good

#########################################################################################
# Let's Choose the cutoff value. 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff)
{
  predicted_attr <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attr, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.30 for final model

test_cutoff_attr <- factor(ifelse(test_pred >=0.30, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attr, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

#View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attr <- ifelse(test_cutoff_attr=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attr, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}