library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(treemap)
library(reshape2)

#################
# DATA SOURCING #
#################
loan <- read.csv("loan.csv", header = TRUE)

######################
# DATA UNDERSTANDING #
######################
dim(loan)
# data set has 39717 rows and 111 columns

str(loan)
summary(loan)

# check if any duplicate entry in 'id' column
length(unique(loan$id)) == nrow(loan) # no duplicate entry

# check if any duplicate entry in 'member_id' column
length(unique(loan$member_id)) == nrow(loan) # no duplicate entry

# data validation
# check if total_rec_prncp > funded_amnt
loan[which(loan$total_rec_prncp>loan$funded_amnt+1),] # no such entries

#################
# DATA CLEANING #
#################

# remove columns with 100% 'NA' values
summary(loan)
loan <- loan[,-c(50:111)]
dim(loan)
summary(loan)
str(loan)

# delete unnecessary columns
names(loan)
loan <- loan[,-c(18,19,23,36)]
dim(loan)
str(loan)

# remove '%' from int_rate and make it numeric
loan$int_rate <- as.numeric(gsub("%", "", loan$int_rate)) / 100

# remove '%' from revol_util and make it numeric
loan$revol_util <- as.numeric(gsub("%", "", loan$revol_util)) / 100


# Standardize Values (Outlier detection)
# checking annual_inc for outliers & cleaning them
str(loan)
summary(loan$annual_inc) # ranges between 4K(min) to 60L(max), 3rd quartile is @ 82300, so definitely needs investigation
sd(loan$annual_inc) # sd is approx same as mean, so values can't be true
ggplot(loan, aes(emp_length, annual_inc)) + geom_boxplot()
ggplot(loan, aes(annual_inc,funded_amnt)) + geom_point(aes(color=grade)) + ggtitle("Annual Income Vs Funded Amt") + geom_smooth()
# It appears that there are some high income borrowers still borrowing money.
loan[which(loan$annual_inc == max(loan$annual_inc)), "funded_amnt"]
# Highest paid person borrowed 5000 units of money, does not justify
loan[which(loan$annual_inc>500000),][c(3,14)] # Will get rid of data with annual income > 5L
loan <- loan[!loan$annual_inc>500000,] # remove bad data
summary(loan$annual_inc)
sd(loan$annual_inc)
ggplot(loan, aes(annual_inc,funded_amnt)) + geom_point(aes(color=grade)) + ggtitle("Annual Income Vs Funded Amt") + geom_smooth()


# Standardize precision
loan$total_pymnt <- round(loan$total_pymnt,2)
loan$total_rec_late_fee <- round(loan$total_rec_late_fee,2)
loan$collection_recovery_fee <- round(loan$collection_recovery_fee,2)

########################################
# DATA TRANSFORMATION/ DERIVED METRICS #
########################################
# Split issue_d col into month, year, qtr
loan$issue_d <- as.character(loan$issue_d)
loan$issue_d <- paste("1", loan$issue_d, sep = "-")
loan$issue_d <- as.Date(loan$issue_d, "%d-%y-%b")
loan$issue_qtr <- quarters(loan$issue_d)
loan$issue_month <- months(loan$issue_d)
loan$issue_year <- substring(loan$issue_d,1,4)

# identify since how many years earliest credit line opened
str(loan$earliest_cr_line)
loan$earliest_cr_line <- as.character(loan$earliest_cr_line)
head(loan$earliest_cr_line)
loan$earliest_cr_line <- as.Date(loan$earliest_cr_line, "%m/%d/%Y")
loan <- mutate(loan, yrs_since_earliest_cr_line = (Sys.Date() - earliest_cr_line)/365)
loan$yrs_since_earliest_cr_line <- round(as.numeric(loan$yrs_since_earliest_cr_line),2)
summary(loan$yrs_since_earliest_cr_line)

# Add 2 new columns: monthly_inc & dti_new
loan <- mutate(loan, monthly_inc = annual_inc/12, dti_new = installment/monthly_inc)

# add a new column credit_loss a.k.a total credit loss
sum(loan$funded_amnt)-sum(loan$total_rec_prncp)
loan$credit_loss <- loan$funded_amnt-loan$total_rec_prncp

summary(loan)
dim(loan)

############################################
# DATA CLEANING DONE, LET'S BEGIN WITH EDA #
############################################

#######################
# UNIVARIATE ANALYSIS #
#######################
# 1. Number of approved loans over time
df_new <- loan %>% group_by(issue_d) %>% summarise(length(id))
colnames(df_new)[2] <- "count"
ggplot(df_new, aes(issue_d,count)) + geom_point() + geom_smooth() + ggtitle("Approve Loans over time") + xlab("Time Period") + ylab("Number of Loans")
df_new <- NULL

# 2. Total funded amount over time
df_new <- loan %>% group_by(issue_d) %>% summarise(total_funded_amnt = sum(funded_amnt))
ggplot(df_new, aes(issue_d, total_funded_amnt)) + geom_point() + geom_smooth() + ggtitle("Total Funded Amnnt Over Time") + xlab("Time Period") + ylab("Total Funded Amount")
df_new <- NULL

# 17. In which quarter & month most defaults loans were disbursed
ggplot(loan, aes(issue_qtr)) + geom_bar(stat = "count") + ggtitle("Loans Issued By Qtr")
ggplot(loan, aes(issue_month)) + geom_bar(stat = "count") + theme(axis.text.x = element_text(angle = 70, vjust = 0.7)) + scale_x_discrete(limits=c("January","February","March","April","May","June", "July", "August","September","October","November", "December")) + ggtitle("Loans Issued By Month") + xlab("Month") + ylab("Number Of Loans")

# 19. avg interest rates over time period
df_new <- loan %>% group_by(issue_d) %>% summarise(avg_int_rate = mean(int_rate))
ggplot(df_new, aes(issue_d, avg_int_rate)) + geom_point() + geom_smooth() + ggtitle("Avg Int Rates Over Time Period") + xlab("Time Period") + ylab("Avg Interest Rate")
df_new <- NULL


#################################
# SEGMENTED UNIVARIATE ANALYSIS #
#################################
# 3. Number of Loans disbursed by term across time period
df_new <- loan %>% group_by(issue_d, term) %>% summarise(num_loans = length(id))
ggplot(df_new, aes(issue_d,num_loans, color = term)) + geom_line(size = 1) + geom_point() + ggtitle("Number Of Loans Disbursed Across Time Period/ Term") + xlab("Time Period") + ylab("Number Of Loans ")
df_new <- NULL

# 4. Volume of Loans by term across time period
df_new <- loan %>% group_by(issue_d, term) %>% summarise(funded_amnt = sum(funded_amnt), credit_loss = sum(credit_loss))
ggplot(df_new, aes(issue_d, funded_amnt, color = term)) + geom_line() + geom_point() + xlab("Time Period") + ylab("Amount Funded") + geom_hline(yintercept = mean(df_new$credit_loss), color = "blue") + ggtitle("Volume Of Loans Across Time Period/ Term")
df_new %>% group_by(term) %>% summarise(avg_credit_loss = round(mean(credit_loss),2))
df_new <- NULL
# Volume of loans issued for 60 months is approx same as that issued for 36 months

# 5. Credit Loss by Term Across Time Period
df_new <- loan %>% group_by(issue_d, term) %>% summarise(funded_amnt = sum(funded_amnt), credit_loss = sum(credit_loss))
df_new$percent_loss <- round(df_new$credit_loss/df_new$funded_amnt*100,2)
ggplot(df_new, aes(issue_d, percent_loss, color = term)) + geom_line() + geom_point() + geom_hline(yintercept = mean(df_new$percent_loss), color = "blue") + xlab("Time Period") + ylab("Percent Credit Loss") + ggtitle("Percent Credit Loss Across Time Period/ Term")
df_new %>% group_by(term) %>% summarise(avg_percent_loss = round(mean(percent_loss),2))
df_new <- NULL
# avg percent credit loss for term 60 months (16%) is twice than that for 36 months (8%)

# 6. Distribution of loan_status across grade
df_new <- loan %>% group_by(loan_status, grade) %>% summarise(num_loan_status = length(id))
ggplot(df_new, aes(grade,num_loan_status,fill = loan_status)) + geom_bar(stat = "identity") + ggtitle("Distribution Of Loan Status Across Grade") + xlab("Grade") + ylab("Number Of Loans")
# B, C & D grade loans have maximum number of defaults
ggplot(df_new, aes(grade,num_loan_status,fill = loan_status)) + geom_bar(stat = "identity", position = "fill") + ggtitle("Distribution Of Loan Status Across Grade")
# Loans Charged Off for grade 'G' thru 'A' in order i.e Lower grade loans are likely to be defaulted more than higher grade loans
df_new <- NULL

# 18. avg interest rate across States
df_new <- loan %>% group_by(addr_state) %>% summarise(avg_int_rate = round(mean(int_rate),2))
summary(df_new$avg_int_rate)
treemap(df_new, index = c("addr_state"), vSize = "avg_int_rate", type = "index", palette = "Greens", title = "Interest Rates Across States")
df_new <- NULL

# open credit line vs grade
ggplot(loan, aes(open_acc)) + geom_density(aes(fill=grade)) + facet_grid(grade~.) + ggtitle("Opened Credit Lines Vs Grade") + xlab("Open Credit Lines")
# majority borrowers under high grade loan category have more open credit lines compared to the ones under low grade loan category

# Analysing revol_bal wrt grade
ggplot(loan, aes(revol_bal)) + geom_density(aes(fill=grade)) + facet_grid(grade~.) + xlab("Revolving Balance") + ggtitle("Revolving Balance by Grade")
# majority of borrowers under high grade loan category have typically less revolving balance compared to borrowers under low grade loan category

# 21. distribution plot for loan_status vs verification_status
ggplot(loan, aes(grade ,fill=verification_status)) + geom_bar(position = "fill", stat = "count")
# High grade loans have high proportion of loans with 'not verified' status


################################################
# BIVARIATE ANALYSIS FOR CATEGORICAL VARIABLES #
################################################
# 7. number of loans by state & loan_status
df_new <- loan %>% group_by(addr_state, loan_status) %>% summarise(num_loans = length(id))
ggplot(df_new, aes(addr_state,num_loans, fill = loan_status)) + geom_bar(stat = "identity") + xlab("State") + ylab("Number Of Loans")
# CA has max loans disbursed followed by NY & TX
ggplot(df_new, aes(addr_state,num_loans, fill = loan_status)) + geom_bar(stat = "identity", position = "fill") + xlab("State") + ylab("Number Of Loans")
# NE state has most loans that were charged off compared to any other state.
df_new <- NULL

# 8. volume of loan by state & loan_status
df_new <- loan %>% group_by(addr_state, loan_status) %>% summarise(vol = sum(funded_amnt))
ggplot(df_new, aes(addr_state,vol, fill = loan_status)) + geom_bar(stat = "identity") + xlab("State") + ylab("Funded Amount")
# CA has max amount funded followed by NY & TX
ggplot(df_new, aes(addr_state,vol, fill = loan_status)) + geom_bar(stat = "identity", position = "fill") + xlab("State") + ylab("Funded Amount")
# NE state has max funded amount charged off due to defaults
df_new <- NULL

# 9. loan_status vs home_ownership wrt number of loans
ggplot(loan, aes(home_ownership, fill=loan_status)) + geom_bar(stat = "count", position = "fill")
# all types of home ownerships have defaulted loans but category with 'other' showed a slight higher chance of loan default

# 10. loan_status vs home_ownership wrt volume of loans
df_new <- loan %>% group_by(home_ownership,loan_status) %>% summarise(total_amnt = sum(funded_amnt))
ggplot(df_new, aes(home_ownership,total_amnt,fill=loan_status)) + geom_bar(stat = "identity") + ggtitle("Total Loan Amnt By Home Ownership")
df_new <- NULL

# 11. loan_status vs purpose wrt number of loans
ggplot(loan, aes(purpose, fill=loan_status)) + geom_bar(stat = "count", position = "fill") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Number Of Loans By Purpose")
ggplot(loan, aes(purpose, fill=loan_status)) + geom_bar(stat = "count") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Number Of Loans By Purpose")


# 12. loan_status vs purpose wrt volume of loans
df_new <- loan %>% group_by(purpose,loan_status) %>% summarise(total_amnt = sum(funded_amnt))
ggplot(df_new, aes(purpose,total_amnt,fill=loan_status)) + geom_bar(stat = "identity", position = "fill") + ggtitle("Total Loan Amnt By Purpose") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggplot(df_new, aes(purpose,total_amnt,fill=loan_status)) + geom_bar(stat = "identity") + ggtitle("Total Loan Amnt By Purpose") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
df_new <- NULL

# 13. loan_status vs sub_grade wrt number of loans
ggplot(loan, aes(sub_grade, fill=loan_status)) + geom_bar(stat = "count", position = "fill") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Number Of Loans By Sub Grade")
ggplot(loan, aes(sub_grade, fill=loan_status)) + geom_bar(stat = "count") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Number Of Loans By Sub Grade")

# 14. loan_status vs sub_grade wrt volume of loans
df_new <- loan %>% group_by(sub_grade,loan_status) %>% summarise(total_amnt = sum(funded_amnt))
ggplot(df_new, aes(sub_grade,total_amnt,fill=loan_status)) + geom_bar(stat = "identity", position = "fill") + ggtitle("Total Loan Amnt By Purpose") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggplot(df_new, aes(sub_grade,total_amnt,fill=loan_status)) + geom_bar(stat = "identity") + ggtitle("Total Loan Amnt By Purpose") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
df_new <- NULL

# 15. loan_status vs emp_length wrt number of loans
ggplot(loan, aes(emp_length, fill=loan_status)) + geom_bar(stat = "count", position = "fill") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Number Of Loans By Emp Length")
ggplot(loan, aes(emp_length, fill=grade)) + geom_bar(stat = "count") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Number Of Loans By Emp Length") + ylab("Number Of Loans") + xlab("Emp Length")

# 16. loan_status vs emp_length wrt volume of loans
df_new <- loan %>% group_by(emp_length,loan_status) %>% summarise(total_amnt = sum(funded_amnt))
ggplot(df_new, aes(emp_length,total_amnt,fill=loan_status)) + geom_bar(stat = "identity", position = "fill") + ggtitle("Total Loan Amnt By Emp Length") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggplot(df_new, aes(emp_length,total_amnt,fill=loan_status)) + geom_bar(stat = "identity") + ggtitle("Total Loan Amnt By Emp Length") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
df_new <- NULL

# 20. funded_amnt & credit_loss wrt purpose
df_new <- loan %>% group_by(purpose) %>% summarise(total_funded_amt = sum(funded_amnt), Total_Credit_Loss = sum(credit_loss))
treemap(df_new, index = c("purpose"), vSize = "total_funded_amt", vColor = "Total_Credit_Loss", type = "value", palette = "Reds", title = "Loan Distribution Across Purpose & Credit Loss") 
df_new <- NULL
# max volume of loans issued and max credit loss happened for purpose 'debt consolidation'


###############################################
# BIVARIATE ANALYSIS FOR CONTINUOUS VARIABLES #
###############################################

# correlation matrix
names(loan)
str(loan)
df_new <- loan[,c(4,7,8,14,22,23,25,30,31,49,51,52)]
summary(df_new)
str(df_new)
res <- round(cor(df_new, use = "complete.obs"),2)
get_lower_tri <- function(res){
  res[upper.tri(res)]<- NA
  return(res)
}

reorder_res <- function(res){
  # Use correlation between variables as distance
  dd <- as.dist((1-res)/2)
  hc <- hclust(dd)
  res <-res[hc$order, hc$order]
}

res <- reorder_res(res)
lower_tri <- get_lower_tri(res)
melted_res <- melt(lower_tri, na.rm = TRUE)
ggheatmap <- ggplot(melted_res, aes(Var1, Var2, fill = value)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1,1), space = "lab", name = "Pearson Correlation") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +  coord_fixed()
ggheatmap + geom_text(aes(Var1, Var2, label = value), color = "black", size = 3) + ggtitle("Correlation for Continuous Variables")
# No major correlation between continuous variables

# 1. distribution across funded_amnt and annual_inc by grade (point plot)
p <- ggplot(loan, aes(annual_inc,funded_amnt)) + geom_point(aes(color=grade)) + ggtitle("Annual Income Vs Funded Amt") + geom_smooth()
p
p + xlim(0,100000) + facet_grid(.~grade) + geom_smooth() + xlab("Funded Amount") + ylab("Annual Income")
# Steepness of lines shows that more money borrowed relative to income, more risky the loan is i.e lower grade. Thus, annual income is a major factor to determine grade of a loan
p <- NULL

# 2. distribution by funded_amnt (density plot)
p <- ggplot(loan, aes(funded_amnt)) + geom_density(aes(fill=grade)) + ggtitle("Funded Amt By Grade") + facet_grid(grade~.)
p + xlab("Funded Amount")
# most borrowers with high grades have less funded amounts
p <- NULL

# 3. distribution across funded_amnt and int_rate by grade (point plot)
p <- ggplot(loan, aes(int_rate,funded_amnt)) + geom_point(aes(color=grade)) + ggtitle("Int Rate Vs Funded Amt") + geom_smooth()
p
p + facet_grid(.~grade) + geom_smooth() + xlab("Interest Rate") + ylab("Funded Amount")
# Higher funded amounts come at higher interest rates and lower grades, making it more risky to turn it to default 
p <- NULL

# 4. distribution across dti_new by grade (density plot)
ggplot(loan, aes(dti_new)) + geom_density(aes(fill=grade)) + facet_grid(grade~.) + xlab("Debt-To-Income Ratio") + ggtitle("DTI By Grade")
# i.e for a high grade loan, majority borrowers commit <10% of their monthly income as installment
ggplot(loan,aes(dti/100)) + xlim(0,1) + geom_density(aes(fill=grade)) + facet_grid(grade~.)

# 8. distribution by int_rate (density plot)
p <- ggplot(loan, aes(int_rate)) + geom_density(aes(fill=grade)) + ggtitle("Int Rate Vs Funded Amt") + facet_grid(grade~.) + xlab("Interest Rate") + ggtitle("Interest Rate By Grade")
p
# lower interest rates for high grade loans i.e less risky loans have lower interest rates
p <- NULL

# 10. distribution across funded_amnt by inq_last_6mths (box plot)
p <- ggplot(loan, aes(factor(inq_last_6mths),funded_amnt)) + geom_boxplot(aes(fill=grade)) + ggtitle("Int Rate Vs Funded Amt")
p
p + facet_grid(.~grade) + ggtitle("Inquiries Vs Funded Amount/ Grade") + xlab("Inquiries Since 6 Months") + ylab("Funded Amount")
# Low grade loans have less inquiries (0-3) in last 6 months while high grade loans have more inquiries (0-8) in last 6 mths. Also, the funded amount for high grade loans is less i.e if the funded amount is high and number of inquiries is less (0-3), then it is a risky loan
p <- NULL