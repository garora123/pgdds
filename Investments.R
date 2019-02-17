#LOAD CSV/TXT & UNDERSTAND DATA
companies <- read.delim("companies.txt", sep="\t", header = TRUE)
rounds2 <- read.csv("rounds2.csv")

#DATA CLEANING & PREPARATION
#change first column in rounds2 to upper case
rounds2[,1] <- toupper(rounds2[,1])

#change first column in companies to upper case
companies[,1] <- toupper(companies[,1])

#change first column name in companies to match with first column in rounds2
colnames(companies)[1] <- "company_permalink"

#merge companies and rounds into master_frame
master_frame <- merge(companies, rounds2, by="company_permalink")

#Fill blank rows in 'country-code' column with string "OTH"
sapply(master_frame, function(x) sum(x == ""))
names(master_frame)
master_frame$country_code <- as.character(master_frame[,6])
master_frame$country_code[which(master_frame$country_code=="")] <- "OTH"
master_frame$country_code <- as.factor(master_frame[,6])

#Fill blank rows in 'category_list' column with string "UNKNOWN"
sapply(master_frame, function(x) sum(x == ""))
names(master_frame)
master_frame$category_list <- as.character(master_frame[,4])
master_frame$category_list[which(master_frame$category_list=="")] <- "UNKNOWN"
master_frame$category_list <- as.factor(master_frame[,4])

###########################
#DATA ANALYSIS BEGINS HERE#
###########################

#Checkpoint-1
#How many unique companies are present in rounds2?
length(unique(rounds2$company_permalink))
#ANS: 66368

#How many unique companies are present in companies?
length(unique(companies$company_permalink))
#ANS: 66368

#In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
#ANS: name

#Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N
#compare 2 dfs
length(setdiff(rounds2$company_permalink, companies$company_permalink))
#ANS: Since the result of above cmd is '0', it means there are no companies in rounds2 which are not present in companies

#Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. Name the merged frame master_frame. How many observations are present in master_frame?
length(master_frame$company_permalink)
#ANS: 114949

#Checkpoint-2
#Average funding amount of venture type
#First method
venture <- aggregate(master_frame$raised_amount_usd, by=list(master_frame$funding_round_type), FUN=mean, na.rm=TRUE, na.action=NULL)
venture_avg <- subset(venture, venture$Group.1 == "venture")
#ANS: 11748949

#Second method
venture <- master_frame[master_frame$funding_round_type == "venture",][15]
venture_avg <- mean(venture$raised_amount_usd, na.rm = TRUE)
#ANS: 11748949

#Average funding amount of angel type
#First method
angel <- aggregate(master_frame$raised_amount_usd, by=list(master_frame$funding_round_type), FUN=mean, na.rm=TRUE, na.action=NULL)
angel_avg <- subset(angel, angel$Group.1 == "angel")
#ANS: 958694.5

#Second method
angel <- master_frame[master_frame$funding_round_type == "angel",][15]
angel_avg <- mean(angel$raised_amount_usd, na.rm = TRUE)
#ANS: 958694.5


#Average funding amount of seed type
#First method
seed <- aggregate(master_frame$raised_amount_usd, by=list(master_frame$funding_round_type), FUN=mean, na.rm=TRUE, na.action=NULL)
seed_avg <- subset(seed, seed$Group.1 == "seed")
#ANS: 719818

#Second method
seed <- master_frame[master_frame$funding_round_type == "seed",][15]
seed_avg <- mean(seed$raised_amount_usd, na.rm = TRUE)
#ANS: 719818

#Average funding amount of private_equity type
#First method
private_equity <- aggregate(master_frame$raised_amount_usd, by=list(master_frame$funding_round_type), FUN=mean, na.rm=TRUE, na.action=NULL)
private_equity_avg <- subset(private_equity, private_equity$Group.1 == "private_equity")
#ANS: 73308593

#Second method
private_equity <- master_frame[master_frame$funding_round_type == "private_equity",][15]
private_equity_avg <- mean(private_equity$raised_amount_usd, na.rm = TRUE)
#ANS: 73308593


#Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, which investment type is the most suitable for it?
aggregate(master_frame$raised_amount_usd, by=list(master_frame$funding_round_type), FUN=mean, na.rm=TRUE, na.action=NULL)
#ANS: amount invested during funding round type as 'venture' is 11748949.1 (~11.7M). Since it is the only figure lying in the range 5M to 15M, hence venture is the most suitable investment type


#Checkpoint-3
#Top9 countries with highest total funding for funding round type "venture"
top9 <- aggregate(master_frame$raised_amount_usd, by=list(master_frame$country_code, master_frame$funding_round_type), FUN = sum, na.rm = TRUE, na.action = NULL) #construct data frame that contains sum of investments grouped by country and funding round type
names(top9)
top9 <- subset(top9, top9[,2] == "venture")
top9 <- head(top9[order(top9[,3], decreasing = TRUE),],9)
top9$Group.2<-NULL
top9$x<-NULL
top9<-NULL
#ANS: Following the pdf, United States, United Kingdom and India are top 3 English speaking countries in the order. There is another category of countries called "OTH" that lies above India but since this category has been generated from 'blank' country codes, it can't be determined if it is English speaking.


#Checkpoint-4
#Extract primary sector into a new column named 'Primary Sector'
library(dplyr)
library(tidyr)
master_frame_separated <- separate(master_frame, category_list, into=c("primary sector", "other sector"), remove = FALSE, extra = "drop", sep="\\|") #separate category list into primary sector and other sector columns
master_frame_separated["other sector"]<-NULL #remove "other sector" column as not required

#Map each primary sector to one of the eight main sectors
mapping <- read.csv("mapping.csv") #load mapping file
sapply(mapping, function(x) sum(x == "")) #identify if there are any blank cells
names(mapping)
mapping <- gather(mapping, main_sector, val, Automotive...Sports:Social..Finance..Analytics..Advertising) #convert wide format to long format based on main sectors
mapping <- mapping[!(mapping$val==0),] #keep asymmetric variables
mapping <- mapping[,-3] #remove unwanted column 'val'
new_master_frame <- merge(master_frame_separated, mapping, by.x = "primary sector", by.y = "category_list", all = TRUE) #final step: map primary sector in master_frame to respective main_sector in mapping
new_master_frame <- new_master_frame[!(is.na(new_master_frame$name)),] #remove rows with 'name' equals 'NA' as these don't exist
master_frame <- new_master_frame
str(master_frame)
new_master_frame<-NULL
master_frame_separated<-NULL

#Checkpoint-5
#DATA CLEANING in master_frame
sapply(master_frame, function (x) sum(x=="")) #identify if there are any NA/ invalid values for any cells under analysis
sapply(master_frame, function (x) sum(is.na(x)))
names(master_frame) #identified raised_amount_usd and main_sector set as 'NA'
master_frame$raised_amount_usd[which(is.na(master_frame$raised_amount_usd))] <- 0 #convert raised_amount_usd 'NA' values to '0'
master_frame$main_sector[which(is.na(master_frame$main_sector))] <- "UNKNOWN" #convert main_sector 'NA' values to 'UNKNOWN'

#DATA PREPARATION in master_frame (add another column displaying fund raised for each company)
d <- aggregate(master_frame$raised_amount_usd, by=list(master_frame$name), FUN = sum)
colnames(d)[1] <- "name"
colnames(d)[2] <- "total_raised_amount_usd"
master_frame <- merge(master_frame, d, by="name")
d <- NULL

###########################
#DATA ANALYSIS BEGINS HERE#
###########################
#1. country=USA, funding type=venture
Df1 <- filter(master_frame, country_code=="USA" & funding_round_type=="venture" & total_raised_amount_usd >= 5000000 & total_raised_amount_usd <= 15000000) #apply filter country=USA, funding type=venture in master_frame and usd invested between 5M & 15M
D1_temp <- aggregate(Df1$main_sector, by=list(Df1$main_sector), FUN=length) #count no. of investments
colnames(D1_temp)[1] <- "main_sector" #rename column to main_sector
colnames(D1_temp)[2] <- "count_of_investments" #rename column to conut of investments
D1_temp1 <- aggregate(Df1$raised_amount_usd, by=list(Df1$main_sector), FUN=sum) #identify total amount invested in country=USA and funding type=venture
colnames(D1_temp1)[1] <- "main_sector" #rename column to main_sector
colnames(D1_temp1)[2] <- "amount_invested" #rename column to amount invested
D1<-merge(D1_temp, D1_temp1, by="main_sector") #Final Step: create D1 df
D1<-merge(Df1, D1, by = "main_sector")
Df1<-NULL
D1_temp<-NULL
D1_temp1<-NULL

library(dplyr)
#total number of investments in country 1
d1 <- D1 %>% group_by(main_sector) %>% summarise(abc = length(count_of_investments)) 
summarise(d1, sum(abc))
#total amount invested in country 1
d2 <- D1 %>% group_by(main_sector) %>% summarise(def = sum(raised_amount_usd))
summarise(d2, sum(def))
#top sector name (no. of investment-wise)
top <- arrange(d1, desc(abc))
head(top[1,1])
#Second sector name (no. of investment-wise)
head(top[2,1])
#Third sector name (no. of investment-wise)
head(top[3,1])
#number of investments in top sector
head(top[1,2])
#number of investments in second sector
head(top[2,2])
#number of investments in third sector
head(top[3,2])
d1<-NULL
d2<-NULL
top <- NULL
#Top sector count-wise, which company received the highest investment?
temp <- filter(D1, main_sector=="Others")
temp <- arrange(temp, desc(total_raised_amount_usd))
head(temp[1,2])
temp<-NULL
#second best sector count-wise, which company received the highest investment?
temp <- filter(D1, main_sector=="Cleantech...Semiconductors")
temp <- arrange(temp, desc(total_raised_amount_usd))
head(temp[1,2])
temp<-NULL

#2. country=GBR, funding type=venture
Df2 <- filter(master_frame, country_code=="GBR" & funding_round_type=="venture" & total_raised_amount_usd >= 5000000 & total_raised_amount_usd <= 15000000) #apply filter country=GBR, funding type=venture in master_frame and usd invested between 5M & 15M
D2_temp <- aggregate(Df2$main_sector, by=list(Df2$main_sector), FUN=length) #count no. of investments
colnames(D2_temp)[1] <- "main_sector" #rename column to main_sector
colnames(D2_temp)[2] <- "count_of_investments" #rename column to conut of investments
D2_temp1 <- aggregate(Df2$raised_amount_usd, by=list(Df2$main_sector), FUN=sum) #identify total amount invested in country=GBR and funding type=venture
colnames(D2_temp1)[1] <- "main_sector" #rename column to main_sector
colnames(D2_temp1)[2] <- "amount_invested" #rename column to amount invested
D2<-merge(D2_temp, D2_temp1, by="main_sector") #Final Step: create D2 df
D2<-merge(Df2, D2, by = "main_sector")
Df2<-NULL
D2_temp<-NULL
D2_temp1<-NULL

library(dplyr)
#total number of investments in country 2
d1 <- D2 %>% group_by(main_sector) %>% summarise(abc = length(count_of_investments)) 
summarise(d1, sum(abc))
#total amount invested in country 2
d2 <- D2 %>% group_by(main_sector) %>% summarise(def = sum(raised_amount_usd))
summarise(d2, sum(def))
#top sector name (no. of investment-wise)
top <- arrange(d1, desc(abc))
head(top[1,1])
#Second sector name (no. of investment-wise)
head(top[2,1])
#Third sector name (no. of investment-wise)
head(top[3,1])
#number of investments in top sector
head(top[1,2])
#number of investments in second sector
head(top[2,2])
#number of investments in third sector
head(top[3,2])
d1<-NULL
d2<-NULL
top <- NULL
#Top sector count-wise, which company received the highest investment?
temp <- filter(D2, main_sector=="Others")
temp <- arrange(temp, desc(total_raised_amount_usd))
head(temp[1,2])
temp<-NULL
#second best sector count-wise, which company received the highest investment?
temp <- filter(D2, main_sector=="Social..Finance..Analytics..Advertising")
temp <- arrange(temp, desc(total_raised_amount_usd))
head(temp[1,2])
temp<-NULL

#3. country=IND, funding type=venture
Df3 <- filter(master_frame, country_code=="IND" & funding_round_type=="venture" & total_raised_amount_usd >= 5000000 & total_raised_amount_usd <= 15000000) #apply filter country=IND, funding type=venture in master_frame and usd invested between 5M & 15M
D3_temp <- aggregate(Df3$main_sector, by=list(Df3$main_sector), FUN=length) #count no. of investments
colnames(D3_temp)[1] <- "main_sector" #rename column to main_sector
colnames(D3_temp)[2] <- "count_of_investments" #rename column to conut of investments
D3_temp1 <- aggregate(Df3$raised_amount_usd, by=list(Df3$main_sector), FUN=sum) #identify total amount invested in country=IND and funding type=venture
colnames(D3_temp1)[1] <- "main_sector" #rename column to main_sector
colnames(D3_temp1)[2] <- "amount_invested" #rename column to amount invested
D3<-merge(D3_temp, D3_temp1, by="main_sector") #Final Step: create D3 df
D3<-merge(Df3, D3, by = "main_sector")
Df3<-NULL
D3_temp<-NULL
D3_temp1<-NULL

library(dplyr)
#total number of investments in country 1
d1 <- D3 %>% group_by(main_sector) %>% summarise(abc = length(count_of_investments)) 
summarise(d1, sum(abc))
#total amount invested in country 1
d2 <- D3 %>% group_by(main_sector) %>% summarise(def = sum(raised_amount_usd))
summarise(d2, sum(def))
#top sector name (no. of investment-wise)
top <- arrange(d1, desc(abc))
head(top[1,1])
#Second sector name (no. of investment-wise)
head(top[2,1])
#Third sector name (no. of investment-wise)
head(top[3,1])
#number of investments in top sector
head(top[1,2])
#number of investments in second sector
head(top[2,2])
#number of investments in third sector
head(top[3,2])
d1<-NULL
d2<-NULL
top <- NULL
#Top sector count-wise, which company received the highest investment?
temp <- filter(D3, main_sector=="Others")
temp <- arrange(temp, desc(total_raised_amount_usd))
head(temp[1,2])
temp<-NULL
#second best sector count-wise, which company received the highest investment?
temp <- filter(D3, main_sector=="News..Search.and.Messaging")
temp <- arrange(temp, desc(total_raised_amount_usd))
head(temp[1,2])
temp<-NULL

#export master_frame to a file for checkpoint-6
new_master_frame <- rbind(D1, D2)
new_master_frame <- rbind(new_master_frame, D3)
write.csv(new_master_frame, "new_master_frame.csv")