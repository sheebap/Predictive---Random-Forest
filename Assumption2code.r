#Installing Packages and libraray

install.packages("dplyr")
install.packages("class")
install.packages("zipcode")
install.packages("data.table")
install.packages("caret")
install.packages("rpart.plot")
install.packages("zoo")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("e1071")
install.packages("rpart")
install.packages("randomForest")
install.packages("ggmap")
install.packages("tidyr")
install.packages("C50")
install.packages("CHAID")
install.packages("usmap")
install.packages("tidyverse")
library(usmap)
library(tidyverse)
library(class)
library(CHAID)
library(ggmap)
library(tidyr)
library(C50)
library(dplyr)
library(data.table)
library(zipcode)
library(caret)
library(rpart.plot)
library(zoo)
library(ggplot2)
library(e1071)
library(rpart)
library(reshape2)
library(randomForest)

#Reading the files Transaction History, Dimstore and Us_zipcodes

TransactionHistory <- read.delim("~/TransactionHistory.txt")
DimStore <- read.delim("~/DimStore.txt")
library(readxl)
us_zipcodes <- read_excel("~/us_zipcodes.xlsx")
View(us_zipcodes)

#creating a subset from the us_zipcode files considering only the columns which are important as per the analysis
#Here Only Population (as depending on that the sales can be increased or decreased), HouseholdsPerZipcode(As no of the houses will affect the sales) and IncomePerHousehold(Income is one of the most important factor as per the data the number of transaction increases as the Income increases)
us_zipcodes <- subset(us_zipcodes, select = c("ZipCode","Population", "HouseholdsPerZipcode","IncomePerHousehold" ))
us_zipcodes$ZipCode <- as.character(us_zipcodes$ZipCode) #converting zipcodes in file from char to factors
summary(us_zipcodes)
data("zipcode")

#Joining the Transaction History with Zipcode in order to get all the columns in the zipcode package in transaction history by using ShipToZipcode as key vallue

NewTransactionHistory <- inner_join(TransactionHistory, zipcode, by=c("ShipToZipCode" = "zip"))

#Converting the Date column into more convinient form such as moth and year.
datetxt <- c(NewTransactionHistory$TheDate)
datetxt <- as.Date(datetxt)
datetxt <- as.Date(NewTransactionHistory$TheDate)

df <- data.frame(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))
df$x <- paste(df$month, "/", df$year)
setnames(us_zipcodes, "ZipCode", "ShipToZipCode")

#merging the date coverted with the NewTransactionHistory dataframe which we created above
Test1 <- merge(NewTransactionHistory, df,by="row.names",all.x = TRUE )
Test1 = subset(Test1, select = -c(Row.names))
Test1 = subset(Test1, select = -c(month))
Test1 = subset(Test1, select = -c(day))
NewTransactionHistory <- Test1
NewTransactionHistory = subset(NewTransactionHistory, select = -c(date))


#merging the date coverted with the TransactionHistory dataframe which we created above

datetxt2 <- c(TransactionHistory$TheDate )
datetxt2 <- as.Date(datetxt2)
datetxt2 <- as.Date(TransactionHistory$TheDate)
df2 <- data.frame(date = datetxt2,
                  year = as.numeric(format(datetxt2, format = "%Y")),
                  month = as.numeric(format(datetxt2, format = "%m")),
                  day = as.numeric(format(datetxt2, format = "%d")))
df2$x <- paste(df$month, "/", df2$year)
Test2 <- merge(TransactionHistory, df2,by="row.names",all.x = TRUE )
Test2 = subset(Test2, select = -c(Row.names))
Test2 = subset(Test2, select = -c(month))#Converting the Date column into more convinient form such as moth and year.
Test2 = subset(Test2, select = -c(day))
TransactionHistory1 <- Test2
TransactionHistory1 = subset(TransactionHistory1, select = -c(date))


#Replacing the blank spaces in the Dataframe with "NA" values in order to detect the blank spaces and then replace it
TransactionHistory1$ShipToZipCode <- sub("^$", "NA", TransactionHistory1$ShipToZipCode)

#Creating the new dataset  as Retail (Brick and Motors store) from Transaction History based on the condition where ShipToZipCode is NULL
Retail <- subset(TransactionHistory1, ShipToZipCode =="NA")

#Looking up the data in Retail dataset and replace the NA value under ShipToZipCode with the respective vale in Dimstore datset
Retail$ShipToZipCode <- DimStore[match(Retail$InternationalStoreCode, DimStore$InternationalStoreCode),3]


#Creating the new dataset as Ecom (Online shopping) from NewTransactionHistory based on the condition where ShipToZipCode is not equal to NULL
Ecom <- subset(NewTransactionHistory, ShipToZipCode != "NA")


#Aggregating the data in Retail based on Date, Transaction Type and ShiptoZipcode. Which gives us more aggregated data and we dont loose the granularity 
AggRetail <- Retail %>% group_by(x,Transaction.Type,ShipToZipCode) %>% summarise(Totaltrans = sum(Transactions))
setnames(AggRetail, "x", "Date") # Change the name of date column from x to Date

#Joining the Aggregated data from above with Zipcode using inner join in order to get all the columns from Zipcode package
AggRetail <- inner_join(AggRetail, zipcode, by=c("ShipToZipCode" = "zip"))
summary(AggRetail)
str(AggRetail)

#Aggregating the Ecom dataset with Date, Transaction type, latitude and longitude. As ultimately items are shipped from some of the stores.
AggEcom <- Ecom %>% group_by(x,Transaction.Type,latitude, longitude, ShipToZipCode) %>% summarise(Totaltrans = sum(Transactions))
setnames(AggEcom, "x", "Date") # Change the name of date column from x to Date


#Considering the group by on international store code as well and summary in consideration, omitting the zipcodes where transcation from store is less than 5 considering from which store code the item is shipped more
AggEcom <- left_join(AggEcom,  us_zipcodes, by = c("ShipToZipCode"="ShipToZipCode"))
summary(AggEcom)
str(AggEcom)

#Creating a final dataset from the Retail depending on the conditions. Here we are taking in consideration that "5" is the mean for the total transaction and we are taking Incom as well in consideration
#Based on the summary we are taking 5 for transaction
#After taking a look at data of retail and the mean is 5 which in this case is considered  minimum requirement to label a store.
#If the transaction is below  it is given poor rating to open a store. The zipcode where trascaion is between  to 1000 is considered as best to open the store
#If the transaction is above 1000 it is the best location to open store.
#Nested if condition is used to determine this values.
#If the status for New Store is poor that means the transactions are below 5 and not giving that much profit as compared to other
# So it wont be a good deal to open a store in this case
#The best Option to open a store will be for the transactions whose status is Best but the zipcodes with good status can also be taken in consideration.

EcomFinal <- ifelse((AggEcom$Totaltrans <= 5), "POOR", 
                    ifelse ((AggEcom$Totaltrans >5 & AggEcom$Totaltrans <= 1000), "BEST","GOOD")) 

#Merging the status data created with Aggregated datset in order to get a final dataset
EcomFinal <- merge(AggEcom , EcomFinal,by="row.names", all.x = TRUE )
setnames(EcomFinal, "y", "Status")
EcomFinal$Status<- as.factor(EcomFinal$Status)#converting Status to factor

#Exploring data with various plots for EcomFinal
boxplot(EcomFinal)
plot(EcomFinal$Population, EcomFinal$Totaltrans)
      

plot(EcomFinal$IncomePerHousehold, EcomFinal$Totaltrans)

#Dropping the column which are not as useful.
EcomFinal$InternationalStoreCode = NULL
EcomFinal$Date = NULL
EcomFinal$ShipToZipCode <- as.factor(EcomFinal$ShipToZipCode)
EcomFinal$Row.names = NULL
EcomFinal <- na.omit(EcomFinal)
setnames(EcomFinal, "Totaltrans", "NewStore")
summary(EcomFinal)
str(EcomFinal)




#Creating Training and Testing dataframes from the EcomFinal. Dividing the data into test and train
SampleData <- EcomFinal[sample(nrow(EcomFinal),100, replace = FALSE, prob= NULL),]
last <- nrow(SampleData)
mid <- round(last * 0.70)
Training <- SampleData[1:mid,]
Testing <- SampleData[mid:last,]



#CART
#The decision tree method is a powerful and popular predictive machine learning technique that is used for both classification and regression. So, it is also known as Classification and Regression Trees (CART).
#Note that the R implementation of the CART algorithm is called RPART (Recursive Partitioning And Regression Trees) available in a package of the same name.
#Deriving the Accuracy by CART method
CartTree <- rpart(NewStore ~.,data = Training,method = "class"  )
rpart.plot(CartTree, type = 3, digits = 3, fallen.leaves = TRUE )
table(Testing$NewStore, predict(CartTree, newdata = Testing, type = "class") )
Cart <- predict(CartTree, newdata = Testing, type = "class")
confusion_MatrixCart = confusionMatrix(Testing$NewStore, Cart)
confusion_MatrixCart

#Here the accuracy is 100% which should be less but it is more due to corelated factors. Even though Income and total transaction are taken in consideration.


#C50
#Deriving the Accuracy by CART method
set.seed(9850)
g <- runif(nrow(EcomFinal))
Final1r <- EcomFinal[order(g),]
head(Training)
Model2 <- C5.0(x = Training[,-c(8)],y = Training$NewStore)
summary(Model2)
plot(Model2)
C50 <- predict(Model2, Testing)
confusion_MatriC50 = confusionMatrix(Testing$NewStore, C50)
confusion_MatriC50

#CHAID
#Deriving the Accuracy by CART method
#Decision Tree Algorithm - CHAID - is explained with an example and you can access the details here CHAID In the brief blog, we are sharing R code and steps to get CHAID based decision tree for a dataset. CHAID using R
#Dropping the fields which are unnecessary.

EcomFinal$Transaction.Type = NULL
EcomFinal$HouseholdsPerZipcode = NULL
EcomFinal$IncomePerHousehold = NULL
EcomFinal$Population = NULL
EcomFinal$Totaltrans = NULL
EcomFinal$ShipToZipCode = NULL
EcomFinal$TotalTrans = NULL
#Converting remaining columns in factors as CHAID only workd with factors
EcomFinal$latitude <- as.factor(EcomFinal$latitude)
EcomFinal$longitude <- as.factor(EcomFinal$longitude)


#SampleData <- EcomFinal[sample(nrow(EcomFinal),100, replace = FALSE, prob= NULL),]
#SampleData1 <- SampleData
#SampleData1$latitude <- as.factor(SampleData1$latitude)
#SampleData1$longitude <- as.factor(SampleData1$longitude)

#Creating Train and Test Data
last1 <- nrow(EcomFinal)
mid1 <- round(last1 * 0.70)
Training1 <- EcomFinal[1:mid1,]
Testing1 <- EcomFinal[mid1:last1,]

#Creating Moded by Chaid
ChaidTree <- chaid(NewStore ~.,data = Training1)
ChaidTree

#Plotting Chaid model
plot(ChaidTree)

#Predicting Data
predictionChaid <- predict(ChaidTree, Testing1, na.action= na.pass)

#Plotting Chaid prediction
plot(predictionChaid)
TableChaid <- table(Testing1$NewStore, predictionChaid)
TableChaid

#Calculating Confusion Matrix
Confusion_MatrixCHAID = confusionMatrix(Testing1$NewStore, predictionChaid)
Confusion_MatrixCHAID

#Random Forest
#Random Forest is one such very powerful ensembling machine learning algorithm which works by creating multiple decision trees and then combining the output generated by each of the decision trees.
#Decision tree is a classification model which works on the concept of information gain at every node. 
#Deriving the Accuracy by Random Forest method

EcomFinal$latitude <- as.numeric(EcomFinal$latitude)
EcomFinal$longitude <- as.numeric(EcomFinal$longitude)
EcomFinal$Row.names = NULL
EcomFinal$Status = NULL
str(EcomFinal)
#Generating Test and Train Data
last2 <- nrow(EcomFinal)
mid2 <- round(last2 * 0.70)
Training2 <- EcomFinal[1:mid2,]
Testing2 <- EcomFinal[mid2:last2,]

#Fitting the Random Forest model 
fit_rf<-randomForest(NewStore~.,
                     data=Training2,
                     importance=TRUE,
                     prOximity=TRUE,
                     na.action=na.roughfix)
fit_rf #Viewing the model

#plotting the model with training data 
plot(fit_rf)

#Creating a predictor using testing
pred <- predict(fit_rf, newdata = Testing2, type = "class")

#Checking length of the Predictor
length(pred)
test= as.data.frame(Testing2)
head(test)

#Creating Confusion Matrix to check the accuracy
Confusion_Matrix= confusionMatrix(test$NewStore, pred)
Confusion_Matrix

#The Accuracy for Random forest is 76.53% which is pretty good and there is no overfitting for best and POOR.
#But still for GOOD there is overfitting as there is no true negative or False positive. 
#We are considering Best values for the first in consideration to open a store and then the good ones. Poor ones are where no need of new store is needed but might need some more investment to for generating more revenue

