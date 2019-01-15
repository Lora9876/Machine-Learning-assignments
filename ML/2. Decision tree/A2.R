#install.packages("tree")
library(rpart)
#uploading the csv file into second
info<-second
str(info)

#deleting empty cells 
sapply(info, function(x) sum(is.na(x)))
info<-info[complete.cases(info),]

library(plyr)

#fixing cells so they don't have different values for the same thing (No phone service = No )
info$MultipleLines <- as.factor(mapvalues(info$MultipleLines,  from=c("No phone service"), to=c("No")))

info$OnlineSecurity <- as.factor(mapvalues(info$OnlineSecurity,  from=c("No internet service"), to=c("No")))
info$OnlineBackup <- as.factor(mapvalues(info$OnlineBackup,  from=c("No internet service"), to=c("No")))
info$DeviceProtection <- as.factor(mapvalues(info$DeviceProtection,  from=c("No internet service"), to=c("No")))
info$TechSupport <- as.factor(mapvalues(info$TechSupport,  from=c("No internet service"), to=c("No")))
info$StreamingTV <- as.factor(mapvalues(info$StreamingTV,  from=c("No internet service"), to=c("No")))
info$StreamingMovies <- as.factor(mapvalues(info$StreamingMovies,  from=c("No internet service"), to=c("No")))

#function for making classes for tenure
group <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('first year')
  }else if(tenure > 12 & tenure <= 24){
    return('second year')
  }else if (tenure > 24){
    return('old costumer')
  }
}

info$tenure_1 = sapply (info$tenure, group)
info$tenure_1 = as.factor(info$tenure_1)

#fixing remaining data
info$SeniorCitizen <- as.factor(mapvalues(info$SeniorCitizen,  from=c("0","1"), to=c("No", "Yes")))
info$Churn <- as.factor(mapvalues(info$Churn,    from=c("Yes","No"),   to=c("Left", "Stayed")))
info$tenure <-NULL
info$customerID <-NULL

#dividing data into 2 parts for training the tree and testing 
part<- createDataPartition(info$Churn,p=0.8,list=FALSE)
training<-info[part, ]
testing <- info[-part,]
dim(training)
dim(testing)

#libraries for making to plot easier to study 
library(RColorBrewer)	
library(rattle)	
library(caret)
library(rpart.plot)

#representation of the trees 
fit<-rpart(Churn~ gender+SeniorCitizen+Partner+Dependents+tenure_1,data= training,method="class", control=rpart.control(minsplit=20,cp=0))
fancyRpartPlot(fit)
fit1<-rpart(Churn~ Contract+MonthlyCharges+PaperlessBilling+TotalCharges+PaymentMethod, training,method="class", control=rpart.control(minsplit=20,cp=0))
fancyRpartPlot(fit1)
summary(fit1)
fit1<-rpart(Churn~ Contract+MonthlyCharges+TotalCharges, training,method="class")
fancyRpartPlot(fit1)
summary(fit1)
fit2<-rpart(Churn~PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV+StreamingMovies,training,method="class", control=rpart.control(minsplit=20,cp=0))
fancyRpartPlot(fit2)
summary(fit2)
fit2<-rpart(Churn~InternetService+OnlineSecurity+OnlineBackup+TechSupport,training,method="class")
summary(fit2)
fancyRpartPlot(fit2)


#final tree
finalfit<-rpart(Churn~tenure_1+ OnlineSecurity+TechSupport+ OnlineBackup+Contract+MonthlyCharges+SeniorCitizen,data=training,method="class",control=rpart.control(minsplit=20,cp=0))
fancyRpartPlot(finalfit)
summary(finalfit)
finalfit<-rpart(Churn~tenure_1+Contract+MonthlyCharges,data=training,method="class")
fancyRpartPlot(finalfit)
summary(finalfit)

#evaluation of the tree
fit_end<-rpart(Churn~ .,data= training,method="class", control=rpart.control(minsplit=20,cp=0))
p2<-predict(fit_end,testing)
p3<-(c(testing$Churn, testing$Churn))
tab2<-table(Predicted=p2, p3)
print(sum(diag(tab2))/ sum(tab2))
