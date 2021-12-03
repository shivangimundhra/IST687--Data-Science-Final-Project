library(tidyverse)
resortData <- read_csv("https://intro-datascience.s3.us-east-2.amazonaws.com/Resort01.csv")


#Trying to make a map showing the 
#percent of reservations that are cancelled by country
countrydata<-resortData %>% group_by(Country) %>% 
  summarise(cancellations = sum(IsCanceled),
            total = n(),
            cancel_pct = cancellations/total)

#removing countries with less than 10 total reservations
map<-countrydata %>% filter(total>=10)

#install.packages("rworldmap")
library(rworldmap)
map_data<- map %>% 
  joinCountryData2Map(joinCode = "ISO3",
                      nameJoinColumn = "Country")
library(RColorBrewer)
#creating map darker = more likely to cancel
mapCountryData(mapToPlot = map_data,
               nameColumnToPlot = "cancel_pct",
               colourPalette = c("#ffffcc",
                 "#a1dab4",
                 "#41b6c4",
                 "#2c7fb8",
                 "#253494"),
               mapTitle = "Cancellation rate for countries with > 10 reservations")


#making factor variable for cancelled status
resortData$canceledStatus <- ifelse(resortData$IsCanceled==1,
                                    "canceled","not canceled")

#running regression to understand why people cancel 
#so i want iscanceled to be the y variable 
#looking at how different variables are related to iscanceled 

#lead time
ggplot(resortData,aes(x=canceledStatus,y=LeadTime,
                      fill=canceledStatus))+
  geom_boxplot() +
  ggtitle("Boxplot of lead time by Canceled Status") +
  scale_fill_brewer(palette="Blues") + theme_classic()
#appears that canceled reservations have longer lead time on average
#so potentially require deposits for very far away reservations


#does length of stay effect cancellations
#creating length of stay variable
resortData<-resortData %>% 
  mutate(stay_length = StaysInWeekendNights+StaysInWeekNights)

ggplot(resortData,aes(x=canceledStatus,y=stay_length,
                      fill=canceledStatus))+
  geom_boxplot() +
  ggtitle("Boxplot of length of stay by cancelled status") +
  scale_fill_brewer(palette="Blues") + theme_classic()

resortData %>% group_by(canceledStatus) %>% 
  summarise(avg = mean(stay_length),
            median = median(stay_length))
#seems that longer stays are slightly more likely to cancel

#does size of group affect cancellations
#creating total in group variable
resortData<-resortData %>% 
  mutate(total_in_party = 
           Adults+Children+Babies)

ggplot(resortData,aes(x=canceledStatus,y=total_in_party,
                      fill=total_in_party))+
  geom_boxplot() +
  ggtitle("Boxplot of total group size by cancelled status") +
  scale_fill_brewer(palette="Blues") + theme_classic()
#cant really tell anything from boxplot
#now looking only at children
ggplot(resortData,aes(x=canceledStatus,y=Children,
                      fill=Children))+
  geom_boxplot() +
  ggtitle("Boxplot of number of children by cancelled status") +
  scale_fill_brewer(palette="Blues") + theme_classic()
#both of these boxplots seem to have 
#too many outliers to be of much use
#I wouldn't use either in the report
resortData %>% group_by(canceledStatus) %>% 
  summarise(avg = mean(total_in_party),
            median = median(total_in_party))
resortData %>% group_by(canceledStatus) %>% 
  summarise(avg = mean(Children),
            median = median(Children))
#having children seems to make cancellation more likely 


#making character columns factors
resortData[sapply(resortData, is.character)] <- lapply(resortData[sapply(resortData, is.character)], 
                                       as.factor)
#meal type
#looking at cancellation percent for each meal type
resortData %>% group_by(Meal) %>% 
  summarise(cncl_pct = (sum(IsCanceled)/n()),
            number = n())
#overall cancellation percent for comparison
sum(resortData$IsCanceled)/nrow(resortData)

#full board and half board have above average cancellation rates

#now the same for market segment
resortData %>% group_by(MarketSegment) %>% 
  summarise(cncl_pct = (sum(IsCanceled)/n()),
            number = n())
#online travel agent and groups highest

#repeated guests
table(resortData$IsCanceled, resortData$IsRepeatedGuest)
#most people both dont cancel and arent repeated guests
#but repeat guests very rarely cancel


#previous cancellations
table(resortData$IsCanceled, resortData$PreviousCancellations)
table(resortData$IsCanceled, resortData$PreviousBookingsNotCanceled)

#people with previous cancellations often cancel and those who haven't 
#cancelled before in their previous reservations continue to not cancel as often

#room type
table(resortData$IsCanceled, resortData$ReservedRoomType)
table(resortData$IsCanceled, resortData$AssignedRoomType)
resortData$ReservedRoomType<-as.character(resortData$ReservedRoomType)
resortData$AssignedRoomType<-as.character(resortData$AssignedRoomType)

#making variable for if the room type chnaged
resortData$rm_type_change<-ifelse(resortData$ReservedRoomType==resortData$AssignedRoomType,
                                  0,1)

table(resortData$IsCanceled,resortData$rm_type_change)
#very interesting people whose room type gets changed almost 
#never cancel it must be that they only upgrade rooms and never downgrade

#booking changes
table(resortData$IsCanceled, resortData$BookingChanges)
#seems like people who change their booking dont cancel as much
#not sure why this is

#deposit type
resortData %>% group_by(DepositType) %>% 
  summarise(cncl_pct = (sum(IsCanceled)/n()),
            number = n(),
            cancels = sum(IsCanceled))

#that is so weird people who do non refundable deposits 
#almost always cancel, 96% of the time over a sample of 1719


#customer type
resortData %>% group_by(CustomerType) %>% 
  summarise(cncl_pct = (sum(IsCanceled)/n()),
            number = n(),
            cancels = sum(IsCanceled))
#transient non-party booking has highest rate
#while groups are very low

#parking spaces
table(resortData$IsCanceled, resortData$RequiredCarParkingSpaces)
#it must cost money to reserve parking spaces because literally 0
#people who reserved any number of parking spaces cancelled

#special requests
table(resortData$IsCanceled, resortData$TotalOfSpecialRequests)
#people who dont make any special requests have highest rate 
#of cancellations
7216/(7216+15145)


#going to make svm and rcart models

#getting data ready

resortModelData<-resortData[,-c(4,5,9,14,15,21)]

#changing repeated guest to factor
resortModelData$IsRepeatedGuest<-as.factor(resortModelData$IsRepeatedGuest)
#also iscanceled
resortModelData$IsCanceled<-as.factor(resortModelData$IsCanceled)

#making testing and training data sets
library(caret)
trainList<- createDataPartition(y = resortModelData$IsCanceled, p =.60,
                                list =F)
trainData<-resortModelData[trainList,]
testData<-resortModelData[-trainList,]

#svm model

svm.model <- train(IsCanceled ~ ., data = trainData,
                   method = "svmRadial",
                   trControl = trainControl(method = "none"),
                   preProcess = c("center", "scale"))

summary(svm.model)
svm.model

predictValues<-predict(svm.model, newdata =  testData)
confusionMatrix(predictValues, testData$IsCanceled)
#82% accuracy 


#rpart model now because its easier to explain

library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
#creating model
model.rpart <- rpart(IsCanceled ~ .,
                     data = trainData, method = "class")
#predicting with test data
predictValues<-predict(model.rpart, 
                       newdata = testData)
#making results of prediction into format for confusion matrix
p<-as.data.frame(predictValues)
p<-p %>% mutate(pred = ifelse(`0`>0.5,0,1))
p$pred<-as.factor(p$pred)
#confusion matrix accuracy 81.5
confusionMatrix(p$pred, testData$IsCanceled)
#looking at the cross validated error 
plotcp(model.rpart)
#pruning tree to make optimal decsion tree
ptree<- prune(model.rpart, cp= model.rpart$cptable[which.min(model.rpart$cptable[,"xerror"]),"CP"])
#visualizing tree
#definitely include this plot
rpart.plot(ptree, uniform=TRUE, main="Pruned Classification Tree")


printcp(ptree)
varImp(ptree)
#important variables are room type change, required parking spaces,
#market segment, leadtime, deposit type, previous cancellations,
#total special requests, customer type, booking changes, total in party,
#and previous booking not cancelled
cm<-confusionMatrix(p$pred, testData$IsCanceled)
cm$overall[1]
#pretty good accuracy 