
#real estate project

estate_train = read.csv(file='C:/Users/DELL/Documents/R study/R materials by udemy/Files/Data/Edvancer projects/Project 1 Real_Estate/datasets_real_estate/housing_train.csv',header=TRUE,stringsAsFactors = FALSE)

summary(estate_train)


estate_test = read.csv(file='C:/Users/DELL/Documents/R study/R materials by udemy/Files/Data/Edvancer projects/Project 1 Real_Estate/datasets_real_estate/housing_test.csv',header=TRUE,stringsAsFactors = FALSE)


summary(estate_test)

estate_test$Price = NA
#take price as NA 

#add an column to segregate between train and test data's as they are going to be combined later

estate_train$data= 'train'

estate_test$data= 'test'


estate_combine = rbind(estate_train,estate_test)

summary(estate_combine)




#install.package('dplyr')
library('dplyr')
library("dummies")


glimpse(estate_combine)

#data processing starts 

#identifying the categorical variables



CreateDummies = function(data,var,freq_cutoff=0)
{
  t= table(data[,var])
  
  t= t[t>freq_cutoff]
  t=sort(t)
  
  categories= names(t)[-1]
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL  #drop table
  return(data)
  
}


#Suburb
table(estate_combine$Suburb)  #as they are many suburbs then we will keep only those which are greater than certain frequency
estate_combine= CreateDummies(estate_combine,"Suburb",200)


#Address
View(table(estate_combine$Address))#as they are many suburbs then we will keep only those which are greater than certain frequency
#as all have frequency 1 so we need to drop it as its not going to change the model
estate_combine$Address = NULL


#Type
table(estate_combine$Type)
# h    t    u 
# 5916 1048 2457 
estate_combine <- cbind(estate_combine, dummy(estate_combine$Type))
estate_combine= estate_combine[,!names(estate_combine) %in% c("Type","estate_combineu")]


#Method
View(table(estate_combine$Method))
estate_combine <- cbind(estate_combine, dummy(estate_combine$Method))
estate_combine= estate_combine[,!names(estate_combine) %in% c("Method","estate_combineSA")]


#SellerG
table(estate_combine$SellerG)
estate_combine= CreateDummies(estate_combine,"SellerG",150)


#CouncilArea
table(estate_combine$CouncilArea)
estate_combine$CouncilArea[which(estate_combine$CouncilArea=="")]<-"Unknown" #to replace all the blank values to unknown. We should not remove this as it has a greater frequency
estate_combine= CreateDummies(estate_combine,"CouncilArea",400)



#treating for NA values

for(col in names(estate_combine))
{
  if(sum(is.na(estate_combine[,col]))>0 &&  !is.character(estate_combine[,col])&& !(col %in% c("Price","data")))
  {
    
    estate_combine[is.na(estate_combine[,col]),col]= mean(estate_combine[estate_combine$data=="train",col],na.rm=T)
    
  }
  
}



#checking for outliners



#Rooms
table(estate_combine$Rooms)
summary(estate_combine$Rooms)

boxplot(estate_combine$Rooms)  #after 5 rooms the frequency is very low so we need to treat it
room_quantile = 2 * quantile(estate_combine$Rooms,0.50)
estate_combine$Rooms[estate_combine$Rooms > room_quantile] = room_quantile

boxplot(estate_combine$Rooms)



#distance
summary(estate_combine$Distance)
boxplot(estate_combine$Distance) #distance is fine-- no need to implement outlier for it


#postcode
summary(estate_combine$Postcode)
boxplot(estate_combine$Postcode)
pairs(~Price+ Postcode, data = estate_combine)#postcode is fine-- no need to implement outlier for it


#Bedroom2
summary(estate_combine$Bedroom2)
boxplot(estate_combine$Bedroom2) #after scale of 5 in y axis there is an outlier--so we need to treat it
pairs(~Price+ Bedroom2, data = estate_combine)
table(estate_combine$Bedroom2)

bedroom_quantile =  2 * quantile(estate_combine$Bedroom2,0.90)
estate_combine$Bedroom2[estate_combine$Bedroom2 > bedroom_quantile] = bedroom_quantile
summary(estate_combine$Bedroom2)


#Bathroom
summary(estate_combine$Bathroom)
boxplot(estate_combine$Bathroom)
pairs(~Price+ Bathroom, data = estate_combine)
table(estate_combine$Bathroom)
bathroom_quantile =  2 * quantile(estate_combine$Bathroom,0.90)
estate_combine$Bathroom[estate_combine$Bathroom > bathroom_quantile] = bathroom_quantile

#Car
summary(estate_combine$Car)
boxplot(estate_combine$Car)
pairs(~Price+ Car, data = estate_combine)
Car_quantile =  2 * quantile(estate_combine$Car,0.90)
estate_combine$Car[estate_combine$Car > Car_quantile] = Car_quantile


#Landsize
summary(estate_combine$Landsize)
boxplot(estate_combine$Landsize)
pairs(~Price+ Landsize, data = estate_combine)
View(table(estate_combine$Landsize))
Landsize_quantile =  2 * quantile(estate_combine$Landsize,0.99)
estate_combine$Landsize[estate_combine$Landsize > Landsize_quantile] = Landsize_quantile


#BuildingArea
summary(estate_combine$BuildingArea)
boxplot(estate_combine$BuildingArea)
pairs(~Price+ BuildingArea, data = estate_combine)
BuildingArea_quantile =  2 * quantile(estate_combine$BuildingArea,0.99)
estate_combine$BuildingArea[estate_combine$BuildingArea > BuildingArea_quantile] = BuildingArea_quantile


#YearBuilt
summary(estate_combine$YearBuilt)
boxplot(estate_combine$YearBuilt)
pairs(~Price+ YearBuilt, data = estate_combine)
View(table(estate_combine$YearBuilt))
YearBuilt_quantile =  0.9 * quantile(estate_combine$YearBuilt,0.03)
estate_combine$YearBuilt[estate_combine$YearBuilt < YearBuilt_quantile] = YearBuilt_quantile



#segregating back train and test data
library('dplyr')

estate_train_data= estate_combine %>% filter(data=="train") %>% select(-data)


estate_test_data= estate_combine %>% filter(data=="test") %>% select(-data,-Price)




library("caTools")
set.seed(0)

split= sample.split(estate_train_data, SplitRatio = 0.8)

training_set= subset(estate_train_data,split==TRUE)

test_set = subset(estate_train_data,split==FALSE)


str(training_set)


#implement random forest for minimal RMSE

library(randomForest)

#implement random forest and find the value of mtry

randomfor1 <- randomForest(Price~., data = training_set,ntree=500)
print(randomfor1)

# Call:
#   randomForest(formula = Price ~ ., data = training_set, ntree = 500) 
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 13
# 
# Mean of squared residuals: 98699885796
# % Var explained: 76.81

mtry <- tuneRF(training_set[-2],training_set$Price, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
print(mtry)

#find best value of mtry
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(best.m)

rf <-randomForest(Price~.,data=training_set, mtry=best.m, importance=TRUE,ntree=500)
summary(rf)
#Predict Output 
test_set$random <- predict(rf, test_set)
MSE2random <- mean((test_set$random - test_set$Price)^2)



#with same value of mtry, now we need to work on entire training data for which we need to make the prediction

random_forest_final <- randomForest(Price~.,data=estate_train_data, mtry=best.m, importance=TRUE,ntree=500)
estate_test_data$random <- predict(random_forest_final, estate_test_data)


write.csv(estate_test_data$random,"result_price_analysis.csv",row.names=F)
