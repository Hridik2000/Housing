df <- read.csv("C:/Users/family/Desktop/Work/Housing/Housing.csv")
View(head(df))
#renaming the columns
colnames(df)<- c('Transaction_Date','House_Age','Distance_from_Metro','Number_of_Convience_Store','Lat','Long','Number_of_Bedrooms',
                         'House_size_sq.ft','House_Price')
# checking if the data is clean
sum(is.na(df))
sum(df == 0)
sum(duplicated(df))
sum(is.na(df))
# checking for correlation
View(cor(df, method = 'pearson'))
# after checking the correlation of the other fields with prices we can see that Number_of_convience_Store
# the lat and long are having a strong postive correlation 
# House_Age and Distance_from_Metro have strong negative correlation and the remaining one have a weak positive 
#corrleation to the housing price

#Creating a prediction model1 using all the values 
model1<- lm(House_Price ~ Transaction_Date + House_Age + Distance_from_Metro + Number_of_Convience_Store + Lat + Long + Number_of_Bedrooms +
                         House_size_sq.ft, data = df)
summary(model1)
# Looking at the Multiple R-squared = 0.5826 and Adjusted R-squared = 0.5743 
# F-statistic = 70.66
# Predicting the House Price based on model1
res1<- predict(model1)
# Creating the second prediciton model using the postively correlated variables only
model2<- lm(House_Price ~ Transaction_Date + Number_of_Convience_Store + Lat + Long + Number_of_Bedrooms +
              House_size_sq.ft+ House_Price, data = df)
summary(model2)
 # Multiple R-squared = 0.4885 and Adjusted R-squared = 0.481.
# F-statistic = 64.8
# we can also see that the significance of the model has decreased compared to model 1
# Predicting the House Price based on model2
res2<- predict(model2)
# Creating a third prediction model using the negatively correlated variables
model3<- lm(House_Price ~ House_Age + Distance_from_Metro , data = df)
summary(model3)
# Multiple R-squared = 0.4911 and Adjusted R-squared = 0.4887 
# F-statistic = 198.3
# Predicting the House Price based on model3
res3<- predict(model3)
# Creating model4 by using highly correlated variables
model4<- lm(House_Price ~ Number_of_Convience_Store + Lat + Long, data = df)
summary(model4)
# Multiple R-squared = 0.481 and Adjusted R-squared = 0.4772 
# F-statistic = 126.6
# Predicting the House Price based on model4
res4<- predict(model4)

# Creating model5 by using the variables with high P-value
model5<- lm(House_Price~House_Age + Distance_from_Metro + Number_of_Convience_Store, data = df)
summary(model5)
# Multiple R-squared = 0.5411 and Adjusted R-squared = 0.0.5377 
# F-statistic = 161.1
# Predicting the House Price based on model5
res5<- predict(model5)
head(res1)
head(res2)
head(res3)     
head(res4)
head(res5)
head(df$House_Price)
# looking at the prediction prices the price of model1 and model2
# are the closet so they would be the recommended models