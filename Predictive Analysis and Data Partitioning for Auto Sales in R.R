#@Author: Thomas Sorenson
##Description: This script illustrates predictive analysis techniques for the auto Sales Dataset. This file is great for instructing how to partition data and perform basic predictive analytics in R

#First Im going to install the car package
install.packages("car")
library(car)



## First im doing to read the Auto dataset into R. 
##Read the data set into R. 


car_sales= read.csv("I:/Analysis6100/Data Sets/Car-Data.csv")

##Now we need a Dummy variable for whether a car is a bad buy or not.
##Convert the BadBuy variable into a dummy variable,where 1 is a bad purchase (a “bad buy”) and 0 is not. 

car_sales$BadBuy= ifelse(car_sales$BadBuy=="YES",1,0)


##Now we need to set a seed to allow for our work to be replicated in future analysis.
##Set the seed to 750 


set.seed(750)

##ii. split the data set into a 60% training data set and a 40% testing data set.


##first find the number of observations
num_obs = nrow(car_sales)

##sample for training data and create new dataset
train_obs= sample(num_obs,0.60*num_obs)
car_train= car_sales[train_obs,]

##Then put the rest of the dataset into a new testing dataset
car_test= car_sales[-train_obs,]


##Now lets look at how well we partitioned the data
##Construct and report boxplots of the 
##(1) auction prices for the cars and 
##(2) ages of the cars, where both boxplots are broken out by cars that are bad buys vs. cars that are not.



##i.	Which data set should you use to do this, training data or testing data? Explain.
#Answer: We would want to use our training dataset as we want to get unbiased look at how well our model fits when testing our model 


colnames(car_sales)

#x variable in boxplot is always categorical
#Y variable in boxplot is always continuous
#boxplot(X~y)


boxplot(car_train$Age~car_train$BadBuy,
ylab= "Bad Buy",
xlab= "Age",
main= "Cars by Age"
)

boxplot(car_train$MMRAauction~car_train$BadBuy,
ylab= "Bad Buy",
xlab= "Auction Price",
main= "Cars by Auction Price"
)


## ii.	Does it appear that there is a relationship between either of these numerical variables and being a bad buy? Explain using the boxplots you created.
#Answer: Yes, through the box plot we can clearly see a different mean for both variables when seperated by Bad buy and not bad bad buy. Clearly both these numeric variables have some relationship with whether a car is a bad buy

##If we were to predict every car to be a bad buy without running any modeling at all:
##i.	What would our accuracy be on the testing data?
#Answer:
#First we need to find the total number of bad buys = 1
#Then we find the number of cars in the dataset that we predict to be badbuy=1, in this case all of them
#Then we divide our actual badbuys by our predicted bad buys

num_badbuy = sum(car_test$BadBuy)
num_predict= nrow(car_test)
accuracy_nomodel = num_badbuy/num_predict  
accuracy_nomodel

 #Our Accuracy would be about 0.502 which is pretty close to a 50%  accuracy.

## ii.	What would our RMSE be on the testing data?
#answer:

#create RMSE Function
RMSE_calc = function(predicted=null, actual=null){
RMSE = sqrt(mean((predicted-actual)^2))
return(RMSE)
}

#create mean of our cart test badbuy
car_mean = mean(car_test$BadBuy)

#calculate RMSE using the prediction we made and the sum of correct predictions, in this case the sume of 1==1
RMSE_mean_predict = RMSE_calc(car_mean, car_test$BadBuy)
RMSE_mean_predict
#The RMSE for this testing data would be 0.499


### - Linear Regression

## a.	Using the training data set, run a linear regression model to predict a bad buy using all other variables and report the summary regression output. Do you think this model will be useful for prediction? Support your answer. 

#Answer:
car.fit=lm(BadBuy~.,data=car_train)
summary(car.fit) 


##Check predictions
lin_predict_train =  predict(car.fit, newdata=car_train)
lin_predict_test = predict(car.fit,newdata=car_test)

#check RMSE to evaluate how good this model is for prediction
#Now calculate RMSE
RMSE_test = sqrt(mean((lin_predict_train-car_train$BadBuy)^2))
RMSE_test
# RMSE = 0.448
#We cannot determine if it is useful for prediction until we compare it to another model's RMSE


##2b. based on the type of model we are running here (a linear regression to predict a categorical outcome), would you expect the residual plots to show issues with this model? For each type of residual plot below, say whether you would or would not expect issues and explain why:
#i.	Normal Q-Q Plot
#answer: Because our data contains just 1 and 0 you will not see any  varying distribution all of our data points will be close together
#ii.	Standard Residual Plot (“Residuals vs Fitted” plot)
#Answer: We will not be able to see a normal distribution because points will be clustered  in groups of 1 and 0
#iii.	Leverage Plot (“Residuals vs Leverage” plot)
#Answer: There will not be any outliers in our y variables as they are only in terms of 1 or 0

##Question 2c
##	Run a partial R^2 analysis for the vehicle odometer reading variable.
#Answer:
#A partial R^2 helps see how much of an x variable is explained by the other x variables in the model
#partial R2 is created the same as a linear regression with a y variable

car.partial = lm(Odo~. -BadBuy, data = car_train)
summary(car.partial)


#ii.##	What is the VIF for this variable?
#Answer:

vif(car.partial)


#iii.##	Do either the R^2 or VIF values indicate an issue for this variable? Explain.
#Answer: Yes, the VIF shows there may be some multicollinearity issues as MMRAuction is over 10 and we have some that are between 5-10 which indicate potential problems. Our partial R2 is way under 0.80 so there may be no problems with the variables as a group in the model

##Compute and report the predicted “probability” that the following car is a bad buy:


#Auction="ADESA"
#Age=1
#Make="HONDA"
#Color="SILVER"
#WheelType="Covers"
#Odo=10000
#Size="LARGE"
#MMRAauction=8000
#MMRAretail=10000

#Answer:

new_car = data.frame(
Auction="ADESA", 
Age=1,
Make="HONDA",
Color="SILVER",
WheelType="Covers",
Odo=10000,
Size="LARGE",
MMRAauction=8000,
MMRAretail=10000
)
predict(car.fit,newdata=new_car)
#Does your prediction make sense?
#Answer:  No in linear regression the Y variable output cannot be interpreted for categorical y variables

#	Compute and report the RMSE for your model on both the training and the testing data. Does overfitting appear to be a severe problem here? Explain.

#first, run predictions for the testing data and training data
lin_predict_train =  predict(car.fit,newdata=car_train)
lin_predict_test <- predict(car.fit,newdata= car_test)

#then, calculate the RMSE for both data sets
car_train_RMSE = RMSE_calc(lin_predict_train, car_train$BadBuy)
car_test_RMSE = RMSE_calc(lin_predict_test, car_test$BadBuy)  
car_train_RMSE
car_test_RMSE

#our training RMSE and testing RMSE are very close to one another.
#If our training data was much lower than our testing data we may have a overfitting issue

##2f. Compare your answer in part (e) to your answer in the second part of question 1, part (d). Does it appear your model is useful for prediction after all? Explain.
#answer:  In part D we had an RMSE from predicting that every car was bad of 0.4999
#our model gave us an RMSE of 0.45  which tells us that our model is more useful for prediction than merely predicting every car is bad.


#2G: Now use a cutoff of 0.5 and classify the predictions for your model as a badbuy if the prediction is greater than 0.5 and not a badbuy if the prediction is less than 0.5.
#Answer:

#First create a cutoff, in this case a cutoff of 0.5
class_BadBuy = ifelse(lin_predict_test> 0.5, 1, 0) 

#i.	Use these new classification values to recalculate testing data RMSE. Does treating this problem as a classification improve your predictions? Explain.
#answer:

#we create new RMSE by inputing our original prediction and our new classifications
RMSE_class_test = RMSE_calc(class_BadBuy, car_test$BadBuy )
RMSE_class_test
#our new RMSE with our classifications reduced significantly which suggests that this new model is better for prediction

#ii.	Does your answer to (i) above surprise you? Why or why not?
#Answer: I am not sure. Our RMSE rose significantly. Perhaps doesnt surprise me because the linear model cannot provide predictions of categorical variables


###  Inference vs. Prediction 

#Consider the following three possible linear regression models:

#(i)	Predict BadBuy using vehicle age and vehicle odometer reading

car.odo = lm(BadBuy~Odo +Age, data=car_train)
summary(car.odo) 

car.predict.odo = predict(car.odo, newdata= car_test)
RMSE_car_odo = RMSE_calc(car.predict.odo, car_test$BadBuy)
RMSE_car_odo


#(ii)	Predict BadBuy using vehicle age, vehicle odometer reading, and type of wheel

car.odo2 = lm(BadBuy~Odo +Age +WheelType, data=car_train)
summary(car.odo2) 

car.predict.odo2 = predict(car.odo2, newdata= car_test)
RMSE_car_odo2 = RMSE_calc(car.predict.odo2, car_test$BadBuy)
RMSE_car_odo2



#(iii)	Predict BadBuy using vehicle age, vehicle odometer reading, type of wheel, and the size of the vehicle

car.odo3 = lm(BadBuy~Odo +Age +WheelType +Size, data=car_train)
summary(car.odo3) 

car.predict.odo3 = predict(car.odo3, newdata= car_test)
RMSE_car_odo3 = RMSE_calc(car.predict.odo3, car_test$BadBuy)
RMSE_car_odo3
#answer: I believe that model car.odo3 is best for inference and prediction. Our Adjusted R2 is very slightly higher in this model which tells us that it is slightly better for inference. At the same time, car.odo3 also has a lower RMSE than the other models, which tells us that it is slightly better for prediction as well.


