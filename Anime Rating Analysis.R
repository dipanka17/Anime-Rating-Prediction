#####################################################################
####################### Anime Rating Prediction #####################
#####################################################################

##Step-1##
##The Objective here is to perform regression analysis and to develop a model that can be used to predict
## the ratings received by the enlisted anime releases, such that, in the future, the anime production studios
## can develop their strategies which can improve the rating.##

# Not allowing R to use scientific notation
options(scipen=999)

##Step-2##
Rating=read.csv("C:/Users/Dipanka/Desktop/R Internship Project/Anime_Final.csv", na.strings = c(""," ","NA","NULL", "[]"))
View(Rating)

##Step-3##
##Exploring the dataset: data exploratory commands gives the descriptive statistics of the data
str(Rating)

##Removing useless Column
UselessColumn= c("title","description","studios","tags")
Rating[,UselessColumn]= NULL
head(Rating,10)

##convert the categorical variables to factors##
factor_cols= c("mediaType","ongoing", "sznOfRelease", "contentWarn")

for (cat_cols in factor_cols){
  Rating[ , cat_cols]=as.factor(Rating[ , cat_cols])
}

str(Rating)

##Step-4##
summary(Rating)

## Looking at the Target variable its a problem of linear regression

##Step-5
## Treating of missing Values
colSums(is.na(Rating))

## sznofRelease and contentWarn column has to be removed because the NA value greater than >30% of the total
Rating$sznOfRelease= NULL
Rating$contentWarn= NULL
head(Rating,5)

##For Continious Variable##
##na.rm=TRUE means remove the NA values and then compute the median##
Rating$duration[is.na(Rating$duration)] = round(median(Rating$duration,na.rm=TRUE))
Rating$watched[is.na(Rating$watched)] =round(median(Rating$watched,na.rm=TRUE))

##For Categorical Variable##
##na.rm=TRUE means remove the NA values and then compute the mode##
table(Rating$mediaType)
Rating$mediaType[is.na(Rating$mediaType)]= "TV"

# Checking missing values after treatment
colSums(is.na(Rating))
summary(Rating)

##Step- 6
##Treatment Outliers

par(mfrow=c(1,1))
boxplot(Rating$rating, horizontal = T)
## There are no such outlier in rating column

##BoxPlot=eps
boxplot(Rating$eps, horizontal = T)

quantiles=quantile(Rating$eps,c(0.95,0.96,0.963,0.965,0.97,0.98,0.99,0.995,0.999,1))
quantiles

quantiles_final=round(quantile(Rating$eps,0.98))
quantiles_final
max(Rating$eps)

Rating$eps = ifelse(Rating$eps > quantiles_final , quantiles_final, Rating$eps)
#check the boxplot again and see whether outliers are removed or not.
boxplot(Rating$eps, horizontal = T)
max(Rating$eps)

##Boxplot= duration
boxplot(Rating$duration, horizontal = T)

quantiles2=quantile(Rating$duration,c(0.95,0.96,0.963,0.965,0.97,0.98,0.99,0.995,0.999,1))
quantiles2

quantiles_final2=round(quantile(Rating$duration,0.97))
quantiles_final2
max(Rating$duration)

Rating$duration = ifelse(Rating$duration > quantiles_final2 , quantiles_final2, Rating$duration)
#check the boxplot again and see whether outliers are removed or not.
boxplot(Rating$duration, horizontal = T)
max(Rating$duration)

##Boxplot= watched
boxplot(Rating$watched, horizontal = T)

quantiles3=quantile(Rating$watched,c(0.95,0.96,0.963,0.965,0.97,0.98,0.99,0.995,0.999,1))
quantiles3

quantiles_final3=round(quantile(Rating$watched,0.98))
quantiles_final3
max(Rating$watched)

Rating$watched = ifelse(Rating$watched > quantiles_final3 , quantiles_final3, Rating$watched)
#check the boxplot again and see whether outliers are removed or not.
boxplot(Rating$watched, horizontal = T)
max(Rating$watched)

##Boxplot= watching
boxplot(Rating$watching, horizontal = T)

quantiles4=quantile(Rating$watching,c(0.95,0.96,0.963,0.965,0.97,0.98,0.99,0.995,0.999,1))
quantiles4

quantiles_final4=round(quantile(Rating$watching,0.985))
quantiles_final4
max(Rating$watching)

Rating$watching = ifelse(Rating$watching > quantiles_final4, quantiles_final4, Rating$watching)
#check the boxplot again and see whether outliers are removed or not.
boxplot(Rating$watching, horizontal = T)
max(Rating$watching)

##Boxplot= wantWatch
boxplot(Rating$wantWatch, horizontal = T)

quantiles5=quantile(Rating$wantWatch,c(0.95,0.96,0.963,0.965,0.97,0.98,0.99,0.995,0.999,1))
quantiles5

quantiles_final5=round(quantile(Rating$wantWatch,0.99))
quantiles_final5
max(Rating$wantWatch)

Rating$wantWatch = ifelse(Rating$wantWatch > quantiles_final5, quantiles_final5, Rating$wantWatch)
#check the boxplot again and see whether outliers are removed or not.
boxplot(Rating$wantWatch, horizontal = T)
max(Rating$wantWatch)

##Boxplot= dropped
boxplot(Rating$dropped, horizontal = T)

quantiles6=quantile(Rating$dropped,c(0.95,0.96,0.963,0.965,0.97,0.98,0.99,0.995,0.999,1))
quantiles6

quantiles_final6=round(quantile(Rating$dropped,0.995))
quantiles_final6
max(Rating$dropped)

Rating$dropped = ifelse(Rating$dropped > quantiles_final6, quantiles_final6, Rating$dropped)
#check the boxplot again and see whether outliers are removed or not.
boxplot(Rating$dropped, horizontal = T)
max(Rating$dropped)

##Boxplot= votes
boxplot(Rating$votes, horizontal = T)

quantiles7=quantile(Rating$votes,c(0.95,0.96,0.963,0.965,0.97,0.98,0.99,0.995,0.999,1))
quantiles7

quantiles_final7=round(quantile(Rating$votes,0.99))
quantiles_final7
max(Rating$votes)

Rating$votes = ifelse(Rating$votes > quantiles_final7, quantiles_final7, Rating$votes)
#check the boxplot again and see whether outliers are removed or not.
boxplot(Rating$votes, horizontal = T)
max(Rating$votes)

##Step-7
# Explore each "Potential" predictor for distribution and Quality
# Exploring MULTIPLE CONTINUOUS features
ColsForHist= c("eps","duration","watched","watching","wantWatch","dropped", "rating", "votes")

#Splitting the plot window into six parts
par(mfrow=c(2,4))

# library to generate professional colors
library(RColorBrewer)

# looping to create the histograms for each column
for(hist_cols in ColsForHist){
  hist(Rating[, c(hist_cols)], main = paste("Histogram of:", hist_cols),
       col = brewer.pal(10,"Paired"))
}

## Exploring MULTIPLE CATEGORICAL features
ColsForBar= c("mediaType","ongoing")

## Splitting the plot window into 2 parts
par(mfrow=c(1,2))

for (cat_col in ColsForBar){
  barplot(table(Rating[,c(cat_col)]), main=paste('Barplot of:', cat_col), 
          col=brewer.pal(10,"Paired"))
}

##Step-8
##Bivariate analysis

# Visual Relationship between predictors and target variable
##Regression- 2 scenarios
# Continuous Vs Continuous ---- Scatter Plot
# Continuous Vs Categorical --- Box Plot

# Continuous Vs Continuous --- Scatter plot
# For multiple columns at once

ContinuousCols= c("eps","duration","watched","watching","wantWatch","dropped", "rating", "votes")
plot(Rating[, ContinuousCols], col='blue')


# Continuous Vs Categorical Visual analysis: Boxplot
Categorical_cols= c("mediaType","ongoing")

library(RColorBrewer) 

par(mfrow=c(1,2))

for (bar_cols in Categorical_cols){
  boxplot(rating~(Rating[,c(bar_cols)]), data = Rating, 
          main=paste('Box plot of:',bar_cols),col=brewer.pal(10,"Paired"))
}

##Step-9##
##Strength of Relationship between predictor and target variable
# Continuous Vs Continuous ---- Correlation test
# Continuous Vs Categorical---- ANOVA test


# Continuous Vs Continuous : Correlation analysis
# Correlation for multiple columns at once

CorrData= cor(Rating[,ContinuousCols], use = "complete.obs")
CorrData
data.frame(CorrData[,'rating'])

# Final continuous columns to be selected for modeling
names(CorrData[,'rating'][abs(CorrData[,'rating'])>0.4])

# Selecting below continuous columns for Model
##"watched", "watching", "wantWatch", "rating", "votes"

# Continuous Vs Categorical correlation strength: ANOVA
# Analysis of Variance(ANOVA)

# H0: Variables are NOT correlated
# P-Value<0.05--> Variables are correlated(H0 is rejected)
# P-Value>=0.05--> Variables are NOT correlated (H0 is accepted)

# Using a for-loop to perform ANOVA on multiple columns
ColsForANOVA=c("mediaType","ongoing")
for (catCol in ColsForANOVA){
  anovaData= Rating[, c("rating", catCol)]
  print(summary(aov(rating~., data= anovaData)))
}

## Selecting below the categorical columns for model
## "mediaType","ongoing"

## Step-10##
# Generating the Data frame for machine learning
TargetVariableName=c('rating')

# Choosing multiple Predictors which may have relation with Target Variable
# Based on the exploratory data analysis
BestPredictorName= c("watched", "watching", "wantWatch", "votes","mediaType","ongoing")

# Extracting Target and predictor variables from data to create a generic dataset
TargetVariable= Rating[, c(TargetVariableName)]
str(TargetVariable)

# Selecting all other columns as Predictors apart from target variable
PredictorVariable= Rating[, BestPredictorName]
str(PredictorVariable)

DataForML=data.frame(TargetVariable,PredictorVariable)
str(DataForML)
head(DataForML)

##Step-11
# Sampling | Splitting data into 70% for training 30% for testing
set.seed(123)
TrainingSampleIndex=sample(1:nrow(DataForML), size=0.7 * nrow(DataForML))
DataForMLTrain=DataForML[TrainingSampleIndex, ]
DataForMLTest=DataForML[-TrainingSampleIndex, ]
dim(DataForMLTrain)
dim(DataForMLTest)

# Creating Predictive models on training data to check the accuracy of each algorithm
###### Linear Regression #######

startTime=Sys.time()
Model_Reg=lm(TargetVariable~watched+watching+wantWatch+votes+mediaType+ongoing,data=DataForMLTrain)
summary(Model_Reg)
endTime=Sys.time()
endTime-startTime

##According to the Linear regression model, the significant variables are as follows:
## watched, watching, wantWatch, votes, mediaType== "Movie", mediaType== "Music Video",
## mediaType== "Other", mediaType== "OVA", mediaType== "TV", mediaType== "TV Special", mediaType== "Web", ongoing=="Yes"
## R-squared:  0.4465,	Adjusted R-squared:  0.4452, F-statistic: 329.9 on 12 and 4907 DF,  p-value: < 0.00000000000000022.
## According to the observations R- squared is considerably low.

##Step-11
##Testing
##MultiColinearity
Model_Reg=lm(TargetVariable~watched+watching+wantWatch+votes+
               I(mediaType== "Movie")+I(mediaType== "Music Video")+I(mediaType== "Other")+I(mediaType== "OVA")+
               I(mediaType== "TV")+I(mediaType== "TV Special")+I(mediaType== "Web")+I(ongoing=="Yes"),data=DataForMLTrain)
summary(Model_Reg)

library(car)
VIF= vif(Model_Reg)
data.frame(VIF)
## The variables votes, wantWatch and watched have vif value more than 5 which mean it has higher correlation.
## Votes have the higher vif value is 35.127670 therefore, I'm removing the the variables first.

##Removing Votes
Model_Reg1=lm(TargetVariable~watched+watching+wantWatch+
               I(mediaType== "Movie")+I(mediaType== "Music Video")+I(mediaType== "Other")+I(mediaType== "OVA")+
               I(mediaType== "TV")+I(mediaType== "TV Special")+I(mediaType== "Web")+I(ongoing=="Yes"),data=DataForMLTrain)
summary(Model_Reg1)

##Checking of Multicolinearity
VIF= vif(Model_Reg1)
data.frame(VIF)
## wantWatch have the higher vif. Therefore, removing the variables next.

##Removing wantWatch
Model_Reg2=lm(TargetVariable~watched+watching+
                I(mediaType== "Movie")+I(mediaType== "Music Video")+I(mediaType== "Other")+I(mediaType== "OVA")+
                I(mediaType== "TV")+I(mediaType== "TV Special")+I(mediaType== "Web")+I(ongoing=="Yes"),data=DataForMLTrain)
summary(Model_Reg2)

##Checking of Multicolinearity
VIF= vif(Model_Reg2)
data.frame(VIF)
## Since after wantWatch 

##Removing mediaType == "TV"
Model_Reg3=lm(TargetVariable~watched+watching+
                I(mediaType== "Movie")+I(mediaType== "Music Video")+I(mediaType== "Other")+I(mediaType== "OVA")+
                I(mediaType== "TV Special")+I(mediaType== "Web")+I(ongoing=="Yes"),data=DataForMLTrain)
summary(Model_Reg3)

##Checking of Multicolinearity
VIF= vif(Model_Reg3)
data.frame(VIF)

##According to the Linear regression model, the significant variables are as follows:
## Residual standard error: 0.7016 on 4910 degrees of freedom
##Multiple R-squared:  0.396,	Adjusted R-squared:  0.3949
##F-statistic: 357.6 on 9 and 4910 DF,  p-value: < 0.00000000000000022

##Step-12
##consider the final model and interpret the values in the estimate column with regards to the Target
#variable
DataForMLTest$Pred_LM=predict(Model_Reg3, DataForMLTest)
head(DataForMLTest)
# Calculating the Absolute Percentage Error for each prediction
DataForMLTest$LM_APE= 100 *(abs(DataForMLTest$TargetVariable-DataForMLTest$Pred_LM)/DataForMLTest$TargetVariable)
head(DataForMLTest)

library(Metrics)
results= rmse(DataForMLTest$TargetVariable,DataForMLTest$Pred_LM)
results

MeanAPE=mean(DataForMLTest$LM_APE)
MedianAPE=median(DataForMLTest$LM_APE)

print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - MeanAPE))
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - MedianAPE))

## Model Mean and Median Accuracy around 75.42 and 83.79 res. but the R-squared is 0.396 and adjusted R-Squared is 0.3949
## which comparatively low

## Step-13
### Additional Data Transformations

#might reduce skewness and increase the correlation with the target variable
# logarithmic is the most common one

#Watched
par(mfrow= c(1,1))
hist(Rating$watched)
cor(x=Rating$watched , y=Rating$rating)
min(Rating$watched)
#Treating the Zeros in the Columns
#apart from 0, minimum no. of people voted =1 
Rating$watched[Rating$watched==0]=1
# Log Transformation
hist(log(Rating$watched))
cor(x=log(Rating$watched) , y=Rating$rating)

#watching
par(mfrow= c(1,1))
hist(Rating$watching)
cor(x=Rating$watching , y=Rating$rating)
min(Rating$watching)
#Treating the Zeros in the Columns
#apart from 0, minimum no. of people voted =1 
Rating$watching[Rating$watching==0]=1
# Log Transformation
hist(log(Rating$watching))
cor(x=log(Rating$watching) , y=Rating$rating)

##wantWatch
par(mfrow= c(1,1))
hist(Rating$wantWatch)
cor(x=Rating$wantWatch , y=Rating$rating)
min(Rating$wantWatch)
#Treating the Zeros in the Columns
#apart from 0, minimum no. of people voted =1 
Rating$wantWatch[Rating$wantWatch==0]=1
# Log Transformation
hist(log(Rating$wantWatch))
cor(x=log(Rating$wantWatch) , y=Rating$rating)

##votes
par(mfrow= c(1,1))
hist(Rating$votes)
cor(x=Rating$votes , y=Rating$rating)
min(Rating$votes)
#Treating the Zeros in the Columns
#apart from 0, minimum no. of people voted =1 
Rating$votes[Rating$votes==0]=1
# Log Transformation
hist(log(Rating$votes))
cor(x=log(Rating$votes) , y=Rating$rating)

##Step-14
## Add watched in Data
watched_1= log(Rating$watched)
Rating= cbind(Rating, watched_1)
head(Rating,5)

## Add watchng in data
watching_1= log(Rating$watching)
Rating= cbind(Rating, watching_1)
head(Rating, 5)

## Add WantWatch in data
wantWatch_1= log(Rating$wantWatch)
Rating= cbind(Rating, wantWatch_1)
head(Rating,5)

## Add votes in data
votes_1= log(Rating$votes)
Rating= cbind(Rating, votes_1)
head(Rating, 5)

##Step-14
## after Transformation

# Generating the Data frame for machine learning
TargetVariableName1=c('rating')

# Choosing multiple Predictors which may have relation with Target Variable
# Based on the exploratory data analysis
BestPredictorName1= c("watched_1", "watching_1", "wantWatch_1", "votes_1","mediaType","ongoing")

# Extracting Target and predictor variables from data to create a generic dataset
TargetVariable1= Rating[, c(TargetVariableName1)]
str(TargetVariable1)

# Selecting all other columns as Predictors apart from target variable
PredictorVariable1= Rating[, BestPredictorName1]
str(PredictorVariable1)

DataForML1=data.frame(TargetVariable1,PredictorVariable1)
str(DataForML1)
head(DataForML1)

# Sampling | Splitting data into 70% for training 30% for testing
set.seed(123)
TrainingSampleIndex1=sample(1:nrow(DataForML1), size=0.7 * nrow(DataForML1))
DataForMLTrain1=DataForML1[TrainingSampleIndex1, ]
DataForMLTest1=DataForML1[-TrainingSampleIndex1, ]
dim(DataForMLTrain1)
dim(DataForMLTest1)

##Step-15
# Creating Predictive models on training data to check the accuracy of each algorithm
###### Linear Regression #######

startTime=Sys.time()
Model_Reg_1=lm(TargetVariable1~watched_1+watching_1+wantWatch_1+votes_1+mediaType+ongoing,data=DataForMLTrain1)
summary(Model_Reg_1)
endTime=Sys.time()
endTime-startTime

##Removing mediaType== "other"
Model_Reg_2=lm(TargetVariable1~watched_1+watching_1+wantWatch_1+votes_1+
                 I(mediaType== "Movie")+I(mediaType== "Music Video")+I(mediaType== "OVA")+
                 I(mediaType== "TV")+I(mediaType== "TV Special")+I(mediaType== "Web")+ongoing,data=DataForMLTrain1)
summary(Model_Reg_2)

##Testing
##MultiColinearity
Model_Reg_2=lm(TargetVariable1~watched_1+watching_1+wantWatch_1+votes_1+
                 I(mediaType== "Movie")+I(mediaType== "Music Video")+I(mediaType== "OVA")+
                 I(mediaType== "TV")+I(mediaType== "TV Special")+I(mediaType== "Web")+I(ongoing== "Yes"),data=DataForMLTrain1)
summary(Model_Reg_2)
library(car)
VIF= vif(Model_Reg_2)
data.frame(VIF)

##Removing votes_1
Model_Reg_3=lm(TargetVariable1~watched_1+watching_1+wantWatch_1+
                 I(mediaType== "Movie")+I(mediaType== "Music Video")+I(mediaType== "OVA")+
                 I(mediaType== "TV")+I(mediaType== "TV Special")+I(mediaType== "Web")+I(ongoing== "Yes"),data=DataForMLTrain1)
summary(Model_Reg_3)
VIF1=vif(Model_Reg_3)
data.frame(VIF1)

##Removing wantWatch_1
Model_Reg_4=lm(TargetVariable1~watched_1+watching_1+
                 I(mediaType== "Movie")+I(mediaType== "Music Video")+I(mediaType== "OVA")+
                 I(mediaType== "TV")+I(mediaType== "TV Special")+I(mediaType== "Web")+I(ongoing== "Yes"),data=DataForMLTrain1)
summary(Model_Reg_4)
VIF2=vif(Model_Reg_4)
data.frame(VIF2)

## Removing watching_1 
Model_Reg_5=lm(TargetVariable1~watched_1+
                 I(mediaType== "Movie")+I(mediaType== "Music Video")+I(mediaType== "OVA")+
                 I(mediaType== "TV")+I(mediaType== "TV Special")+I(mediaType== "Web")+I(ongoing== "Yes"),data=DataForMLTrain1)
summary(Model_Reg_5)

VIF3=vif(Model_Reg_5)
data.frame(VIF3)

## Removing mediaType == "Music Video"
Model_Reg_6=lm(TargetVariable1~watched_1+
                 I(mediaType== "Movie")+I(mediaType== "OVA")+
                 I(mediaType== "TV")+I(mediaType== "TV Special")+I(mediaType== "Web")+I(ongoing== "Yes"),data=DataForMLTrain1)
summary(Model_Reg_6)
VIF4=vif(Model_Reg_6)
data.frame(VIF4)

##According to the Linear regression model, the significant variables are as follows:
##Residual standard error: 0.5923 on 4912 degrees of freedom
##Multiple R-squared:  0.5693,	Adjusted R-squared:  0.5687
##F-statistic: 927.4 on 7 and 4912 DF,  p-value: < 0.00000000000000022

## Step-16
##consider the final model and interpret the values in the estimate column with regards to the Target
#variable
DataForMLTest1$Pred_LM=predict(Model_Reg_6, DataForMLTest1)
head(DataForMLTest1)
# Calculating the Absolute Percentage Error for each prediction
DataForMLTest1$LM_APE= 100 *(abs(DataForMLTest1$TargetVariable1-DataForMLTest1$Pred_LM)/DataForMLTest1$TargetVariable1)
head(DataForMLTest1)

library(Metrics)
results1= rmse(DataForMLTest1$TargetVariable1,DataForMLTest1$Pred_LM)
results1
MeanAPE=mean(DataForMLTest1$LM_APE)
MedianAPE=median(DataForMLTest1$LM_APE)

print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - MeanAPE))
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - MedianAPE))

## Model Mean and Median Accuracy around 80.38 and 87.39 res. but the R- squared is 0.5693 and adjusted R- Squared is 0.5687
## which comparatively better than before.

##Global Hypothesis
##Null Hypothesis: The model is not possible (P-value > 0.05)
##Alternate Hypothesis: The model is possible (P-value < 0.05)
## The P-value: < 0.00000000000000022
## Since the p-value is low. Therefore we reject null hypothesis which means the model prediction is possible.

## Business Recommendations

## With increase in every one person who watched different type of anime, the rating increases by 0.284. 
## The anime which are still ongoing is more than the anime which are not by 0.606.
## The rating of movies is more than the other media types by 0.403.
## the ratings of web series is less than the the other media types by 0.095.
## Therefore we conclude that rating of different type anime dependent on the viewers who already watch the particular anime.
## Rating is more for ongoing anime types than the anime are not.
## By this we can say that the viewers are more inclined towards movies types contents of animes are popular. 

