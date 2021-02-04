

#loading the required packages 
library(corrplot)
library(car)
library(ROCR)
library(caret)
library(caTools)
library(psych)
library(rpart)
library(rpart.plot)
library(e1071)
library(rattle.data)
library(rattle)

#loading the datasets 
HR <- read.csv("promotion_tr.csv", stringsAsFactors = TRUE)
summary(HR)
HR$education <- as.factor(HR$education)
table(HR$education)
table(HR$recruitment_channel)
table(HR$is_promoted)
View(HR)

#Cleaning the dataset by removing and imputing rows 


#mode imputation on column previous year rating
table(HR$previous_year_rating)
HR$previous_year_rating[is.na(HR$previous_year_rating)] <- 3
table(HR$previous_year_rating)
summary(HR$previous_year_rating)

#NA imputing on empty cells on column education
HR[which(HR$education==""),]$education <-NA
summary(HR$education)
View(HR)

#removing all NA rows from the dataset
HR <- na.omit(HR)
View(HR)
summary(HR)

#checking correlation between the variables 
CR <- cor(HR[c("no_of_trainings","age","previous_year_rating",
               "length_of_service","KPIs_met..80.","awards_won.",
               "avg_training_score","is_promoted")])
CR
corrplot(CR, type = "full")
corrplot(CR,method = "number")
corrplot.mixed(CR)
corrplot.mixed(CR,  tl.col = "black", lower.col = "black",number.cex = .7,tl.cex=1 )

#To create dummy variables
HR$gender_male<- ifelse(HR$gender == "m", 1,0)
HR$gender_female<- ifelse(HR$gender == "f", 1,0)

HR$recruitment_channel_sourcing<- ifelse(HR$recruitment_channel == "sourcing", 1,0)
HR$recruitment_channel_other<- ifelse(HR$recruitment_channel == "other", 1,0)
HR$recruitment_channel_referred<- ifelse(HR$recruitment_channel == "referred", 1,0)

HR$education_bachelors<- ifelse(HR$education == "Bachelor's", 1,0)
HR$education_masters<- ifelse(HR$education == "Master's & above", 1,0)
HR$education_BelowSecondary<- ifelse(HR$education == "Below Secondary", 1,0)

View(HR)

#Splitting of the dataset into training and testing

split <- sample.split(HR$is_promoted, SplitRatio = 0.7)
HR_TR <- subset(HR, split == "TRUE")
HR_TS <- subset(HR, split == "FALSE")

View(HR_TR)
#logistic regression models
#adding variables at first 
model1 <- glm(is_promoted ~ no_of_trainings, data=HR_TR, family = binomial)
summary(model1)

model2 <- glm(is_promoted ~ no_of_trainings+age, data=HR_TR, family = binomial)
summary(model2)

model3 <- glm(is_promoted ~ no_of_trainings+age+previous_year_rating, data=HR_TR, 
              family = binomial)
summary(model3)

model4 <- glm(is_promoted ~ no_of_trainings+age+previous_year_rating+length_of_service, 
              data=HR_TR, family = binomial)
summary(model4)

model5 <- glm(is_promoted ~ no_of_trainings+age+previous_year_rating+length_of_service
              +KPIs_met..80., 
              data=HR_TR, family = binomial)
summary(model5)

model6 <- glm(is_promoted ~ no_of_trainings+age+previous_year_rating+length_of_service
              +KPIs_met..80.+awards_won., 
              data=HR_TR, family = binomial)
summary(model6)

model7 <- glm(is_promoted ~ no_of_trainings+age+previous_year_rating+length_of_service
              +KPIs_met..80.+awards_won.+avg_training_score, 
              data=HR_TR, family = binomial)
summary(model7)

model8 <- glm(is_promoted ~ no_of_trainings+age+previous_year_rating+length_of_service
              +KPIs_met..80.+awards_won.+avg_training_score+education_bachelors
              +education_masters+education_BelowSecondary, 
              data=HR_TR, family = binomial)
summary(model8)

model9 <- glm(is_promoted ~ no_of_trainings+age+previous_year_rating+length_of_service
              +KPIs_met..80.+awards_won.+avg_training_score+education_bachelors
              +education_masters+education_BelowSecondary+recruitment_channel_sourcing
              +recruitment_channel_other+recruitment_channel_referred, 
              data=HR_TR, family = binomial)
summary(model9)

model10 <- glm(is_promoted ~ no_of_trainings+age+previous_year_rating+length_of_service
              +KPIs_met..80.+awards_won.+avg_training_score+education_bachelors
              +education_masters+education_BelowSecondary+recruitment_channel_sourcing
              +recruitment_channel_other+recruitment_channel_referred
              +gender_male+gender_female, 
              data=HR_TR, family = binomial)
summary(model10)

#removing variables 

model11 <- glm(is_promoted ~ no_of_trainings+age+previous_year_rating+length_of_service
               +KPIs_met..80.+awards_won.+avg_training_score+education_bachelors
               +education_masters+education_BelowSecondary
               +gender_male+gender_female, 
               data=HR_TR, family = binomial)
summary(model11)

model12 <- glm(is_promoted ~no_of_trainings+previous_year_rating+length_of_service
               +KPIs_met..80.+awards_won.+avg_training_score+education_bachelors
               +education_masters+education_BelowSecondary
               +gender_male+gender_female, 
               data=HR_TR, family = binomial)
summary(model12)

model13 <- glm(is_promoted ~age+previous_year_rating+length_of_service
               +KPIs_met..80.+awards_won.+avg_training_score+education_bachelors
               +education_masters+education_BelowSecondary
               +gender_male+gender_female, 
               data=HR_TR, family = binomial)
summary(model13)

model14 <- glm(is_promoted ~no_of_trainings+previous_year_rating+length_of_service
               +KPIs_met..80.+awards_won.+avg_training_score+education_bachelors
               +education_masters+education_BelowSecondary, 
               data=HR_TR, family = binomial)
summary(model14)

model15 <- glm(is_promoted ~no_of_trainings+previous_year_rating+length_of_service
               +KPIs_met..80.+awards_won.+avg_training_score+education_bachelors
               +education_masters, 
               data=HR_TR, family = binomial)
summary(model15)

model16 <- glm(is_promoted ~no_of_trainings+previous_year_rating
               +KPIs_met..80.+awards_won.+avg_training_score+education_bachelors
               +education_masters, 
               data=HR_TR, family = binomial)
summary(model16)

model17 <- glm(is_promoted ~ no_of_trainings+age+previous_year_rating+length_of_service
               +KPIs_met..80.+awards_won.+avg_training_score+education_bachelors
               +education_masters+education_BelowSecondary+recruitment_channel_sourcing
               +recruitment_channel_other+recruitment_channel_referred, 
               data=HR_TR, family = binomial)
summary(model17)

#so far model 11

#prediction and confusion matrix

res <- predict(model11,HR_TS, type='response')
head(res)
head(HR_TS$is_promoted)

table(Actualvalue=HR_TS$is_promoted, PredictedValue= res>0.45)
View(HR_TS)
#ROCR Curve 

ROCRpred <- prediction(res,HR_TS$is_promoted)
ROCRpref <- performance(ROCRpred, "tpr","fpr")
plot(ROCRpref,colorize = TRUE, print.cutoffs.at=seq(0.2,by=0.3))



#Decision tree 

#information gain 
model18 <- rpart(is_promoted ~ no_of_trainings+age+previous_year_rating+length_of_service
               +KPIs_met..80.+awards_won.+avg_training_score+education_bachelors
               +education_masters+education_BelowSecondary
               +gender_male+gender_female,  
               data=HR_TR, 
               method = "class",
               parms = list(split = "infogain"))
model18

#gini index
model19 <- rpart(is_promoted ~ no_of_trainings+age+previous_year_rating+length_of_service
                 +KPIs_met..80.+awards_won.+avg_training_score+education_bachelors
                 +education_masters+education_BelowSecondary,  
                 data=HR_TR, 
                 method = "class",
                 parms = list(split = "gini"))
model19

fancyRpartPlot(model18)
fancyRpartPlot(model19)

pred <- predict(model19, newdata = HR_TS, type = "class")
confusionMatrix(table(pred,HR_TS$is_promoted))



##Running our finals models to predict the testing data 

TrainHR <- read.csv("promotion_ts.csv", stringsAsFactors = TRUE)
View(TrainHR)
summary(TrainHR)

#Cleaning the dataset by removing and imputing rows 


#mode imputation on column previous year rating
table(TrainHR$previous_year_rating)
TrainHR$previous_year_rating[is.na(TrainHR$previous_year_rating)] <- 3
table(TrainHR$previous_year_rating)
summary(TrainHR$previous_year_rating)

#NA imputing on empty cells on column education
TrainHR[which(TrainHR$education==""),]$education <-NA
summary(TrainHR$education)
View(TrainHR)

#removing all NA rows from the dataset
TrainHR <- na.omit(TrainHR)
View(TrainHR)
summary(TrainHR)

#To create dummy variables
TrainHR$gender_male<- ifelse(TrainHR$gender == "m", 1,0)
TrainHR$gender_female<- ifelse(TrainHR$gender == "f", 1,0)
TrainHR$education_bachelors<- ifelse(TrainHR$education == "Bachelor's", 1,0)
TrainHR$education_masters<- ifelse(TrainHR$education == "Master's & above", 1,0)
TrainHR$education_BelowSecondary<- ifelse(TrainHR$education == "Below Secondary", 1,0)

View(TrainHR)

#Logistic regression prediction
model20 <- glm(is_promoted ~ no_of_trainings+age+previous_year_rating+length_of_service
               +KPIs_met..80.+awards_won.+avg_training_score+education_bachelors
               +education_masters+education_BelowSecondary, 
               data=HR_TR, family = binomial)
summary(model20)
res <- predict(model20,TrainHR, type='response')
table(PredictedValue= res>0.45)


#decision tree gini index model prediction
model19 <- rpart(is_promoted ~ no_of_trainings+age+previous_year_rating+length_of_service
                 +KPIs_met..80.+awards_won.+avg_training_score+education_bachelors
                 +education_masters+education_BelowSecondary,  
                 data=HR_TR, 
                 method = "class",
                 parms = list(split = "gini"))
model19
pred <- predict(model19, newdata = TrainHR, type = "class")
View(TrainHR)
table(pred)
