#Determine company's attrition based on employee profile

#Read the data- Call from computer location and read file
setwd('C:/Users//')
df1 = read.csv('table.csv')

#Libraries
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(tidyr)
library(lattice)
library(gbm)
library(randomForest)
library(ipred)
library(rpart)
library(rpart.plot)
library(caTools)
library(caret)
library(dplyr)
library(ISLR)
library(ROCR)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(Metrics)
################################## DATA WRANGLING ####################################
#clean depending on the input data
#Inspect the data
library(skimr)
skim(df1)
#There are 11 variables with 0 missing value

#Change job code format
df1$job_code = as.character(df1$job_code)
df1$attrition_status<- as.factor(df1$attrition_status)

#Solve unbalance data issue for (Attrition | Employee Profile)
Yes = which(df1$attrition_status == 1)
No = which(df1$attrition_status == 0)

Yes_downsample = sample(Yes, length(No))
df1_down = df1[c(Yes_downsample, No),]

length(Yes)
length(No)

#Create a new dataframe with selected variables. 
#Note> Add new variables or delete variables as needed 
df2 = df1_down %>% select(`attrition_status`, `city`, `country`, `duration`, 
                          `job_family`, `job_level`, `number_of_direct_reports`)

###############################LOGISTIC REGRESSION#####################################

#Split Data
set.seed(1031)

split = sample(1:nrow(df2),size = nrow(df2)*0.7)
train = df2[split,]
test = df2[-split,]

#Modification to city variable
test$city[which(!test$city%in% unique(train$city))]<-NA

#Graph distribution
train%>%
  select_if(is.numeric)%>%
  pivot_longer(cols = 1,names_to = 'numeric_predictor', values_to = 'values'  )%>%
  ggplot(aes(x = values))+
  geom_histogram()+
  facet_wrap(numeric_predictor~., scales = 'free')+
  theme_bw()


#Model
mymodel<-glm(`attrition_status`~.,  data = train, family = 'binomial')
options(max.print=10000)
summary(mymodel)


#test dataset prediction 
data_test_new <- test

data_test_new$job_family[which(!(data_test_new$job_family %in% unique(train$job_family)))] <- NA

data_test_new$city[which(!(data_test_new$city %in% unique(train$city)))] <- NA

p2<-predict(mymodel, newdata = data_test_new, 
            type = 'response')

ct_2 = table(leave = data_test_new$attrition_status,
             predictions = as.numeric(p2>0.5))
ct_2

#test dataset accuracy, specificity, sensitivity
accuracy = sum(ct_2[1,1],ct_2[2,2])/nrow(data_test_new); accuracy

specificity = ct_2[1,1]/sum(ct_2[1,1],ct_2[1,2]); specificity

sensitivity = ct_2[2,2]/sum(ct_2[2,1],ct_2[2,2]); sensitivity



############################# CLASSIFICATION TREE  ######################################


#Tree model
tree= rpart(`attrition_status` ~. ,data=train, method = "class",
            control=rpart.control(cp=0))

#Inference
summary(tree)
rpart.plot(tree)
tree$variable.importance

# Test dataset Prediction
pred <- predict(tree, newdata=test, type = "class")

#confusion matrix
confusion_matrix = table(test$`attrition_status`, pred)
confusion_matrix 

#test dataset accuracy, specificity, sensitivity
accuracy_Test <- sum(diag(confusion_matrix)) / sum(confusion_matrix) 
sensitivity(confusion_matrix)
specificity(confusion_matrix)

# Area Under the Curvey (AUC) 
pred = predict(tree,newdata=test, )
ROCRpred = prediction(pred[,2],test$`attrition_status`)

aucDT <- performance(ROCRpred, measure = "auc")
aucDT <- aucDT@y.values[[1]] 
#An excellent model has AUC near to the 1
