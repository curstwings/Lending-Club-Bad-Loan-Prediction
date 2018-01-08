library('caret') 
library('dplyr') 
library('rpart')
library('car')
library('e1071')
library('stringr')
library('ROCR')
library('pscl')

train <- read.csv("data/train_trimmed2.csv", stringsAsFactors = F)
fold<- 4

# variables: purpose(class),int.rate, inq.last.6mths, delinq.2yrs, inq_last_6mths, purpose(class), dti, annual_inc(log), home_ownership(class)
# grade(class), pub_rec

# Remove "Current status" loans
train<- train[train$loan_status!="Current" & train$loan_status!= "Issued" & train$loan_status!= "In Grace Period", ]

##Check the length and see how many varibles of them we can move to factor for our analysis
#colSums(train=='')
apply(train,2, function(x) length(unique(x)))

train %>% group_by(loan_status) %>% summarise(count = n())

# Deal w/ missing data
train$home_ownership[train$home_ownership == ""]="RENT"
train$annual_inc[ is.na(train$annual_inc)]=0

# Log "annual income" to normalize it
train$annual_inc <- log(train$annual_inc)

#colSums(train=="")

#feature_set$annual_inc <- feature_set$annual_inc/1000
# View(feature_set$annual_inc)
# x<- feature_set$annual_inc
# View(x)
# maxmin <- function(x) (x - min(x))/(max(x)-min(x))
# feature_set$annual_inc <- apply(x, 2, maxmin)
# plot(feature_set$annual_inc,xlim = c(-1,2),ylim = c(-1,2)) 

## check if there's any missing value:
##data$Age[is.na(data$Age)]
# train$annual_inc[is.na(train$annual_inc)]
# train$int_rate[is.na(train$int_rate)]
# train$grade[is.na(train$grade)]
# train$annual_inc[is.na(train$home_ownership)]

# set features for analysis

# variables: purpose(class),int.rate, inq.last.6mths, delinq.2yrs, inq_last_6mths, dti, annual_inc(log), home_ownership(class)
# grade(class), pub_rec

cols<- c("annual_inc","int_rate","home_ownership", "purpose", "delinq_2yrs", "inq_last_6mths", "dti", "pub_rec", "grade")

feature_set<-train[cols]
for (i in c("home_ownership", "grade", "purpose")){
  feature_set[,i]<- as.factor(feature_set[,i])
}

good_indicators <- c("Fully Paid",
                     "Default",
                     "Does not meet the credit policy. Status:Fully Paid")

# if the loan is bad, ind= 1; else =0
train$is_bad <- ifelse(train$loan_status %in% good_indicators, 0,
                       ifelse(train$loan_status=="", NA, 1)
)

# # if the loan is bad, ind= 1; else =0
# train$is_bad <- ifelse(train$loan_status %in% bad_indicators, 1,
#                           ifelse(train$loan_status=="", NA, 0)
# )

tmp = train %>% group_by(is_bad) %>% summarise(ncount = n())
tmp$ncount = 100 * tmp$ncount/nrow(train)
tmp$ncount_p = str_c(round(tmp$ncount,2),"%")
ggplot(tmp,aes(x=is_bad,y=ncount,fill=is_bad)) + geom_bar(stat="identity") +
  geom_text(aes(label=ncount_p),vjust = 2)

feature_set$is_bad=as.factor(train$is_bad)
response <- as.factor(train$is_bad)

# response <- as.factor(test_val$is_bad)

##Check 
apply(feature_set,2, function(x) length(unique(x)))

###For Cross validation purpose will keep 20% of data aside from my orginal train set
##This is just to check how well my data works for unseen data
set.seed(32768)
ind=createDataPartition(feature_set$is_bad,times=1,p=0.8,list=FALSE)
train_val=feature_set[ind,]
test_val=feature_set[-ind,]

####check the proprtion of Survival rate in orginal training data, current traing and testing data
# round(prop.table(table(train$is_bad)*100),digits = 1)
# round(prop.table(table(test_val$Survived)*100),digits = 1)

#### Logistic Regression
# # contrasts(feature_set$annual_inc)
# # contrasts(feature_set$int_rate)
# # contrasts(feature_set$grade)
# # contrasts(feature_set$home_ownership)
# ##The above shows how the varible coded among themself
# 

# Logistic Regression with k-fold cv

train_val<- na.exclude(train_val) 
test_val<- na.exclude(test_val) 
ctrl <- trainControl(method = "repeatedcv", number = fold,repeats=1,search= "random", savePredictions = TRUE)
mod_fit <- train(is_bad ~ .,  data=train_val, method="glm", family = binomial(),
                 trControl = ctrl, tuneLength = 5)

summary(mod_fit)

pred = predict(mod_fit, newdata=test_val)
confusionMatrix(data=pred, test_val$is_bad)

ggplot(data=test_val) + geom_density(aes(x=pred,color=is_bad,linetype=is_bad))

# p <- predict(mod_fit, newdata=test_val, type="prob")
# pr <- prediction(p, test_val$is_bad)
# prf <- performance(pr, measure = "tpr", x.measure = "fpr")
# plot(prf)
# 
# auc <- performance(pr, measure = "auc")
# auc <- auc@y.values[[1]]
# auc

# accuracy<- confmat$overall
# acc<- round(accuracy[['Accuracy']], digits =2)
# 
# result<- rbind(c('set', 'accuracy'),c('train', acc))

