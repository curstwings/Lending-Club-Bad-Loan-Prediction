library('caret') 
library('dplyr') 
library('rpart')
library('car')
library('e1071')
library('stringr')
library('MASS')

train <- read.csv("data/train_trimmed2.csv", stringsAsFactors = F)
fold<- 4

train<- train[train$loan_status!="Current" & train$loan_status!= "Issued" , ]

apply(train,2, function(x) length(unique(x)))

train %>% group_by(loan_status) %>% summarise(count = n())

# Deal w/ missing data
train$home_ownership[train$home_ownership == ""]="RENT"
train$annual_inc[ is.na(train$annual_inc)]=0

s = sort(rexp(100))
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
train$annual_inc <- range01(train$annual_inc)
train$loan_amnt <- range01(train$loan_amnt)
train$installment <- range01(train$installment)

good_indicators <- c("Fully Paid",
                     "Does not meet the credit policy. Status:Fully Paid"
                     )

# if the loan is bad, ind= 1; else =0
train$is_bad <- ifelse(train$loan_status %in% good_indicators, 0,
                       ifelse(train$loan_status=="", NA, 1)
)

tmp = train %>% group_by(is_bad) %>% summarise(ncount = n())
tmp$ncount = 100 * tmp$ncount/nrow(train)
tmp$ncount_p = str_c(round(tmp$ncount,2),"%")
ggplot(tmp,aes(x=is_bad,y=ncount,fill=is_bad)) + geom_bar(stat="identity") +
  geom_text(aes(label=ncount_p),vjust = 2)

# cols<- c("annual_inc","grade","home_ownership", "purpose", "delinq_2yrs", "inq_last_6mths", "dti", "pub_rec", "is_bad", "loan_amnt","term", "installment")
cols<- c("annual_inc","grade","home_ownership", "purpose", "delinq_2yrs", "dti", "is_bad", "term", "installment")

feature_set<-train[cols]
for (i in c("home_ownership", "purpose", "grade", "is_bad", "term")){
  feature_set[,i]<- as.factor(feature_set[,i])
}

##Check 
apply(feature_set,2, function(x) length(unique(x)))

###For Cross validation purpose will keep 20% of data aside from my orginal train set
##This is just to check how well my data works for unseen data
set.seed(32768)
ind=createDataPartition(feature_set$is_bad,times=1,p=0.8,list=FALSE)
train_val=feature_set[ind,]
test_val=feature_set[-ind,]

train_val<- na.exclude(train_val) 
test_val<- na.exclude(test_val) 

#### Logistic Regression

log.mod <- glm(is_bad ~ . , data = train_val, family = binomial())

###Check the summary
summary(log.mod)
confint(log.mod)
summary(log.mod)$coeff[-1,4] < 0.05

train.probs <- predict(log.mod, data=train_val,type =  "response")

threshold<- 0.5
###Predict train data
predict_loan_status_label = ifelse(train.probs>threshold,1,0)
confusionMatrix(data=predict_loan_status_label, train_val$is_bad)

###Predict test data
test.probs <- predict(log.mod, newdata=test_val,type ="response")
predict_loan_status_label2 = ifelse(test.probs>threshold,1,0)
confusionMatrix(data=predict_loan_status_label2, test_val$is_bad)
