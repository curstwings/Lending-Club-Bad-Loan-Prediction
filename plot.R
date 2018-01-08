library('caret') 
library('dplyr') 
library('rpart')
library('car')
library('e1071')
library('stringr')
library('MASS')
library('ggthemes')

train <- read.csv("data/train_trimmed2.csv", stringsAsFactors = F)
fold<- 4

train<- train[train$loan_status!="Current" & train$loan_status!= "Issued" & train$loan_status!= "In Grace Period", ]
good_indicators <- c("Fully Paid",
                     "Default",
                     "Does not meet the credit policy. Status:Fully Paid"
)

# if the loan is bad, ind= 1; else =0
train$goodloan <- as.factor(ifelse(train$loan_status %in% good_indicators, 1,
                       ifelse(train$loan_status=="", NA, 0))
)
loan<- train %>% mutate_if(is.character, as.factor) 


ggplot(data=loan, aes(loan_amnt,fill=term))+geom_histogram(bins=40)+theme_solarized()
#loan amount distribution

ggplot(data=loan, aes(int_rate))+geom_histogram(bins=45,color="red",fill="yellow")
#interest rate distribution

ggplot(data=loan,aes(loan_amnt, col=grade))+  geom_histogram(bins=40) +   facet_grid(grade ~ .,scales = "free_y")
# loan amount vs grade

ggplot(loan, aes(term, fill = loan_status)) + geom_density() + facet_grid(loan_status ~ .)
# loan status

ggplot(data=loan,aes(x=int_rate,fill=goodloan)) +
  geom_histogram(bins=40) +
  facet_grid(grade ~ .,scales = "free_y") +
  xlab("Interest Rate") +
  ylab("Count") +
  theme_solarized()
# int rate vs grade

ggplot(data=loan,aes(x=int_rate,fill=term)) +
  geom_histogram(bins=40) +
  facet_grid(grade ~ .,scales = "free_y") +
  xlab("Interest Rate") +
  ylab("Count") +
  theme_solarized()

box_status <- ggplot(loan, aes(loan_status, loan_amnt))
box_status + geom_boxplot(aes(fill = loan_status)) +
  theme(axis.text.x = element_blank()) +
  labs(list(
    title = "Loan amount by status",
    x = "Status",
    y = "Amount"))

ggplot(data=loan, aes(x=grade, fill=sub_grade))+geom_bar(position="dodge")+labs(x="Loan Grade", y ="Loan Transactions",title="Sub-Gradewise Loan Distribution") + theme_solarized()

## Total Loan amount basis Grade & Term wise
ggplot(loan, aes(x=grade, y=loan_amnt, fill=loan$term, width=0.6)) +
  stat_summary(fun.y="sum", geom="bar") +
  labs(x="Loan Grade", y ="Total Loan Amount",title="Total Loan amount basis Grade & Term")+ 
  theme_solarized()

# purpose
ggplot(data=loan, aes(x=grade, fill=purpose)) +
  geom_bar() +   theme_solarized()

###is there any association between Survial rate and where he get into the Ship.   
ggplot(loan,aes(x = home_ownership,fill=factor(goodloan))) +
  geom_bar() +
  ggtitle("House Ownership vs Good Loan") +
  xlab("House Ownership") +
  ylab("Total Count") +
  labs(fill = "Good Loan") +   theme_solarized()

###is there any association between Survial rate and where he get into the Ship.   
ggplot(loan,aes(x = grade,fill=factor(goodloan))) +
  geom_bar() +
  ggtitle("Grade vs Good Loan") +
  xlab("LC Grade") +
  ylab("Total Count") +
  labs(fill = "Good Loan") +   theme_solarized()

loan_by_grade <- aggregate(loan_amnt ~ sub_grade + goodloan, data = loan, sum)
gbar <- ggplot(loan_by_grade, aes(sub_grade, loan_amnt, fill = goodloan))
gbar + geom_bar(stat = "identity") + theme(axis.text.x=element_text(size=7))+ theme_solarized()
gbar + geom_bar(position = "fill", stat = "identity") + theme(axis.text.x=element_text(size=7))  + theme_solarized()