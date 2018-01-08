


##Load raw data
whole_data <- read.csv("data/loan.csv", stringsAsFactors = F)
#test  <- read.csv("data/test.csv", stringsAsFactors = F)
#test$Survived <- NA
#whole_dataset <- rbind(train, test)

## 75% of the sample size
smp_size <- floor(0.01 * nrow(whole_data))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(whole_data)), size = smp_size)

train <- whole_data[train_ind, ]
last <- whole_data[-train_ind, ]

View(train)
write.csv(train, file="data/train_trimmed.csv", row.names = FALSE)