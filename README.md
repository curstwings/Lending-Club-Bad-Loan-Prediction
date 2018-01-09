Lending-Club-Bad-Loan-Prediction

Dataset Used:
=============
Lending club history data from Kaggles: 

https://www.kaggle.com/wendykan/lending-club-loan-data

Package Used: 
=============
library('caret') 

library('dplyr') 

library('rpart')

library('car')

library('e1071')

library('stringr')

library('MASS')


Method:
=============
Logistic Regression with K-fold Cross Validation

Result:
=============

| Result Table | |
| ------------- | ------------- |
| Accuracy  | 0.7597 |
| 95% CI  | (0.754, 0.7654)  |
| No Information Rate  | 0.7518    |
| P-Value [Acc > NIR]  | 0.003157  |
| Kappa  | 0.1257    |
| ------------- | ------------- |
| Sensitivity  |  0.9702   |
| Specificity  | 0.1224   |
