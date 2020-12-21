# nikhil wora
# 114763136
# exercise 4 antoun

require(rpart)
require(rpart.plot)
require(rattle)
require(caret)
require(dplyr)
require(Rcpp)
require(e1071)

dfBankChurn = read.csv("/Users/NikhilWoraMacPro/Downloads/bankchurn.csv", header = TRUE)
dfBankChurnRaw = dfBankChurn 
dfBankChurn$Gender = factor(dfBankChurn$Gender)
dfBankChurn$HasCrCard = factor(dfBankChurn$HasCrCard)
dfBankChurn$IsActiveMember = factor(dfBankChurn$IsActiveMember)
dfBankChurn$Exited = factor(dfBankChurn$Exited)
dfBankChurn = na.omit(dfBankChurnRaw)

# Question 1
# ANSWERED IN WORD DOCUMENT

# Question 2
rows = nrow(dfBankChurn)
cols = ncol(dfBankChurn)
print(rows)
print(cols)

# Question 3
vars = ls(dfBankChurnRaw)
i = 1
print("Dataset Vars")
print(vars)

print("Vars w NA Values")
for(var in vars){
  if(sum(is.na(dfBankChurnRaw[var])) > 0){
    print(i)
    print(var)
    i = i + 1
  }
}

# Question 4
dfBankChurn$Exited = factor(dfBankChurn$Exited)

# Question 5
closedAccounts = sum(dfBankChurn$Exited == "1")
openedAccounts = sum(dfBankChurn$Exited == "0")
avgCustomerAge = mean(dfBankChurn$Age)
avgCreditScore = mean(dfBankChurn$CreditScore)
avgTenure = mean(dfBankChurn$Tenure)
print(closedAccounts)
print(openedAccounts)
print(avgCustomerAge)
print(avgCreditScore)
print(avgTenure)

# Question 6
# 25% of data
numTest = floor(nrow(dfBankChurn) * 0.25)
# Sample data
testRows = sample(1:nrow(dfBankChurn), numTest)
# Indices
testDfBankChurn = dfBankChurn[testRows,]
# Train set
trainDfBankChurn = dfBankChurn[-testRows,]

# Question 7
trainDfBankChurn$Exited = factor(trainDfBankChurn$Exited)
treemod = rpart(Exited ~ CreditScore + Age + Tenure, 
                data = trainDfBankChurn, 
                method = 'class', 
                control = rpart.control(minsplit = 25))
fancyRpartPlot(treemod, sub = "")

# Question 8
pred <- predict(treemod, testDfBankChurn)
testDfBankChurn$ExitedScore <- pred[,2]
testPred <- testDfBankChurn %>% select(Exited, ExitedScore)
testPred <- testPred %>% arrange(desc(ExitedScore))
testPred$pred <- 0
topScores <- floor(nrow(testPred)*0.2)
testPred$pred[1:topScores] <- 1
predTable <- table(testPred$pred,testPred$Exited)
dimnames(predTable)[[2]] = c("0","1")
confusionMatrix(predTable, positive = "1")

# Question 9
precision(predTable, relevant = '1')
recall(predTable, relevant = '1')
