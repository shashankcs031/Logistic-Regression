# Download the data set as german_credit

german_credit <- read.csv("german.csv")

## Loading neccessary Libraries 

library(ggplot2)
library(caret)
library(car)
library(caTools)
library(ROCR)
library(MASS)
library(Hmisc)


## Data Understanding and Data Exploration

str(german_credit)

summary(german_credit)

## Univariate plots for some variables 

##1.Credit history

str(german_credit$Credit.history)

summary(factor(german_credit$Credit.history))

library(ggplot2)

ggplot(german_credit, aes(x = Credit.history, fill = factor(german_credit$Default_status))) + geom_bar(position = "dodge") + stat_count(aes(label= ..count..), geom = "text", size = 4.5,colour="Black", position=position_dodge(width = 1),vjust = 1)


## 2. Age.in.years

str(german_credit$Age.in.Years)

summary(german_credit$Age.in.Years)

ggplot(german_credit, aes(german_credit$Age.in.Years)) + geom_histogram( binwidth = 10, bins = 9, fill = "Sky Blue")



## 3.Present employment since

str(german_credit$Present.employment.since)

summary(german_credit$Present.employment.since)

ggplot(german_credit, aes(german_credit$Present.employment.since, fill = factor(german_credit$Default_status))) + geom_bar(position = "dodge") + stat_count(aes(label = ..count..), geom = "text", size = 4.5, colour = "Black", position = position_dodge(width = 1), vjust = 1)


## 4. Housing

str(german_credit$Housing.)

summary(german_credit$Housing.)

ggplot(german_credit, aes(german_credit$Housing., fill = factor(german_credit$Default_status))) + geom_bar(position = "dodge") + stat_count(aes(label = ..count..), geom = "text", size = 4.5, color = "Black", position = position_dodge(width = 1),vjust =1 )

## 5.Credit amount

str(german_credit$Credit.amount)

summary(german_credit$Credit.amount)

ggplot(german_credit, aes(german_credit$Credit.amount,fill = factor(Default_status))) + geom_histogram( binwidth = 1000)


# Data prpeapartion and feature transformation

## we check for NA values first for all the variables in the data set.

sum(is.na(german_credit))

## we could see that there are no NA values in the dataset so now we proceed with 
## checking the outliers

## we convert the variables which were of class integer into factor variable 

german_credit$Installment.rate.in.percentage.of.disposable.income <- as.factor(german_credit$Installment.rate.in.percentage.of.disposable.income)

german_credit$Present.residence.since <- as.factor(german_credit$Present.residence.since)

german_credit$Number.of.existing.credits.at.this.bank. <- as.factor(german_credit$Number.of.existing.credits.at.this.bank.)

german_credit$Number.of.people.being.liable.to.provide.maintenance.for. <- as.factor(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.)

str(german_credit)

## Now that all the required variables are converted as factor variables we do 
## Data preparation for the model

## 1.Credit amount

boxplot(german_credit$Credit.amount)

quantile(german_credit$Credit.amount, probs = seq(0.6,1,0.01))

german_credit$Credit.amount[german_credit$Credit.amount > 7687.880] <- 7687.880

german_credit$Credit.amount <- scale(german_credit$Credit.amount)

## 2.Age in Years

boxplot(german_credit$Age.in.Years)

quantile(german_credit$Age.in.Years, probs = seq(0.5,1,0.01))

german_credit$Age.in.Years[german_credit$Age.in.Years > 57.00] <- 57.00

german_credit$Age.in.Years <- scale(german_credit$Age.in.Years)

## 3.Duration.in.month

summary(german_credit$Duration.in.month)

str(german_credit$Duration.in.month)

boxplot(summary(german_credit$Duration.in.month))

quantile(german_credit$Duration.in.month, probs = seq(0.7,1, 0.01))

german_credit$Duration.in.month[german_credit$Duration.in.month > 42.00 ] <- 42.00

## Now we check the boxplot again to see if outliers have been removed

boxplot(summary(german_credit$Duration.in.month))

## Now since the outliers have been removed we proceed with scaling

german_credit$Duration.in.month <- scale(german_credit$Duration.in.month)


## Dummy variable creation 

## 1.Status of existing checking account

summary(german_credit$Status.of.existing.checking.account)

dummy_1 <- data.frame(model.matrix(~Status.of.existing.checking.account, data = german_credit))[-1]

## 2.Credit.history

summary(german_credit$Credit.history)

dummy_2 <- data.frame(model.matrix(~Credit.history ,data = german_credit))[-1]

## 3.Purpose

summary(german_credit$Purpose)

dummy_3 <- data.frame(model.matrix(~Purpose ,data = german_credit))[-1]

## 4.Savings.account.bonds

summary(german_credit$Savings.account.bonds)

dummy_4 <- data.frame(model.matrix(~Savings.account.bonds,data = german_credit))[-1]

## 5.Present.employment.since.

summary(german_credit$Present.employment.since.)

dummy_5 <- data.frame(model.matrix(~Present.employment.since.,data = german_credit))[-1]

## 6. Installment.rate.in.percentage.of.disposable.income

summary(german_credit$Installment.rate.in.percentage.of.disposable.income)

dummy_6 <- data.frame(model.matrix(~Installment.rate.in.percentage.of.disposable.income,data = german_credit))[-1]

## 7. Personal.status.and.sex

summary(german_credit$Personal.status.and.sex)

dummy_7 <- data.frame(model.matrix(~Personal.status.and.sex,data = german_credit))[-1]

## 8. Other.debtors...guarantors

summary(german_credit$Other.debtors...guarantors)

dummy_8 <- data.frame(model.matrix(~Other.debtors...guarantors,data = german_credit))[-1]

## 9.Present.residence.since

summary(german_credit$Present.residence.since)

dummy_9 <- data.frame(model.matrix(~Present.residence.since,data = german_credit))[-1]

## 10.Property

summary(german_credit$Property)

dummy_10 <- data.frame(model.matrix(~Property,data = german_credit))[-1]

## 11.Other.installment.plans

summary(german_credit$Other.installment.plans)

dummy_11 <- data.frame(model.matrix(~Other.installment.plans,data = german_credit))[-1]


## 12.Housing.

summary(german_credit$Housing.)

dummy_12 <- data.frame(model.matrix(~Housing.,data = german_credit))[-1]


## 13.Number.of.existing.credits.at.this.bank.

summary(german_credit$Number.of.existing.credits.at.this.bank.)

dummy_13 <- data.frame(model.matrix(~Number.of.existing.credits.at.this.bank.,data = german_credit))[-1]


## 14. Job_status

summary(german_credit$Job_status)

dummy_14 <- data.frame(model.matrix(~Job_status,data = german_credit))[-1]


## 15. Number.of.people.being.liable.to.provide.maintenance.for.

summary(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.)

dummy_15 <- data.frame(model.matrix(~Number.of.people.being.liable.to.provide.maintenance.for.,data = german_credit))[-1]


## 16. Telephone.

summary(german_credit$Telephone.)

dummy_16 <- data.frame(model.matrix(~Telephone.,data = german_credit))[-1]

## 17. foreign.worker

summary(german_credit$foreign.worker)

dummy_17 <- data.frame(model.matrix(~foreign.worker,data = german_credit))[-1]

german_credit <- german_credit[c(2,5,13,21)]

german_credit <- cbind(german_credit,dummy_17,dummy_16,dummy_15,dummy_14,dummy_13,dummy_12,dummy_11,dummy_10,dummy_9,dummy_8,dummy_7,dummy_6,dummy_5,dummy_4,dummy_3,dummy_2,dummy_1)


## Merging all the dummy variables into german_credit data frame.

## Dividing into training and test data sets

set.seed(2)

s=sample(1:nrow(german_credit),0.7*nrow(german_credit))
train = german_credit[s,]
test = german_credit[-s,]

# Initial Model with all variables

model_1 <- glm(Default_status~. , family = binomial, data = train)

summary(model_1)

# Stepwise selection

step <- stepAIC(model_1, direction = "both")

summary(step)

### MOdel 2 ########

model_2 <- glm(formula = Default_status ~ Duration.in.month + Credit.amount + 
                 Age.in.Years + foreign.workerA202 + Number.of.people.being.liable.to.provide.maintenance.for.2 + 
                 Number.of.existing.credits.at.this.bank.2 + Number.of.existing.credits.at.this.bank.3 + 
                 Housing.A152 + Other.installment.plansA143 + Present.residence.since2 + 
                 Other.debtors...guarantorsA102 + Other.debtors...guarantorsA103 + 
                 Personal.status.and.sexA93 + Personal.status.and.sexA94 + 
                 Installment.rate.in.percentage.of.disposable.income3 + Installment.rate.in.percentage.of.disposable.income4 + 
                 Present.employment.since.A74 + Savings.account.bondsA64 + 
                 Savings.account.bondsA65 + PurposeA41 + PurposeA42 + PurposeA43 + 
                 PurposeA49 + Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
                 Status.of.existing.checking.accountA14, family = binomial, 
               data = train)

summary(model_2)

vif(model_2)

## we remove  Credit.historyA32  as it has high VIF 


model_3 <- glm(formula = Default_status ~ Duration.in.month + Credit.amount + 
                 Age.in.Years + foreign.workerA202 + Number.of.people.being.liable.to.provide.maintenance.for.2 + 
                 Number.of.existing.credits.at.this.bank.2 + Number.of.existing.credits.at.this.bank.3 + 
                 Housing.A152 + Other.installment.plansA143 + Present.residence.since2 + 
                 Other.debtors...guarantorsA102 + Other.debtors...guarantorsA103 + 
                 Personal.status.and.sexA93 + Personal.status.and.sexA94 + 
                 Installment.rate.in.percentage.of.disposable.income3 + Installment.rate.in.percentage.of.disposable.income4 + 
                 Present.employment.since.A74 + Savings.account.bondsA64 + 
                 Savings.account.bondsA65 + PurposeA41 + PurposeA42 + PurposeA43 + 
                 PurposeA49 + Credit.historyA33 + Credit.historyA34 + 
                 Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
                 Status.of.existing.checking.accountA14, family = binomial, 
               data = train)

summary(model_3)

vif(model_3)

## we remove Credit.amount

model_4 <- glm(formula = Default_status ~ Duration.in.month +
                 Age.in.Years + foreign.workerA202 + Number.of.people.being.liable.to.provide.maintenance.for.2 + 
                 Number.of.existing.credits.at.this.bank.2 + Number.of.existing.credits.at.this.bank.3 + 
                 Housing.A152 + Other.installment.plansA143 + Present.residence.since2 + 
                 Other.debtors...guarantorsA102 + Other.debtors...guarantorsA103 + 
                 Personal.status.and.sexA93 + Personal.status.and.sexA94 + 
                 Installment.rate.in.percentage.of.disposable.income3 + Installment.rate.in.percentage.of.disposable.income4 + 
                 Present.employment.since.A74 + Savings.account.bondsA64 + 
                 Savings.account.bondsA65 + PurposeA41 + PurposeA42 + PurposeA43 + 
                 PurposeA49 + Credit.historyA33 + Credit.historyA34 + 
                 Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
                 Status.of.existing.checking.accountA14, family = binomial, 
               data = train)

summary(model_4)

vif(model_4)

## Now we could see that all the VIF values are below threshold so we use
## P value (significance ) to remove some insignificant variables.

## we remove Installment.rate.in.percentage.of.disposable.income3 

model_5 <- glm(formula = Default_status ~ Duration.in.month +
                 Age.in.Years + foreign.workerA202 + Number.of.people.being.liable.to.provide.maintenance.for.2 + 
                 Number.of.existing.credits.at.this.bank.2 + Number.of.existing.credits.at.this.bank.3 + 
                 Housing.A152 + Other.installment.plansA143 + Present.residence.since2 + 
                 Other.debtors...guarantorsA102 + Other.debtors...guarantorsA103 + 
                 Personal.status.and.sexA93 + Personal.status.and.sexA94 + 
                 Installment.rate.in.percentage.of.disposable.income4 + 
                 Present.employment.since.A74 + Savings.account.bondsA64 + 
                 Savings.account.bondsA65 + PurposeA41 + PurposeA42 + PurposeA43 + 
                 PurposeA49 + Credit.historyA33 + Credit.historyA34 + 
                 Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
                 Status.of.existing.checking.accountA14, family = binomial, 
               data = train)

summary(model_5)

## we remove PurposeA49


model_6 <- glm(formula = Default_status ~ Duration.in.month +
                 Age.in.Years + foreign.workerA202 + Number.of.people.being.liable.to.provide.maintenance.for.2 + 
                 Number.of.existing.credits.at.this.bank.2 + Number.of.existing.credits.at.this.bank.3 + 
                 Housing.A152 + Other.installment.plansA143 + Present.residence.since2 + 
                 Other.debtors...guarantorsA102 + Other.debtors...guarantorsA103 + 
                 Personal.status.and.sexA93 + Personal.status.and.sexA94 + 
                 Installment.rate.in.percentage.of.disposable.income4 + 
                 Present.employment.since.A74 + Savings.account.bondsA64 + 
                 Savings.account.bondsA65 + PurposeA41 + PurposeA42 + PurposeA43 + 
                 Credit.historyA33 + Credit.historyA34 + 
                 Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
                 Status.of.existing.checking.accountA14, family = binomial, 
               data = train)

summary(model_6)

## we remove Number.of.existing.credits.at.this.bank.3

model_7 <- glm(formula = Default_status ~ Duration.in.month +
                 Age.in.Years + foreign.workerA202 + Number.of.people.being.liable.to.provide.maintenance.for.2 + 
                 Number.of.existing.credits.at.this.bank.2 +
                 Housing.A152 + Other.installment.plansA143 + Present.residence.since2 + 
                 Other.debtors...guarantorsA102 + Other.debtors...guarantorsA103 + 
                 Personal.status.and.sexA93 + Personal.status.and.sexA94 + 
                 Installment.rate.in.percentage.of.disposable.income4 + 
                 Present.employment.since.A74 + Savings.account.bondsA64 + 
                 Savings.account.bondsA65 + PurposeA41 + PurposeA42 + PurposeA43 + 
                 Credit.historyA33 + Credit.historyA34 + 
                 Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
                 Status.of.existing.checking.accountA14, family = binomial, 
               data = train)


summary(model_7)

## we remove Age.in.Years 


model_8 <- glm(formula = Default_status ~ Duration.in.month +foreign.workerA202 + Number.of.people.being.liable.to.provide.maintenance.for.2 + 
                 Number.of.existing.credits.at.this.bank.2 +
                 Housing.A152 + Other.installment.plansA143 + Present.residence.since2 + 
                 Other.debtors...guarantorsA102 + Other.debtors...guarantorsA103 + 
                 Personal.status.and.sexA93 + Personal.status.and.sexA94 + 
                 Installment.rate.in.percentage.of.disposable.income4 + 
                 Present.employment.since.A74 + Savings.account.bondsA64 + 
                 Savings.account.bondsA65 + PurposeA41 + PurposeA42 + PurposeA43 + 
                 Credit.historyA33 + Credit.historyA34 + 
                 Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
                 Status.of.existing.checking.accountA14, family = binomial, 
               data = train)

summary(model_8)

## we remove Number.of.people.being.liable.to.provide.maintenance.for.2 

model_9 <- glm(formula = Default_status ~ Duration.in.month +foreign.workerA202 + 
                 Number.of.existing.credits.at.this.bank.2 +
                 Housing.A152 + Other.installment.plansA143 + Present.residence.since2 + 
                 Other.debtors...guarantorsA102 + Other.debtors...guarantorsA103 + 
                 Personal.status.and.sexA93 + Personal.status.and.sexA94 + 
                 Installment.rate.in.percentage.of.disposable.income4 + 
                 Present.employment.since.A74 + Savings.account.bondsA64 + 
                 Savings.account.bondsA65 + PurposeA41 + PurposeA42 + PurposeA43 + 
                 Credit.historyA33 + Credit.historyA34 + 
                 Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
                 Status.of.existing.checking.accountA14, family = binomial, 
               data = train)


summary(model_9)

## we remove Savings.account.bondsA64

model_10 <- glm(formula = Default_status ~ Duration.in.month +foreign.workerA202 + 
                  Number.of.existing.credits.at.this.bank.2 +
                  Housing.A152 + Other.installment.plansA143 + Present.residence.since2 + 
                  Other.debtors...guarantorsA102 + Other.debtors...guarantorsA103 + 
                  Personal.status.and.sexA93 + Personal.status.and.sexA94 + 
                  Installment.rate.in.percentage.of.disposable.income4 + 
                  Present.employment.since.A74 +
                  Savings.account.bondsA65 + PurposeA41 + PurposeA42 + PurposeA43 + 
                  Credit.historyA33 + Credit.historyA34 + 
                  Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
                  Status.of.existing.checking.accountA14, family = binomial, 
                data = train)


summary(model_10)

## we remove Other.debtors...guarantorsA102

model_11 <-  glm(formula = Default_status ~ Duration.in.month +foreign.workerA202 + 
                   Number.of.existing.credits.at.this.bank.2 +
                   Housing.A152 + Other.installment.plansA143 + Present.residence.since2 + 
                   Other.debtors...guarantorsA103 + 
                   Personal.status.and.sexA93 + Personal.status.and.sexA94 + 
                   Installment.rate.in.percentage.of.disposable.income4 + 
                   Present.employment.since.A74 +
                   Savings.account.bondsA65 + PurposeA41 + PurposeA42 + PurposeA43 + 
                   Credit.historyA33 + Credit.historyA34 + 
                   Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
                   Status.of.existing.checking.accountA14, family = binomial, 
                 data = train)

summary(model_11)

## we remove Personal.status.and.sexA94

model_12 <- glm(formula = Default_status ~ Duration.in.month +foreign.workerA202 + 
                  Number.of.existing.credits.at.this.bank.2 +
                  Housing.A152 + Other.installment.plansA143 + Present.residence.since2 + 
                  Other.debtors...guarantorsA103 + 
                  Personal.status.and.sexA93 + 
                  Installment.rate.in.percentage.of.disposable.income4 + 
                  Present.employment.since.A74 +
                  Savings.account.bondsA65 + PurposeA41 + PurposeA42 + PurposeA43 + 
                  Credit.historyA33 + Credit.historyA34 + 
                  Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
                  Status.of.existing.checking.accountA14, family = binomial, 
                data = train)

summary(model_12)

## we remove Personal.status.and.sexA93

model_13 <- glm(formula = Default_status ~ Duration.in.month +foreign.workerA202 + 
                  Number.of.existing.credits.at.this.bank.2 +
                  Housing.A152 + Other.installment.plansA143 + Present.residence.since2 + 
                  Other.debtors...guarantorsA103 + 
                  Installment.rate.in.percentage.of.disposable.income4 + 
                  Present.employment.since.A74 +
                  Savings.account.bondsA65 + PurposeA41 + PurposeA42 + PurposeA43 + 
                  Credit.historyA33 + Credit.historyA34 + 
                  Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
                  Status.of.existing.checking.accountA14, family = binomial, 
                data = train)

summary(model_13)

## we remove Installment.rate.in.percentage.of.disposable.income4


model_14 <- glm(formula = Default_status ~ Duration.in.month +foreign.workerA202 + 
                  Number.of.existing.credits.at.this.bank.2 +
                  Housing.A152 + Other.installment.plansA143 + Present.residence.since2 + 
                  Other.debtors...guarantorsA103 + 
                  Present.employment.since.A74 +
                  Savings.account.bondsA65 + PurposeA41 + PurposeA42 + PurposeA43 + 
                  Credit.historyA33 + Credit.historyA34 + 
                  Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
                  Status.of.existing.checking.accountA14, family = binomial, 
                data = train)

summary(model_14)

## we remove Present.residence.since2


model_14 <- glm(formula = Default_status ~ Duration.in.month +foreign.workerA202 + 
                  Number.of.existing.credits.at.this.bank.2 +Housing.A152 + Other.installment.plansA143 +
                  Other.debtors...guarantorsA103 +  Present.employment.since.A74 +
                  Savings.account.bondsA65 + PurposeA41 + PurposeA42 + PurposeA43 +Credit.historyA33 + Credit.historyA34 + 
                  Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
                  Status.of.existing.checking.accountA14, family = binomial, 
                data = train)

summary(model_14)


train$predicted_prob <- predict(model_14,type = "response")

rcorr.cens(train$predicted_prob,train$Default_status)

## test Data 

test$predicted_prob_test <- predict(model_14, newdata = test, type = "response")

rcorr.cens(test$predicted_prob_test, test$Default_status)

## KS statistic 

## Train Data 

model_score <- prediction(train$predicted_prob, train$Default_status)

model_perf <- performance(model_score, "tpr", "fpr")

ks_table <- attr(model_perf,"y.values")[[1]] - (attr(model_perf, "x.values")[[1]])

ks <- max(ks_table)

ks

which(ks_table == ks)

178/nrow(train)

## 0.2542857

## this means that our KS Statistic lies in the 3rd decile which is good.

model_score_test <- prediction(test$predicted_prob_test, test$Default_status)

model_perf_test <- performance(model_score_test ,"tpr", "fpr")

ks_table1 <- attr(model_perf_test,"y.values")[[1]] - (attr(model_perf_test, "x.values")[[1]])

ks1 <- max(ks_table1)

ks1

which(ks_table1 == ks1)

86/nrow(test)

## for our test data the KS statistic lies in the 3rd decile which is good.

# Selecting threshold value

plot(model_perf, colorize = T,print.cutoffs.at=seq(0,1, by = 0.1))

abline(0,1)

## confusion matrix 

## Training Data set 

confusionMatrix(as.numeric(train$predicted_prob > 0.35),train$Default_status, positive = "1")

## Test Data set

confusionMatrix(as.numeric(test$predicted_prob_test > 0.35),test$Default_status, positive = "1")


