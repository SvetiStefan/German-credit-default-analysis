#Save the required packages in a variable
load_packages <- c("ggplot2","car","Hmisc","ROCR","caret","dummies","caTools",
                   "MASS", "gridExtra")
#load the packages
lapply(load_packages,require,character.only = TRUE)


# Download the data set as german_credit
german_credit <- read.csv("german.csv",stringsAsFactors = FALSE)

# Data prepartion and feature transformation

#check for NA values
sum(is.na(german_credit))
#check for duplicated values
sum(duplicated(german_credit))
#check the structure and summary
str(german_credit)
summary(german_credit)

#outlier treatment
#check for outliers in numeric variables
#If outliers found than do capping and flooring
boxplot.stats(german_credit$Duration.in.month)
boxplot(german_credit$Duration.in.month)
table(german_credit$Duration.in.month)
quantile(german_credit$Duration.in.month,seq(0,1,0.01))
german_credit$Duration.in.month[which(german_credit$Duration.in.month>42)]<-quantile(german_credit$Duration.in.month,0.92)

boxplot.stats(german_credit$Credit.amount)
boxplot(german_credit$Credit.amount)
table(german_credit$Credit.amount)
quantile(german_credit$Credit.amount,seq(0,1,0.01))
german_credit$Credit.amount[which(german_credit$Credit.amount>7687.88)]<-quantile(german_credit$Credit.amount,0.92)

boxplot.stats(german_credit$Age.in.Years)
boxplot(german_credit$Age.in.Years)
table(german_credit$Age.in.Years)
quantile(german_credit$Age.in.Years,seq(0,1,0.01))
german_credit$Age.in.Years[which(german_credit$Age.in.Years>63)]<-quantile(german_credit$Age.in.Years,0.97)


#There are four numeric values in this variable 1,2,3,4
#As there are equally spaced it is considered as numeric itself
boxplot.stats(german_credit$Number.of.existing.credits.at.this.bank.)
boxplot(german_credit$Number.of.existing.credits.at.this.bank.)
table(german_credit$Number.of.existing.credits.at.this.bank.)



#There are four numeric values in this variable 1,2
#As there are equally spaced it is considered as numeric itself
boxplot.stats(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.)
boxplot(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.)
table(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.)


#There are four numeric values in this variable 1,2,3,4
#As there are equally spaced it is considered as numeric itself
boxplot.stats(german_credit$Installment.rate.in.percentage.of.disposable.income)
boxplot(german_credit$Installment.rate.in.percentage.of.disposable.income)
table(german_credit$Installment.rate.in.percentage.of.disposable.income)


#There are four numeric values in this variable 1,2,3,4
#As there are equally spaced it is considered as numeric itself
boxplot.stats(german_credit$Present.residence.since)
boxplot(german_credit$Present.residence.since)
table(german_credit$Present.residence.since)

# Exploratory Data Analysis
#univariate plots

#plot credit history
  loan_credit_history <-  ggplot(german_credit,aes(x=Credit.history,fill=factor(Default_status)))+geom_bar()+
   scale_x_discrete(limits=c("A32","A34","A33","A31","A30"))+guides(fill=FALSE)
   
#plot loan purpose
loan_purpose <-  ggplot(german_credit,aes(x=Purpose,fill=factor(Default_status)))+geom_bar( )+
     scale_x_discrete(limits=c("A43","A40","A42","A41","A49","A46","A45",
                            "A44","A410","A48"))+guides(fill=FALSE)
  

#plot credit amount
credit_amount <-  ggplot(german_credit,aes(x=Credit.amount,
                                fill=factor(Default_status)))+geom_histogram(binwidth = 500)+guides(fill=FALSE)

#plot Installment_rate
Installment_rate <- ggplot(german_credit,aes(x=factor(Installment.rate.in.percentage.of.disposable.income),
                        fill=factor(Default_status)))+geom_bar()+
                      guides(fill=guide_legend(reverse=TRUE))+scale_fill_discrete(labels=c("Good","Bad"))+
                    labs(fill='Loan status')+scale_x_discrete(limits=c("4","2","3","1"))

grid.arrange(loan_credit_history,loan_purpose,
                      credit_amount,Installment_rate, ncol=2,
                      top = "Univariate Analysis")
#plot loan property
loan_propery <-  ggplot(german_credit,aes(x=Property,fill=factor(Default_status)))+geom_bar( )+
                  scale_x_discrete(limits=c("A123","A121","A122","A124"))+guides(fill=FALSE)
 
#plot housing                           
loan_housing <-  ggplot(german_credit,aes(x=Housing.,fill=factor(Default_status)))+geom_bar( )+
                  scale_x_discrete(limits=c("A152","A151","A153"))+guides(fill=FALSE)

#plot loan job
loan_job <-  ggplot(german_credit,aes(x=Job_status,fill=factor(Default_status)))+geom_bar( )+
              scale_x_discrete(limits=c("A173","A172","A174","A171"))+guides(fill=FALSE)

#plot loan telephone
loan_telephone <- ggplot(german_credit,aes(x=Telephone.,fill=factor(Default_status)))+geom_bar( )+
                  guides(fill=guide_legend(reverse=TRUE))+
                  labs(fill='Loan status')+scale_fill_discrete(labels=c("Good","Bad"))

grid.arrange(loan_housing,loan_job,
             loan_propery,loan_telephone, ncol=2,
             top = "Univariate Analysis")

#plot checking account
loan_checking_account <- ggplot(german_credit,aes(x=Status.of.existing.checking.account,fill=factor(Default_status)))+geom_bar( )+
                   scale_x_discrete(limits=c("A14","A11","A12","A13"))+guides(fill=FALSE)

#plot savings bonds
loan_savings_bonds <- ggplot(german_credit,aes(x=Savings.account.bonds,fill=factor(Default_status)))+geom_bar( )+
                       scale_x_discrete(limits=c("A61","A65","A62","A63","A64"))+guides(fill=FALSE)

#plot loan duration
loan_duration <- ggplot(german_credit,aes(x=Duration.in.month,fill=factor(Default_status)))+geom_histogram(binwidth = 5)+
                  guides(fill=FALSE)


#plot loan other debtors
loan_debtors <- ggplot(german_credit,aes(x=Other.debtors...guarantors,fill=factor(Default_status)))+geom_bar( )+
  guides(fill=guide_legend(reverse=TRUE))+scale_x_discrete(limits=c("A101","A103","A102"))+
  labs(fill='Loan status')+scale_fill_discrete(labels=c("Good","Bad"))


grid.arrange(loan_checking_account,loan_debtors,loan_duration,
             loan_savings_bonds,ncol=2,top = "Univariate Analysis")

#separate all the character variables for data preparation
#Convert the variables from character type to factor
german_credit_char <- as.data.frame(unclass(german_credit[,-c(2,5,8,11,13,16,18,21)]))
#check the structure
str(german_credit_char)

#create dummy variables for the factor variables using dummies package
german_credit_dummy <- dummy.data.frame(german_credit_char,all = FALSE)
#Remove one  dummy column  from all factor variables
german_credit_dummy <- german_credit_dummy[,-c(1,5,10,20,25,30,34,37,41,44,47,51,53)]
#Remove the original character variables from the dataframe
german_credit <- german_credit[,-c(1,3,4,6,7,9,10,12,14,15,17,19,20)]
#Bind the german credit dataframe with dummy variables dataframe
german_credit <- cbind(german_credit,german_credit_dummy)
#Check the dataframe. It should consist only numerical variables
str(german_credit)

#check for number of good/bad loans
table(german_credit$Default_status)

#split the dataframe into train and test data frames
#Ensure the same proportion of class labels in train and test datasets
#Set seed 100 for ensuring same results every time
set.seed(100)
split_indices <- sample.split(german_credit$Default_status,0.7)
train <- german_credit[split_indices==TRUE,]
test <- german_credit[split_indices==FALSE,]
table(train$Default_status)
table(test$Default_status)



# Initial Model with all variables
initial_model <- glm(Default_status~.,family = binomial, data=train)
summary(initial_model)


# Perform Stepwise selection for removing the insignificant variables
step <- stepAIC(initial_model,direction="both")
step$call
#StepAIC reduced the number of variables from 49 to 21

# Remove multicollinearity through VIF check
#Use the model obtained from stepAIC and check for collinearity
model_2 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                 Age.in.Years + Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41 + PurposeA410 + PurposeA42 + PurposeA43 + PurposeA45 + 
                 PurposeA49 + Savings.account.bondsA64 + Savings.account.bondsA65 + 
                 Present.employment.since.A74 + Personal.status.and.sexA93 + 
                 Other.debtors...guarantorsA103 + Other.installment.plansA143 + 
                 Housing.A152, family = binomial, data = train)
summary(model_2)
vif(model_2)

#Remove the insignificant variable purposeA410
model_3 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                 Age.in.Years + Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41  + PurposeA42 + PurposeA43 + PurposeA45 + 
                 PurposeA49 + Savings.account.bondsA64 + Savings.account.bondsA65 + 
                 Present.employment.since.A74 + Personal.status.and.sexA93 + 
                 Other.debtors...guarantorsA103 + Other.installment.plansA143 + 
                 Housing.A152, family = binomial, data = train)
summary(model_3)
vif(model_3)

#Remove the insignificant variable PurposeA49
model_4 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                 Age.in.Years + Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41  + PurposeA42 + PurposeA43 + PurposeA45 + 
                 Savings.account.bondsA64 + Savings.account.bondsA65 + 
                 Present.employment.since.A74 + Personal.status.and.sexA93 + 
                 Other.debtors...guarantorsA103 + Other.installment.plansA143 + 
                 Housing.A152, family = binomial, data = train)
summary(model_4)
vif(model_4)

#Remove the insignificant variable personel status and sexA93
model_5 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                 Age.in.Years + Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41  + PurposeA42 + PurposeA43 + PurposeA45 + 
                 Savings.account.bondsA64 + Savings.account.bondsA65 + 
                 Present.employment.since.A74  + 
                 Other.debtors...guarantorsA103 + Other.installment.plansA143 + 
                 Housing.A152, family = binomial, data = train)
summary(model_5)
vif(model_5)

#Remove the insignificant variable PurposeA45
model_6 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                 Age.in.Years + Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41  + PurposeA42 + PurposeA43 +  
                 Savings.account.bondsA64 + Savings.account.bondsA65 + 
                 Present.employment.since.A74  + 
                 Other.debtors...guarantorsA103 + Other.installment.plansA143 + 
                 Housing.A152, family = binomial, data = train)
summary(model_6)
vif(model_6)

#Remove the insignificant variable PurposeA43
model_7 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                 Age.in.Years + Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41  + PurposeA42 +  
                 Savings.account.bondsA64 + Savings.account.bondsA65 + 
                 Present.employment.since.A74  + 
                 Other.debtors...guarantorsA103 + Other.installment.plansA143 + 
                 Housing.A152, family = binomial, data = train)
summary(model_7)
vif(model_7)

#Remove the insignificant variable PurposeA42
model_8 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                 Age.in.Years + Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41  +  
                 Savings.account.bondsA64 + Savings.account.bondsA65 + 
                 Present.employment.since.A74  + 
                 Other.debtors...guarantorsA103 + Other.installment.plansA143 + 
                 Housing.A152, family = binomial, data = train)
summary(model_8)
vif(model_8)

#Remove the insignifaicant variable Age in years
model_9 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                 Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41  +  
                 Savings.account.bondsA64 + Savings.account.bondsA65 + 
                 Present.employment.since.A74  + 
                 Other.debtors...guarantorsA103 + Other.installment.plansA143 + 
                 Housing.A152, family = binomial, data = train)
summary(model_9)
vif(model_9)
#Further removing the variables is increasing the AIC by a big margin
#All the variables are significant
#As vif of all variables are less than 3, model_9 is considered as the final model


# c-statistic and KS -statistic
#Calculate the predicted probabilities of train default status
train$predicted_prob <- predict(model_9,type="response")
#calculate the c-statistic
#Higher value of c-statistic indicate that the model is good
rcorr.cens(train$predicted_prob,train$Default_status)

#Utility of the model will be determined by the results from test dataset
#calculate the c-statistic
#Higher value of c-statistic indicate that the model is good
test$predicted_probs <- predict(model_9,newdata=test[,-8],type="response")
rcorr.cens(test$predicted_probs,test$Default_status)
#c-statistic of 7.6915 indicates that the model is good

#calculate the KS statistic
#create 'predictions' object
model_score <- prediction(train$predicted_prob,train$Default_status)
#Evaluate the performace of the model
model_perf <- performance(model_score, "tpr", "fpr")
#plot the model
plot(model_perf)
#subtract the cumulative TPR from cumulative FPR
ks_table <- attr(model_perf, "y.values")[[1]] - (attr(model_perf, "x.values")[[1]])
#Find out the value with max distance, which is the KS statistic
ks = max(ks_table)
#Find the decile in which the max value occurs
which(ks_table == ks)/nrow(train)

#The true test of the model is the test dataset
#calculate ks statistic for test dataset
#create 'predictions' object
model_score_test <- prediction(test$predicted_probs,test$Default_status)
#Evaluate the performace of the model
model_perf_test <- performance(model_score_test, "tpr", "fpr")
#subtract the cumulative TPR from cumulative FPR
ks_table_test <- attr(model_perf_test, "y.values")[[1]] - (attr(model_perf_test, "x.values")[[1]])
#Find out the value with max distance, which is the KS statistic
ks_test = max(ks_table_test)
#Find the decile in which the max value occurs
which(ks_table_test == ks_test)/nrow(test)
#As the ks statistic is 44.44% which is above 40 and
#it is occuring in the 4th decile.
#As the c and KS statistic of the model is good, it is considered for utilisation


# Selecting threshold value
#plot the ROC curve for the test data
ROC_credit <- performance(model_score_test,measure="tpr",x.measure="fpr")
plot(ROC_credit,col="red",label=c(10,10,10))

#confusion Matrix for train data
confusionMatrix(as.numeric(train$predicted_prob>0.2),train$Default_status,positive = '1')
confusionMatrix(as.numeric(train$predicted_prob>0.28989295),train$Default_status,positive = '1')
confusionMatrix(as.numeric(train$predicted_prob>0.04206682),train$Default_status,positive = '1')


#The aim of the project is to reduce the number of defaulters.
#The business aim is to minimize the number of loans given to defaulters.
#The False positives needs to be reduced. So the sensitivity needs to be high
confusionMatrix(as.numeric(test$predicted_probs > 0.2),test$Default_status, positive = "1")
#For the threshold level of 0.2, the sensitivity is 81% and accuracy of the model is 64%
#Even though the accuracy of the model is less, the aim is to achieve high sensitivity
#In order to achieve this a threshold level of 0.2 is selected

#plot sensitivity vs specificity 
ss <- performance(model_score_test,"sens","spec")
plot(ss)
#Find the probaility for which sensitivity is maximum
ss@alpha.values[[1]][which.max(ss@y.values[[1]])]
#see the misclassification table for the max sensitivity probability value
confusionMatrix(as.numeric(test$predicted_probs > 0.04206682),test$Default_status, positive = "1")

#Check the structure of performance object
str(ss)
#create a dataframe with cutoff values, true positive and false positive rate
cutoffs <- data.frame(cut=ss@alpha.values[[1]],tpr=ss@y.values[[1]],
                     fpr=ss@x.values[[1]])

#Make a additional column for cutoff dataframe 
cutoffs$opt <- cutoffs$tpr+cutoffs$fpr-1

#If the business objective is to achieve high sensitivity and specificity than the
#Threshold is 0.28989295
confusionMatrix(as.numeric(test$predicted_probs > 0.28989295),test$Default_status, positive = "1")


#If the business objective is to achieve 90% high sensitivity  than the
#Threshold is 0.133223885
confusionMatrix(as.numeric(test$predicted_probs > 0.133223885),test$Default_status, positive = "1")


#If the business objective is to achieve high sensitivity  than the
#Threshold is 0.28989295
confusionMatrix(as.numeric(test$predicted_probs > 0.04206682),test$Default_status, positive = "1")


#Depending on the business objective any threshold level can be selected
#End of assignment
#___________________________________________________________________________________________________________________________
