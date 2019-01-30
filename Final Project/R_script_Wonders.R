
######load environment
mypackages = c("randomForest",'gbm','rpart','gdata','glmnet')   # required packages
tmp = setdiff(mypackages, rownames(installed.packages()))  # packages need to be installed
if (length(tmp) > 0) install.packages(tmp)
lapply(mypackages, require, character.only = TRUE)


######load data
train<-read.csv('application2017.csv')
test<-read.csv('application2018.csv')

#######Data Preprocessing (Select Variables)
#first check the margin effect of variables one by one
#combine highly correlated variables

#Check date ---not significant
train$date<-as.Date(train$date,format = "%m-%d-%Y")
train$date<-as.numeric(format(train$date,"%m"))
train$date<-as.factor(train$date)
datemod<-lm(amt_paid~date,data=train)
summary(datemod)

#Check statecode ---not significant
train$statecode<-as.factor(train$statecode)
statecodemod<-lm(amt_paid~statecode,data=train)
summary(statecodemod)

#Check age ----not significant
agemod<-lm(amt_paid~age,data = train)
summary(agemod)
plot(train$age,train$amt_paid)

#Check EDUC---- not significant
train$educ<-as.factor(train$educ)
edumod<-lm(amt_paid~educ,data = train)
summary(edumod)

#Check unemprate----not significant
unempratemod<-lm(amt_paid~unemprate,data = train)
summary(unempratemod)

#Check married----significant!
train$married<-as.factor(train$married)
marriedmod<-lm(amt_paid~married,data = train)
summary(marriedmod)

#Check W2inc_m2 and W2inc_m1 highly correlated or not
cor(train$W2inc_m1,train$W2inc_m2)
#it seems they are highly correlated, so create a new variable W2inc=(W2inc_m2+ W2inc_m1)/2

#Check W2inc----significant!
W2inc=(train$W2inc_m2+train$W2inc_m1)/2
mod<-lm(train$amt_paid~W2inc)
summary(mod)

#Check taxdependent----significant!
train$taxdependent<-as.factor(train$taxdependent)
taxmod<-lm(amt_paid~taxdependent,data = train)
summary(taxmod)

#Check asset (since it is self-reported)----significant!
assmod<-lm(amt_paid~asset,data = train)
summary(assmod)

#Check debt (since it is self-reported)----significant!
debtmod<-lm(amt_paid~debt,data = train)
summary(debtmod)


#Check avg_homeprice ----significant!
homemod<-lm(amt_paid~avg_homeprice,data = train)
summary(homemod)

#Check creditscore ----significant!
creditscoremod<-lm(amt_paid~creditscore,data = train)
summary(creditscoremod)

#since loan_amt and amt_due express similar info and highly correlated, remove amt_due


###########################begin

Preprocessing<-function(data){
#change categorical variable as factor
  data$married<-as.factor(data$married)
  data$taxdependent<-as.factor(data$taxdependent)
  data$W2inc<-(data$W2inc_m1+data$W2inc_m2)/2
  
#remove necessary variable
  data<-data[,!names(data)%in%c("id", "name" ,"SSN","date","amt_due","statecode","age",
                                "educ", "W2inc_m1","W2inc_m2","unemprate")]
  return(data)
}

train_clean<-Preprocessing(train)
test_clean<-Preprocessing(test)

#models
#1.linear regression
#stepwise selection based on AIC
model<-step(lm(amt_paid~.,data=train_clean),direction = "both",k = 2)
summary(model)
lm_pred<-predict(model, new =test_clean)
result0<-ifelse(lm_pred>= test_clean$loan_amt,1,0)

#2.lasso
x<-model.matrix(amt_paid~.^2,data = train_clean)[,-1]
amtpaid<-rep(0,250)
test_clean_lasso<-cbind(test_clean,amtpaid)
# Fit the CV model
set.seed(1)
cvfit <- cv.glmnet(x, train_clean$amt_paid,alpha=1,nfolds = 3,type.measure = "mse")
lasso.pred <- predict(cvfit,s = cvfit$lambda.min, newx = model.matrix(amtpaid~.^2,data = test_clean_lasso)[,-1])
result1<-ifelse(lasso.pred>= test_clean$loan_amt,1,0)

#3.decision tree
# Fit the tree 
treemod<- rpart(amt_paid ~.,data = train_clean)
tree_predict <- predict(treemod, newdata = test_clean)
result2<-ifelse(tree_predict>= test_clean$loan_amt,1,0)

#4.random forest
#limiting the number of predictors =2
set.seed(1)
rf_mod<- randomForest(amt_paid ~ ., data = train_clean, mtry = 2,ntree=100, importance = TRUE)
rf_predict <- predict(rf_mod, test_clean)
result3<-ifelse(rf_predict>= test_clean$loan_amt,1,0)

#5.Boosting
set.seed(1)
boost_mod <- gbm(amt_paid ~ ., data = train_clean, distribution = "gaussian", n.trees = 100, interaction.depth = 5) 
boost_predict <- predict(boost_mod, test_clean, n.trees = 100)
result4<-ifelse(boost_predict>= test_clean$loan_amt,1,0)

#####combine the result
result<-cbind(result0,result1,result2,result3,result4)
result<-as.data.frame(result)
names(result)<-c('linear','lasso','tree','randomforest','boosting')

result$final<-result$linear+result$lasso+result$tree+result$randomforest+result$boosting
result$final<-ifelse(result$final>=3,1,0)

######Output
# Output csv file of 2018 prediction
test$APPROVE<-result$final
output<-test[,names(test)%in%c('id','name','APPROVE')]
colnames(output)<-c('ID','NAME','APPROVE')
row.names(output)<-NULL
write.csv(output,row.names=FALSE,quote=FALSE,
          file = "Loan_file_Wonders.csv")

#1.people amount
cat("The number of people intend to give loans:",sum(result$final))

#2.total loan amount
loan_info<-test[,names(test)%in%c('id','name','APPROVE', "loan_amt" )]
cat("The total loan amounts :", sum(loan_info[loan_info$APPROVE ==1,]$loan_amt))

#anticipated resulting profit
pred_amtpaid<-(lm_pred+lasso.pred+tree_predict+rf_predict+boost_predict)/5
test$pred_amtpaid<-pred_amtpaid
profit_info<-test[test$APPROVE ==1,]
cat("Anticipated resulting profit :",sum(profit_info$pred_amtpaid-profit_info$loan_amt))



