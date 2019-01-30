
mypackages = c("randomForest", "glmnet",'gbm','rpart','glmnet')   # required packages
tmp = setdiff(mypackages, rownames(installed.packages()))  # packages need to be installed
if (length(tmp) > 0) install.packages(tmp)
lapply(mypackages, require, character.only = TRUE)

##################################begain
######load data
alldata<-read.csv('application2017.csv')

set.seed(1)
train_index<-sample(1:250,220)#220 training,30 testing
train<-alldata[train_index,]
test<-alldata[-train_index,]

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

####################
train_clean<-Preprocessing(train)
train_clean_noy<-train_clean[,!names(train_clean)%in% c('amt_paid')]
test_clean<-Preprocessing(test)
test_clean_noy<-test_clean[,!names(test_clean)%in% c('amt_paid')]
test_y<-test_clean[,names(test_clean)%in% c('amt_paid')]


#calculate profit
cal_profit<-function(pred_amtpaid,loan_amount,act_amtpaid){
  result<-cbind(pred_amtpaid,loan_amount,act_amtpaid)
  result<-as.data.frame(result)
  colnames(result)<-c('pred_amtpaid','loan_amount','act_amtpaid')
  rownames(result)<-c()
  result$loan_index<-ifelse(result$pred_amtpaid>= result$loan_amount,1,0)
  giveloan<-result[result$loan_index == 1,]
  profit<-0
  for (i in 1:nrow(giveloan)){
    profit<-giveloan[i,3]-giveloan[i,2]+profit
  }
  return(list(profit=profit,prediction = result$loan_index))
}

#model
#1.linear regression
#stepwise based on AIC
model<-step(lm(amt_paid~.,data=train_clean),direction = "both",k = 2)
summary(model)
lm_pred<-predict(model, new =test_clean_noy)

accrate<-matrix(nrow = 1, ncol = 5)
trytest0<-cal_profit(lm_pred,test$loan_amt,test$amt_paid)$prediction
real<-ifelse(test$loan_amt>=test$amt_paid,0,1)
accrate[1,1]<-sum(trytest0 == real)/length(real)

#2.lasso
x<-model.matrix(amt_paid~.^2,data = train_clean)[,-1]
set.seed(1)
cvfit <- cv.glmnet(x, train_clean$amt_paid,alpha=1,nfolds = 3,type.measure = "mse")
lasso.pred <- predict(cvfit,s = cvfit$lambda.min, newx = model.matrix(amt_paid~.^2,data = test_clean)[,-1])
trytest1<-cal_profit(lasso.pred,test$loan_amt,test$amt_paid)$prediction
accrate[1,2]<-sum(trytest1 == real)/length(real)


#3.decision tree
# Fit the tree 
treemod<- rpart(amt_paid ~.,data = train_clean)
# Predict in the test data
tree_predict <- predict(treemod, newdata = test_clean_noy)
trytest2<-cal_profit(tree_predict,test$loan_amt,test$amt_paid)$prediction
accrate[1,3]<-sum(trytest2 == real)/length(real)

#4.Random Forest
#limiting the number of predictors =2
set.seed(1)
rf_mod<- randomForest(amt_paid ~ ., data = train_clean, mtry = 2,ntree=100, importance = TRUE)
rf_predict <- predict(rf_mod, test_clean_noy)
trytest3<-cal_profit(rf_predict,test$loan_amt,test$amt_paid)$prediction
accrate[1,4]<-sum(trytest3 == real)/length(real)

#5.Boosting #good
set.seed(1)
boost_mod <- gbm(amt_paid ~ ., data = train_clean, distribution = "gaussian", n.trees = 2000, interaction.depth = 1) 
boost_predict <- predict(boost_mod, test_clean, n.trees = 2000)
trytest4<-cal_profit(boost_predict,test$loan_amt,test$amt_paid)$prediction
accrate[1,5]<-sum(trytest4 == real)/length(real)

accrate<-as.data.frame(accrate)
colnames(accrate)<-c('Linear Regression','Lasso','Decision Tree','Bagging','Boosting')
accrate
