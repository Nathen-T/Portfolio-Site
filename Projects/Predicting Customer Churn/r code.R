###################### Predicting Customer Churn ###################### 
#### Load Libraries -------------------------------------------
library(dplyr)
library(Hmisc)
library(ROSE)
library(leaps)
library(caret)
library(MASS)
library(class)
library(tree)
library(glmnet)
library(ggplot2)
library(tornado)
library(pROC)

#### Data Preparation -------------------------------------------
#Importing Datasets
training_data <- read.csv("D:/Uni Work/Year 3/ACTL3142/Assignment/training_data.csv")
eval_data <- read.csv("D:/Uni Work/Year 3/ACTL3142/Assignment/eval_data.csv")
#training_data <- read.csv("Documents/Portfolio/Uni Projects/ACTL3142/training_data.csv")

#Removing any duplicates
training_data <- unique(training_data)

#check for NA
sapply(training_data,function(x) sum(is.na(x)))

#omit NA
a <- na.omit(training_data)
summary(training_data)

#change to 0 (no) and 1 (yes)
training_data <- training_data %>% 
  mutate(Churn = ifelse(Churn == "No",0,1)) %>%
  mutate(ChildrenInHH = ifelse(ChildrenInHH == "No",0,1) ) %>%
  mutate(HandsetRefurbished = ifelse(HandsetRefurbished == "No",0,1)) %>%
  mutate(HandsetWebCapable = ifelse(HandsetWebCapable == "No",0,1 )) %>%
  mutate(TruckOwner = ifelse(TruckOwner == "No",0,1)) %>% 
  mutate(RVOwner = ifelse(RVOwner == "No",0,1)) %>%
  mutate(BuysViaMailOrder = ifelse(BuysViaMailOrder == "No",0,1)) %>% 
  mutate(RespondsToMailOffers = ifelse(RespondsToMailOffers == "No",0,1)) %>%
  mutate(OptOutMailings = ifelse(OptOutMailings == "No",0,1)) %>%
  mutate(OwnsComputer = ifelse(OwnsComputer == "No",0,1)) %>%
  mutate(HasCreditCard = ifelse(HasCreditCard == "No",0,1)) %>%
  mutate(NewCellphoneUser = ifelse(NewCellphoneUser == "No",0,1)) %>%
  mutate(OwnsMotorcycle = ifelse(OwnsMotorcycle == "No",0,1)) %>%
  mutate(MadeCallToRetentionTeam = ifelse(MadeCallToRetentionTeam == "No",0,1)) %>% 
  mutate(MaritalStatus = ifelse(MaritalStatus == "No", 0, ifelse(MaritalStatus == "Yes", 1, NA)))

#service area vs churn
sc <- training_data %>% 
  select(Churn,ServiceArea)
names <- as.data.frame(unique(sc$ServiceArea))
result <- vector('numeric',90)
for(i in 1:719){
  a <- names[i,1]
  b <- sc %>% filter(ServiceArea == a)
  b <- as.data.frame(table(b))
  t <- b$Freq[1] + b$Freq[2]
  r <- b$Freq[2]/t
  result[i] <- r
}
result <- as.data.frame(result)
count <- as.data.frame(table(sc$ServiceArea))
summary(count) #median is 11 not enough data to determine by area

#handsetprice 
table(training_data$HandsetPrice)
22748/40000 #too much unknown data (0.5678) for a regression 

#marriagestatus
table(training_data$MaritalStatus) #too much unknown remove from data
training_data <- training_data %>%
  select(-CustomerID,-DroppedBlockedCalls,-MadeCallToRetentionTeam,-RespondsToMailOffers,-ServiceArea,-HandsetPrice,-MaritalStatus)

#Hmisc
impute <- aregImpute(~AgeHH1 + AgeHH2 + RoamingCalls + OverageMinutes + TotalRecurringCharge + MonthlyMinutes + MonthlyRevenue + CurrentEquipmentDays + HandsetModels + Handsets, data = training_data, n.impute = 5)
#very low R^2 for Age, RoamingCalls, CurrentEquipmentDays(only one NA) and MaritalStatus
#possible delection of 

#subbing in imputed data
training_data$AgeHH1[is.na(training_data$AgeHH1)] <- impute$imputed$AgeHH1
training_data$AgeHH2[is.na(training_data$AgeHH2)] <- impute$imputed$AgeHH2
training_data$RoamingCalls[is.na(training_data$RoamingCalls)] <- impute$imputed$RoamingCalls
training_data$OverageMinutes[is.na(training_data$OverageMinutes)] <- impute$imputed$OverageMinutes
training_data$TotalRecurringCharge[is.na(training_data$TotalRecurringCharge)] <- impute$imputed$TotalRecurringCharge
training_data$MonthlyMinutes[is.na(training_data$MonthlyMinutes)] <- impute$imputed$MonthlyMinutes
training_data$MonthlyRevenue[is.na(training_data$MonthlyRevenue)] <- impute$imputed$MonthlyRevenue
training_data$CurrentEquipmentDays[is.na(training_data$CurrentEquipmentDays)] <- impute$imputed$CurrentEquipmentDays
training_data$HandsetModels[is.na(training_data$HandsetModels)] <- impute$imputed$HandsetModels
training_data$Handsets[is.na(training_data$Handsets)] <- impute$imputed$Handsets
training_data$MaritalStatus[is.na(training_data$MaritalStatus)] <- impute$imputed$MaritalStatus


#splitting creditrating and prizm code and occupation
training_data <- training_data %>% 
  mutate(RatingLowest = ifelse(CreditRating == '7-Lowest', 1, ifelse(CreditRating == '6-VeryLow', 1, 0))) %>%
  mutate(RatingMid = ifelse(CreditRating == '5-Low',1,ifelse(CreditRating == '4-Medium',1,0))) %>% 
  mutate(RatingHigh = ifelse(CreditRating == '3-Good',1,ifelse(CreditRating == '2-High',1,0))) %>%
  mutate(Town = ifelse(PrizmCode == 'Town',1,0)) %>%
  mutate(Suburban = ifelse(PrizmCode == 'Suburban',1,0)) %>% 
  mutate(Rural = ifelse(PrizmCode == 'Rural',1,0)) %>%
  mutate(Student = ifelse(Occupation == 'Student',1,0)) %>%
  mutate(self = ifelse(Occupation == 'Self',1,0)) %>%
  mutate(Retired = ifelse(Occupation == 'Retired',1,0)) %>%
  mutate(Professional = ifelse(Occupation == 'Professional',1,0)) %>%
  mutate(Homemaker = ifelse(Occupation == 'Homemaker',1,0)) %>%
  mutate(Crafts = ifelse(Occupation == 'Crafts',1,0)) %>%
  mutate(Clerical = ifelse(Occupation == 'Clerical',1,0)) %>%
  select(-CreditRating,-PrizmCode,-Occupation)



#imbalance data generate data synthetically
prop.table(table(training_data$Churn))
Balanced_data <- ROSE(Churn~.,data = training_data,seed = 1)$data
#with model selection u get shit test error rate

#### Variable Selection -------------------------------------------
#foward and backward selection linear fit
regfit.fwd = regsubsets(Churn~.,training_data,nvmax=46,method='forward')
regfit.bwd = regsubsets(Churn~.,training_data,nvmax=46,method='backward')
fwd <- summary(regfit.fwd)
bwd <- summary(regfit.bwd)
plot(fwd$rsq,main='Foward selection',xlab='Number of Variables',ylab='Rsquare',type='l')
plot(bwd$rsq,main='Backward selection',xlab='Number of Variables',ylab='Rsquare',type='l')
plot(fwd$bic)
plot(fwd$adjr2)
plot(fwd$cp) # best model is 15 variable
#very low R^2 
#probs look at models with 10-20 variables
#using balanced data
regfit.fwdb = regsubsets(Churn~.,Balanced_data,nvmax=46,method='forward')
regfit.bwdb = regsubsets(Churn~.,Balanced_data,nvmax=46,method='backward')
fwdb <- summary(regfit.fwdb)
bwdb <- summary(regfit.bwdb)
plot(fwdb$rsq,main='Foward selection',xlab='Number of Variables',ylab='Rsquare',type='l')
plot(bwdb$rsq,main='Backward selection',xlab='Number of Variables',ylab='Rsquare',type='l')
plot(fwdb$bic)
plot(fwdb$adjr2)
plot(fwdb$cp) #best model is 15 variable 

#### Regularisation ------------------------------------------
#ridge regression and the lasso
x <- model.matrix(Churn~.,training_data)
y <- training_data$Churn
library(glmnet)
grid <- 10^seq(10,-2,length=100)
set.seed(1)
tr <- sample(1:nrow(x),nrow(x)/2)
te <- (-tr)
y.te <- y[te]
ridge <- glmnet(x[tr,],y[tr],alpha=0,lambda=grid)
#cross validation to choose lambda
set.seed(1)
cv <- cv.glmnet(x[tr,],y[tr],alpha=0)
plot(cv)
best <- cv$lambda.min
best
#predicton
ridge.p <- predict(ridge,s=best,newx=x[te,])
pre <- rep("No",length(te))
pre[ridge.p>.5]="Yes"
act <- factor(y.te,levels=c(0,1), labels=c("No","yes"))
table(pre,act)
mean(pre!=act) # test error rate 0.2946
#Lasso
lasso <- glmnet(x[tr,],y[tr],alpha=1,lambda=grid)
plot(lasso)
set.seed(1)
cvl <- cv.glmnet(x[tr,],y[tr],alpha=1)
plot(cvl)
bestl <- cvl$lambda.min
lasso.p <- predict(lasso,s=bestl,newx=x[te,])
prel <- rep("No",length(te))
prel[lasso.p>.5]="Yes"
actl <- factor(y.te,levels=c(0,1), labels=c("No","yes"))
table(prel,actl)
mean(prel!=actl) # test error rate 0.28945
##logistic regression and ridge/lasso
log.ridge <- glmnet(x[tr,],y[tr],family=binomial,alpha=0,lambda=grid)
#cross validation to choose lambda
set.seed(1)
log.cv <- cv.glmnet(x[tr,],y[tr],alpha=0)
plot(log.cv)
log.best <- log.cv$lambda.min
log.best
#predicton
log.ridge.p <- predict(log.ridge,s=log.best,newx=x[te,])
log.pre <- rep("No",length(te))
log.pre[log.ridge.p>.5]="Yes"
log.act <- factor(y.te,levels=c(0,1), labels=c("No","yes"))
table(log.pre,log.act)
mean(log.pre!=log.act) #test error rate 0.28955
#Lasso with logistic regression
log.lasso <- glmnet(x[tr,],y[tr],family=binomial,alpha=1,lambda=grid)
plot(log.lasso)
set.seed(1)
log.cvl <- cv.glmnet(x[tr,],y[tr],alpha=1)
plot(log.cvl)
log.bestl <- log.cvl$lambda.min
log.lasso.p <- predict(log.lasso,s=log.bestl,newx=x[te,])
log.prel <- rep("No",length(te))
log.prel[log.lasso.p>.5]="Yes"
log.actl <- factor(y.te,levels=c(0,1), labels=c("No","yes"))
table(log.prel,log.actl)
mean(log.prel!=log.actl) #test error rate 0.28865
#predictors from lasso
out <- glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestl)

#use on balanced data lasso log regression
xb <- model.matrix(Churn~.,Balanced_data)
yb <- Balanced_data$Churn
log.lassob <- glmnet(xb[tr,],yb[tr],family=binomial,alpha=1,lambda=grid)
plot(log.lassob)
set.seed(1)
log.cvlb <- cv.glmnet(xb[tr,],yb[tr],alpha=1)
plot(log.cvlb)
log.bestlb <- log.cvlb$lambda.min
log.lassob.p <- predict(log.lassob,s=log.bestlb,newx=x[te,])
log.prelb <- rep("No",length(te))
log.prelb[log.lassob.p>.5]="Yes"
log.actlb <- factor(y.te,levels=c(0,1), labels=c("No","yes"))
table(log.prelb,log.actlb)
mean(log.prelb!=log.actlb) # test error rate 0.29125

#### Model Selection ---------------------------------------------------
#fit a logistic regression full model
#train data
n <- nrow(training_data)
train_indices <- 1:round(0.5 * n)
train <- training_data[train_indices, ]
test_indices <- (round(0.5 * n) + 1):n
test <- training_data[test_indices, ]
#shuffle the training_data to get a different train set
set.seed(1)
rows <- sample(nrow(training_data))
d <- training_data[rows,]
#fitting the model
gl <- glm(Churn~.,data=train,family=binomial)
glp <- predict(gl,test,type='response')
glpr <- rep("No",20000)
glpr[glp>.5]="Yes"
testchurn <- factor(test$Churn, levels=c(0,1), labels=c("No","yes"))
table(glpr,testchurn)
mean(glpr!=testchurn) #test error rate 0.2965
#use balance data
n <- nrow(Balanced_data)
set.seed(1)
rows <- sample(nrow(Balanced_data))
Balanced_data <- Balanced_data[rows,]
trainb_indices <- 1:round(0.5 * n)
trainb <- Balanced_data[trainb_indices, ]
testb_indices <- (round(0.5 * n) + 1):n
testb <- Balanced_data[testb_indices, ]
glb <- glm(Churn~.,data=trainb,family=binomial)
glpb <- predict(glb,testb,type='response')
glprb <- rep("No",20000)
glprb[glpb>.5]="Yes"
testchurnb <- factor(testb$Churn, levels=c(0,1), labels=c("No","yes"))
table(glprb,testchurnb)
mean(glprb!=testchurnb) # error rate 0.7044
#predict on the other test set
p <- predict(glb,test,type='response')
pr <- rep("No",20000)
pr[p>.5]='Yes'
table(pr,testchurn)
mean(pr!=testchurn) # test error rate 0.5506

#log regress on best 15 variable 
best_variables <- training_data %>% select(Churn,MonthlyRevenue,MonthlyMinutes,TotalRecurringCharge,DroppedCalls,MonthsInService,Handsets,CurrentEquipmentDays,AgeHH1,ChildrenInHH,HandsetRefurbished,HandsetWebCapable,RespondsToMailOffers,MadeCallToRetentionTeam,RatingLowest,RatingMid)
set.seed(1)
tr <- sample(1:nrow(best_variables),nrow(best_variables)/2)
bv <- glm(Churn~.,data=best_variables[tr,],family=binomial)
bvp <- predict(bv,best_variables[-tr,],type='response')
bvpr <- rep("No",20000)
bvpr[bvp>.5]="Yes"
ans <- factor(best_variables$Churn[-tr], levels=c(0,1), labels=c("No","yes"))
table(bvpr,ans)
mean(bvpr!=ans) # test error rate 0.2966
anova(bv,test='Chisq') #monthsinservice & ratinglowest insignifcant
#log regress on best 20 variables
best_variables2 <- training_data %>% select(Churn,MonthlyRevenue,MonthlyMinutes,TotalRecurringCharge,DroppedCalls,CustomerCareCalls,MonthsInService,Handsets,CurrentEquipmentDays,AgeHH1,ChildrenInHH,HandsetRefurbished,HandsetWebCapable,RespondsToMailOffers,RetentionCalls,RetentionOffersAccepted,MadeCallToRetentionTeam,RatingLowest,RatingMid,Town,Rural)
set.seed(1)
tr <- sample(1:nrow(best_variables2),nrow(best_variables2)/2)
bv2 <- glm(Churn~.,data=best_variables2[tr,],family=binomial)
bvp2 <- predict(bv2,best_variables2[-tr,],type='response')
bvpr2 <- rep("No",20000)
bvpr2[bvp2>.5]="Yes"
ans2 <- factor(best_variables2$Churn[-tr], levels=c(0,1), labels=c("No","yes"))
table(bvpr2,ans2)
mean(bvpr2!=ans2) # test error rate 0.2981

#best model from forward and backward selection
set.seed(1)
train.control <- trainControl(method = "cv", number = 10)
training_data <- training_data %>% mutate(Churn = factor(Churn,levels=c(0,1), labels=c("No","yes")))
back.model <- train(Churn ~., data = training_data,method = "leapBackward",tuneGrid = data.frame(nvmax = 1:5),trControl = train.control)

#fit a Linear Discriminant Analysis LDA
lda <- lda(Churn~.,data=train)
ldap <- predict(lda,test)
ldac <- factor(ldap$class, levels=c(0,1), labels=c("No","yes"))
table(ldac,testchurn)
mean(ldac!=testchurn) #test error rate 0.2904
# use balance data
ldab <- lda(Churn~.,data=trainb)
ldapb <- predict(ldab,testb)
ldacb <- factor(ldapb$class, levels=c(0,1), labels=c("No","yes"))
table(ldacb,testchurnb)
mean(ldacb!=testchurnb) #0.4421

#fit a Quandratic Discriminant Analysis QDA
qda <- qda(Churn~.,data=train)
qdap <- predict(qda,test)
qdac <- factor(qdap$class, levels=c(0,1), labels=c("No","yes"))
table(qdac,testchurn)
mean(qdac!=testchurn) #test error rate 0.339
#use balance data
qdab <- qda(Churn~.,data=trainb)
qdapb <- predict(qdab,testb)
qdacb <- factor(qdapb$class, levels=c(0,1), labels=c("No","yes"))
table(qdacb,testchurnb)
mean(qdacb!=testchurnb) #test error rate 0.3611

#fit KNN
train.x <- select(train,-Churn)
train.x <- as.matrix(train.x)
test.x <- select(test,-Churn)
test.x <- as.matrix(test.x)
trainchurn <- factor(train$Churn, levels=c(0,1), labels=c("No","yes"))
set.seed(1)
knnp <- knn(train.x,test.x,trainchurn,k=1)
table(knnp,testchurn)
mean(knnp!=testchurn) 
#test error rate k=1 0.3863, k=3 0.3512, k=5 0.3355, k=10 0.3104
#k=15 0.2999 k=20 0.2974 k=18 0.295 (best)

#classification trees
tree <- tree(Churn~.,training_data)
set.seed(1)
tree.tr <- sample(1:nrow(training_data),nrow(training_data)/2)
tree.te <- training_data[-tree.tr,] 
tree.testchurn <- training_data$Churn[-tree.tr]
tree.train <- tree(Churn~.,training_data,subset=tree.tr)
tree.pred <- predict(tree.train,tree.te,type="class")
table(tree.pred,tree.testchurn) #test error rate 0.28865
#regression tree
set.seed(1)
rtree.train <- tree(Churn~.,training_data,subset=tree.tr)

#try fit the best model from fwd and bwd selection
gl <- glm(Churn~.,training_data,family=binomial)

#### Final Model Selection (13 Variables) -------------------------------------------
#foward and backward selection linear fit
regfit.fwd = regsubsets(Churn~.,training_data,nvmax=46,method='forward')
regfit.bwd = regsubsets(Churn~.,training_data,nvmax=46,method='backward')
fwd <- summary(regfit.fwd)
bwd <- summary(regfit.bwd)
plot(fwd$rsq,main='Foward selection',xlab='Number of Variables',ylab='Rsquare',type='l')
plot(bwd$rsq,main='Backward selection',xlab='Number of Variables',ylab='Rsquare',type='l')
plot(fwd$bic,main='Foward selection',xlab='Number of Variables',ylab='BIC')
plot(fwd$adjr2,main='Foward selection',xlab='Number of Variables',ylab='adjusted R square')
plot(fwd$cp,main='Foward selection',xlab='Number of Variables',ylab='Cp') # best model is 13 variable
coef(regfit.fwd,13)

####______________logistic regression on best 13 variable ------------------------------------
best_variables <- training_data %>% select(Churn,MonthlyRevenue,MonthlyMinutes,TotalRecurringCharge,DroppedCalls,MonthsInService,Handsets,CurrentEquipmentDays,AgeHH1,HandsetRefurbished,HandsetWebCapable,RetentionCalls,RatingHigh,RatingMid)
set.seed(1)
tr <- sample(1:nrow(best_variables),nrow(best_variables)/2)
bv <- glm(Churn~.,data=best_variables[tr,],family=binomial)
bvp <- predict(bv,best_variables[-tr,],type='response')
bvpr <- rep("No",20000)
bvpr[bvp>.5]="Yes"
ans <- factor(best_variables$Churn[-tr], levels=c(0,1), labels=c("No","yes"))
table(bvpr,ans) # 0.29065
mean(bvpr!=ans) # test error rate 0.29825

#LDA on best 13 variable
best.lda <- lda(Churn~.,data=best_variables[tr,])
best.ldap <- predict(best.lda,best_variables[-tr,])
best.ldac <- factor(best.ldap$class, levels=c(0,1), labels=c("No","yes"))
table(best.ldac,ans)
mean(best.ldac!=ans) #test error rate 0.2902

#QDA on best 13 variable
best.qda <- qda(Churn~.,data=best_variables[tr,])
best.qdap <- predict(best.qda,best_variables[-tr,])
best.qdac <- factor(best.qdap$class, levels=c(0,1), labels=c("No","yes"))
table(best.qdac,ans)
mean(best.qdac!=ans) #test error rate 0.31715

#KNN on best 13 variable
best.train.x <- select(best_variables[tr,],-Churn)
best.train.x <- as.matrix(best.train.x)
best.test.x <- select(best_variables[-tr,],-Churn)
best.test.x <- as.matrix(best.test.x)
best.trainchurn <- factor(best_variables[tr,]$Churn, levels=c(0,1), labels=c("No","yes"))
set.seed(1)
best.knnp <- knn(best.train.x,best.test.x,best.trainchurn,k=1)
table(best.knnp,ans)
mean(best.knnp!=ans) 
#k=18 0.3179, k=40 0.3011, k=50 0.2983, k=60 0.2965
#k=70 0.2942, k=100 0.2922, k=200 0.2912, k=300 0.29075
#k=250 0.29075
#it predict all no for lowest error rate.

##ridge regression
x <- model.matrix(Churn~.,training_data)
y <- training_data$Churn
grid <- 10^seq(10,-2,length=100)
set.seed(1)
tr <- sample(1:nrow(x),nrow(x)/2)
te <- (-tr)
y.te <- y[te]
ridge <- glmnet(x[tr,],y[tr],alpha=0,lambda=grid)
#cross validation to choose lambda
set.seed(1)
cv <- cv.glmnet(x[tr,],y[tr],alpha=0)
plot(cv)
best <- cv$lambda.min
best
#predicton
ridge.p <- predict(ridge,s=best,newx=x[te,])
pre <- rep("No",length(te))
pre[ridge.p>.5]="Yes"
act <- factor(y.te,levels=c(0,1), labels=c("No","yes"))
table(pre,act) # 0.28955
mean(pre!=act) # test error rate 0.29465
#Lasso
lasso <- glmnet(x[tr,],y[tr],alpha=1,lambda=grid)
plot(lasso)
set.seed(1)
cvl <- cv.glmnet(x[tr,],y[tr],alpha=1)
plot(cvl)
bestl <- cvl$lambda.min
lasso.p <- predict(lasso,s=bestl,newx=x[te,])
prel <- rep("No",length(te))
prel[lasso.p>.5]="Yes"
actl <- factor(y.te,levels=c(0,1), labels=c("No","yes"))
table(prel,actl) # 0.2888
mean(prel!=actl) # test error rate 0.28915
#predictors from lasso
out <- glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestl)


##logistic regression and ridge/lasso
log.ridge <- glmnet(x[tr,],y[tr],family=binomial,alpha=0,lambda=grid)
#cross validation to choose lambda
set.seed(1)
log.cv <- cv.glmnet(x[tr,],y[tr],alpha=0)
plot(log.cv)
log.best <- log.cv$lambda.min
log.best
#predicton
log.ridge.p <- predict(log.ridge,s=log.best,newx=x[te,])
log.pre <- rep("No",length(te))
log.pre[log.ridge.p>.5]="Yes"
log.act <- factor(y.te,levels=c(0,1), labels=c("No","yes"))
table(log.pre,log.act) #0.28895
mean(log.pre!=log.act) #test error rate 0.2895
#Lasso with logistic regression
log.lasso <- glmnet(x[tr,],y[tr],family=binomial,alpha=1,lambda=grid)
plot(log.lasso)
set.seed(1)
log.cvl <- cv.glmnet(x[tr,],y[tr],alpha=1)
plot(log.cvl)
log.bestl <- log.cvl$lambda.min
log.lasso.p <- predict(log.lasso,s=log.bestl,newx=x[te,])
log.prel <- rep("No",length(te))
log.prel[log.lasso.p>.5]="Yes"
log.actl <- factor(y.te,levels=c(0,1), labels=c("No","yes"))
table(log.prel,log.actl) # 0.2885
mean(log.prel!=log.actl) #test error rate 0.28865
#predictors from lasso logistic regression
log.out <- glmnet(x,y,alpha=1,lambda=grid)
log.lasso.coef=predict(out,type="coefficients",s=log.bestl)
#same coefficients as lasso 

#### Importance Graph ----------------------------------------------
#training the final model
best_variables <- training_data %>% 
  select(Churn,MonthlyRevenue,MonthlyMinutes,TotalRecurringCharge,
         DroppedCalls,MonthsInService,Handsets,CurrentEquipmentDays,
         AgeHH1,HandsetRefurbished,HandsetWebCapable,RetentionCalls,RatingHigh,RatingMid)
set.seed(1)
bv <- glm(Churn~.,data=best_variables,family=binomial)
#null model
bv_null <- glm(Churn ~ 1, data = best_variables, family = binomial)
imp <- importance(bv, bv_null)
plot(imp)

torn1 <- tornado::tornado(bv, type = "ranges", alpha = 0.10)
p <- plot(torn1, xlabel = "Churn", geom_bar_control = list(width = 0.4))
p + ggtitle('Important Variables Influencing Churn')

#Auc Graph
bvp <- predict(bv,best_variables,type='response')
par(pty = 's')
roc(best_variables$Churn ~ bvp, plot = TRUE, legacy.axes = TRUE, percent = TRUE, main = 'Logistic Regression ROC curve',
           xlab = 'False Positive Percentage', ylab = 'True Positive Percentage')
