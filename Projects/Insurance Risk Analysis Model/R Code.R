################### Insurance Risk Analysis Model ###################
#### Load Libraries and import data set --------------------------------
library(readxl)
library(dplyr)
library(randomForest)
library(gbm)
library(tidyverse)
library(plyr)

data <- read_excel("UNSW_Assignment_Data.xlsx")

#### Exploratory Data Analysis -----------------------------------------
colSums(is.na(data))
colnames(data)[colSums(is.na(data)) > 0]
dim(data)
summary(data)

data <- data %>%
  filter(totalIncurred > 0, yearsInsured > -0.000001, yearsInsured < 100)

#Counting proportion of missing data 
length(data$roofType[data$roofType == "NA"])/nrow(data)
length(data$roofType[data$roofType == "NA"])/nrow(data)
length(data$roofType[data$roofType == "NA"])/nrow(data)

data$roofType[data$roofType=="NA"]<-NA
data$wallType[data$wallType=="NA"]<-NA
data$floorType[data$floorType=="NA"]<-NA

table(data$peril)

#boxplot of avg claim by locality 
avg_total_locality <- data %>%
  group_by(locality) %>%
  summarise(Mean_Total_Incurred = mean(totalIncurred))

ggplot(data=avg_total_locality, mapping=aes(x=locality , y=Mean_Total_Incurred, color=locality, fill=locality)) +
  geom_bar(stat='identity')

#barplot of avg claim by peril
avg_total_peril <- data %>%
  group_by(peril) %>%
  summarise(Mean_Total_Incurred = mean(totalIncurred))

ggplot(data=avg_total_peril, mapping=aes(x=peril , y=Mean_Total_Incurred, color=peril, fill=peril)) +
  geom_bar(stat='identity')

#boxplot of severity by state
ggplot(data=data, mapping=aes(x=stateRisk , y=log(totalIncurred))) +
  geom_boxplot()


#boxplot of avg claim by locality 
year_claims <- data %>%
  group_by(YearIncurred) %>%
  count(YearIncurred)

#boxplot of severity by state
ggplot(data=year_claims, mapping=aes(x=YearIncurred , y=n)) +
  geom_bar(stat='identity')


#### Creating the Risk Groups ------------------------------------------------
data <- data %>% mutate(claimperpolicy = totalIncurred/buildingSI)

#new dataframe which is the average claim per policy per anzsic2 code  
#grouping by peril
ACD <- data %>% filter(peril == 'Accident Loss or Damage')
Fire <- data %>% filter(peril == 'Fire')
ITP <- data %>% filter(peril == 'Impact by Third Party')
MD <- data %>% filter(peril == 'Malicious Damage')
OPD <- data %>% filter(peril == 'Other Property Damage')
Water <- data %>% filter(peril == 'Water')

#group by anzic 2 code
library(plyr)
ACD.avgclaimpp <- ddply(ACD, .(anzsic2_desc), summarize,  avg=mean(claimperpolicy))
Fire.avgclaimpp <- ddply(Fire, .(anzsic2_desc), summarize,  avg=mean(claimperpolicy))
ITP.avgclaimpp <- ddply(ITP, .(anzsic2_desc), summarize,  avg=mean(claimperpolicy))
MD.avgclaimpp <- ddply(MD, .(anzsic2_desc), summarize,  avg=mean(claimperpolicy))
OPD.avgclaimpp <- ddply(OPD, .(anzsic2_desc), summarize,  avg=mean(claimperpolicy))
Water.avgclaimpp <- ddply(Water, .(anzsic2_desc), summarize,  avg=mean(claimperpolicy))

# k means cluster
ACD.K <- kmeans(ACD.avgclaimpp$avg,20,nstart=20)
Fire.K <- kmeans(Fire.avgclaimpp$avg,20,nstart=20)
ITP.K <- kmeans(ITP.avgclaimpp$avg,20,nstart=20)
MD.K <- kmeans(MD.avgclaimpp$avg,20,nstart=20)
OPD.K <- kmeans(OPD.avgclaimpp$avg,20,nstart=20)
Water.K <- kmeans(Water.avgclaimpp$avg,20,nstart=20)

# order the data properly
ACD.np <- ACD.K$cluster
Fire.np <- Fire.K$cluster
ITP.np <- ITP.K$cluster
MD.np <- MD.K$cluster
OPD.np <- OPD.K$cluster
Water.np <- Water.K$cluster

ACD.od <- order(ACD.K$centers) #lowest to highest
Fire.od <- order(Fire.K$centers)
ITP.od <- order(ITP.K$centers)
MD.od <- order(MD.K$centers)
OPD.od <- order(OPD.K$centers)
Water.od <- order(Water.K$centers)

ACD.nw <- c()
for (i in 1:20){
  c <- ACD.od[i]
  ACD.nw[which(ACD.np==c)] <- i }

Fire.nw <- c()
for (i in 1:20){
  c <- Fire.od[i]
  Fire.nw[which(Fire.np==c)] <- i }

ITP.nw <- c()
for (i in 1:20){
  c <- ITP.od[i]
  ITP.nw[which(ITP.np==c)] <- i }

MD.nw <- c()
for (i in 1:20){
  c <- MD.od[i]
  MD.nw[which(MD.np==c)] <- i }

OPD.nw <- c()
for (i in 1:20){
  c <- OPD.od[i]
  OPD.nw[which(OPD.np==c)] <- i }

Water.nw <- c()
for (i in 1:20){
  c <- Water.od[i]
  Water.nw[which(Water.np==c)] <- i }


ACD.avgclaimpp <- cbind(ACD.avgclaimpp,ACD.nw)
Fire.avgclaimpp <- cbind(Fire.avgclaimpp,Fire.nw)
ITP.avgclaimpp <- cbind(ITP.avgclaimpp,ITP.nw)
MD.avgclaimpp <- cbind(MD.avgclaimpp,MD.nw)
OPD.avgclaimpp <- cbind(OPD.avgclaimpp,OPD.nw)
Water.avgclaimpp <- cbind(Water.avgclaimpp,Water.nw)

# remove avg in col
ACD.avgclaimpp <- ACD.avgclaimpp[,-2]
Fire.avgclaimpp <- Fire.avgclaimpp[,-2]
ITP.avgclaimpp <- ITP.avgclaimpp[,-2]
MD.avgclaimpp <- MD.avgclaimpp[,-2]
OPD.avgclaimpp <- OPD.avgclaimpp[,-2]
Water.avgclaimpp <- Water.avgclaimpp[,-2]

# simple method 
avgclaimpp <- ddply(data, .(anzsic2_desc), summarize,  avg=mean(claimperpolicy))
k <- kmeans(avgclaimpp$avg,20,nstart=20)
np <- k$cluster
od <- order(k$centers) #lowest to highest

nw <- c()
for (i in 1:20){
  c <- od[i]
  nw[which(np==c)] <- i }

avgclaimpp <- cbind(avgclaimpp,nw)
das <- ddply(avgclaimpp, .(nw), summarize,  avg=mean(avg))
ggplot(das, aes(x=nw,y=avg)) + geom_bar(stat="identity", aes(fill=avg)) + 
  labs(x = 'Rating', y = 'Risk Value', fill = 'Risk Value') + 
  ggtitle('Risk Values Across Hazard Rating Levels')

#merge together
Rating <- merge(ACD.avgclaimpp,avgclaimpp, by = 'anzsic2_desc',all=TRUE)
Rating <- merge(Fire.avgclaimpp,Rating, by = 'anzsic2_desc',all=TRUE)
Rating <- merge(ITP.avgclaimpp,Rating, by = 'anzsic2_desc',all=TRUE)
Rating <- merge(MD.avgclaimpp,Rating, by = 'anzsic2_desc',all=TRUE)
Rating <- merge(OPD.avgclaimpp,Rating, by = 'anzsic2_desc',all=TRUE)
Rating <- merge(Water.avgclaimpp,Rating, by = 'anzsic2_desc',all=TRUE)

#make NA equal 0
Rating[is.na(Rating)] <- 0

avg_total_peril <- ddply(data, .(peril), summarize,  avg=mean(totalIncurred))

w_ACD <- avg_total_peril[1,2]/sum(avg_total_peril[,2])
w_Fire <- avg_total_peril[2,2]/sum(avg_total_peril[,2])
w_ITP <- avg_total_peril[3,2]/sum(avg_total_peril[,2])
w_MD <- avg_total_peril[4,2]/sum(avg_total_peril[,2])
w_OPD <- avg_total_peril[5,2]/sum(avg_total_peril[,2])
w_Water <- avg_total_peril[6,2]/sum(avg_total_peril[,2])


# add all peril rating together then K mean again to get final rating
Rating <- Rating %>% mutate(sum = w_Water * `Water.nw`+ w_OPD * `OPD.nw`+ w_MD * `MD.nw`+ w_ITP * `ITP.nw`
                            + w_Fire * `Fire.nw`+ w_ACD * `ACD.nw`)
R.K <- kmeans(Rating$sum,20,nstart=20)
R.np <- R.K$cluster
R.od <- order(R.K$centers) #lowest to highest

R.nw <- c()
for (i in 1:20){
  c <- R.od[i]
  R.nw[which(R.np==c)] <- i }

Rating <- cbind(Rating,R.nw)

Final.Rating <- Rating[,c(1,9,11)]
names(Final.Rating) <- c('anzic2_desc','Simple Method','Complex Method')

unique(data$anzsic2_desc)

data <- data %>% mutate(hazard_rating = avgclaimpp$nw[match(data$anzsic2_desc, avgclaimpp$anzic2_desc)])

for (i in 1:6711){
  a <- data$anzsic2_desc[i]
  b <- which(Final.Rating$anzic2_desc == a)
  c <- Final.Rating$`Complex Method`[b]
  data$hazard_rating[i] = c
}

#### Claims Cost Modelling -----------------------------------------------
# random forest and boosting
data$roofType <- data$roofType%>%
  replace_na('none')
data$wallType <- data$roofType%>%
  replace_na('none')
data$floorType <- data$roofType%>%
  replace_na('none')

data$lossDescription <- as.factor(data$lossDescription)
data$stateRisk <- as.factor(data$stateRisk)
data$anzsic2_desc <- as.factor(data$anzsic2_desc)
data$occupancy <- as.factor(data$occupancy)
data$locality <- as.factor(data$locality)

data$roofType <- as.factor(data$roofType)
data$wallType <- as.factor(data$wallType)
data$floorType <- as.factor(data$floorType)


data$fireprotecAlarm <- as.factor(data$fireprotecAlarm)
data$fireprotecBlanket <- as.factor(data$fireprotecBlanket)
data$fireprotecFexting <- as.factor(data$fireprotecFexting)
data$fireprotecHose <- as.factor(data$fireprotecHose)
data$fireprotecMonitored <- as.factor(data$fireprotecMonitored)
data$fireprotecMthermal <- as.factor(data$fireprotecMthermal)
data$fireprotecNmonitored <- as.factor(data$fireprotecNmonitored)
data$fireprotecNthermal <- as.factor(data$fireprotecNthermal)

# dummy variables
Dummy <- function(var1, short, dat2){
  names(dat2)[names(dat2) == var1]  <- "V1"
  n2 <- ncol(dat2)
  dat2$X <- as.integer(dat2$V1)
  n0 <- length(unique(dat2$X))
  for (n1 in 2:n0){dat2[, paste(short, n1, sep="")] <- as.integer(dat2$X==n1)}
  names(dat2)[names(dat2) == "V1"]  <- var1
  dat2[, c(1:n2,(n2+2):ncol(dat2))]
}

data <- Dummy("lossDescription", "lossdescriptor", data)
data <- Dummy("stateRisk", "stateriskdescriptor", data)
data <- Dummy("anzsic2_desc", "anzsic2descriptor", data)
data <- Dummy("locality", "localitydescriptor", data)
data <- Dummy("roofType", "roofTypedescriptor", data)
data <- Dummy("hazard_rating", "hazard_ratingdescriptor", data)

#remove useless columns
data2 <- data[-c(3,5,6:16)]

nrow1 <- round((0.75*nrow(data2))) ;nrow1
nrow2 <- round(( (0.75*nrow(data2) ) +1.5)) ;nrow2
nrow3 <- (nrow(data2)-1) ;nrow3

#Train should be 1 to 0.75*5060
traindata3 <- data2[1: nrow1 ,]
#test is 6745-5062=~1684
testdata3 <- data2[ nrow2:nrow3,]
num_var <- c(1,2,5,7,25)
train_set <- traindata3
test_set <- testdata3

#
train_water <- train_set %>% filter(peril=="Water")
train_ALD <- train_set %>% filter(peril == 'Accident Loss or Damage')
train_Fire <- train_set %>% filter(peril == 'Fire')
train_ITP <- train_set %>% filter(peril == 'Impact by Third Party')
train_OPD <- train_set %>% filter(peril == 'Other Property Damage')
train_MD <- train_set %>% filter(peril=="Malicious Damage")

test_water <- test_set %>% filter(peril=="Water")
test_ALD <- test_set %>% filter(peril == 'Accident Loss or Damage')
test_Fire <- test_set %>% filter(peril == 'Fire')
test_ITP <- test_set %>% filter(peril == 'Impact by Third Party')
test_OPD <- test_set %>% filter(peril == 'Other Property Damage')
test_MD <- test_set %>% filter(peril=="Malicious Damage")

#remove peril from data
train_water <- train_water[-3]
train_ALD <- train_ALD[-3]
train_Fire <- train_Fire[-3]
train_ITP <- train_ITP[-3]
train_OPD <- train_OPD[-3]
train_MD <- train_MD[-3]

test_water <- test_water[-3]
test_ALD <- test_ALD[-3]
test_Fire <- test_Fire[-3]
test_ITP <- test_ITP[-3]
test_OPD <- test_OPD[-3]
test_MD <- test_MD[-3]

# Random forest
RF_water <- randomForest(totalIncurred~., data=train_water,importance=TRUE,na.action = na.omit)
RF_ALD <- randomForest(totalIncurred~., data=train_ALD,importance=TRUE,na.action = na.omit)
RF_Fire <- randomForest(totalIncurred~., data=train_Fire,importance=TRUE,na.action = na.omit)
RF_ITP <- randomForest(totalIncurred~., data=train_ITP,importance=TRUE,na.action = na.omit)
RF_OPD <- randomForest(totalIncurred~., data=train_OPD,importance=TRUE,na.action = na.omit)
RF_MD <- randomForest(totalIncurred~., data=train_MD,importance=TRUE,na.action = na.omit)

importance(RF_water)
varImpPlot(RF_water)
plot(RF_water)

#prediction accuracy
pred_water = predict (RF_water ,newdata = test_water) # a lot of NA due to missing rooftype (might want to impute)
pred_ALD = predict (RF_ALD ,newdata = test_ALD)
pred_Fire = predict (RF_Fire ,newdata = test_Fire)
pred_ITP = predict (RF_ITP ,newdata = test_ITP)
pred_OPD = predict (RF_ALD ,newdata = test_OPD)
pred_MD = predict (RF_MD ,newdata = test_MD)


# water plot
plot(pred_water, test_water$totalIncurred, xlim = c(0,50000), ylim = c(0,50000), xlab = 'Predicted', ylab = 'Observed', main = 'RF Water Claims Predicted vs Observed')
abline (0,1)
plot(pred_ALD, test_ALD$totalIncurred)

#mean square error test set
pw <- (pred_water - test_water$totalIncurred)^2
pw <- mean(pw)^0.5

pa <- (pred_ALD - test_ALD$totalIncurred)^2
pa <- mean(pa)^0.5

pf <- (pred_Fire - test_Fire$totalIncurred)^2
pf <- mean(pf)^0.5

pi <- (pred_ITP - test_ITP$totalIncurred)^2
pi <- mean(pi)^0.5

po <- (pred_OPD - test_OPD$totalIncurred)^2
po <- mean(po)^0.5

pm <- (pred_MD - test_MD$totalIncurred)^2
pm <- mean(pm)^0.5

#boosting
boost.water <- gbm(
  formula = totalIncurred ~ .,
  distribution = "gaussian",
  data = train_water,
  n.trees = 5000,
  interaction.depth = 6,
  shrinkage = 0.01,
  bag.fraction = 0.65,
  n.minobsinnode = 10,
  cv.folds = 5,
  n.cores = NULL, 
  verbose = FALSE
)

boost.ALD <- gbm(
  formula = totalIncurred ~ .,
  distribution = "gaussian",
  data = train_ALD,
  n.trees = 5000,
  interaction.depth = 6,
  shrinkage = 0.01,
  bag.fraction = 0.65,
  n.minobsinnode = 10,
  cv.folds = 5,
  n.cores = NULL, 
  verbose = FALSE
)

boost.Fire <- gbm(
  formula = totalIncurred ~ .,
  distribution = "gaussian",
  data = train_Fire,
  n.trees = 5000,
  interaction.depth = 6,
  shrinkage = 0.01,
  bag.fraction = 0.65,
  n.minobsinnode = 10,
  cv.folds = 5,
  n.cores = NULL, 
  verbose = FALSE
)

boost.ITP <- gbm(
  formula = totalIncurred ~ .,
  distribution = "gaussian",
  data = train_ITP,
  n.trees = 5000,
  interaction.depth = 6,
  shrinkage = 0.01,
  bag.fraction = 0.65,
  n.minobsinnode = 10,
  cv.folds = 5,
  n.cores = NULL, 
  verbose = FALSE
)

boost.OPD <- gbm(
  formula = totalIncurred ~ .,
  distribution = "gaussian",
  data = train_OPD,
  n.trees = 5000,
  interaction.depth = 6,
  shrinkage = 0.01,
  bag.fraction = 0.65,
  n.minobsinnode = 10,
  cv.folds = 5,
  n.cores = NULL, 
  verbose = FALSE
)

boost.MD <- gbm(
  formula = totalIncurred ~ .,
  distribution = "gaussian",
  data = train_MD,
  n.trees = 5000,
  interaction.depth = 6,
  shrinkage = 0.01,
  bag.fraction = 0.65,
  n.minobsinnode = 10,
  cv.folds = 5,
  n.cores = NULL, 
  verbose = FALSE
)

#prediction
pred_waterb = predict (boost.water ,newdata = test_water) # a lot of NA due to missing rooftype (might want to impute)
pred_ALDb = predict (boost.ALD ,newdata = test_ALD) # a lot of NA due to missing rooftype (might want to impute)
pred_Fireb = predict (boost.Fire ,newdata = test_Fire) # a lot of NA due to missing rooftype (might want to impute)
pred_ITPb = predict (boost.ITP ,newdata = test_ITP) # a lot of NA due to missing rooftype (might want to impute)
pred_OPDb = predict (boost.OPD ,newdata = test_OPD) # a lot of NA due to missing rooftype (might want to impute)
pred_MDb = predict (boost.MD ,newdata = test_MD) # a lot of NA due to missing rooftype (might want to impute)


pwb <- (pred_waterb - test_water$totalIncurred)^2
pwb <- mean(pwb)^0.5

pab <- (pred_ALDb - test_ALD$totalIncurred)^2
pab <- mean(pab)^0.5

pfb <- (pred_Fireb - test_Fire$totalIncurred)^2
pfb <- mean(pfb)^0.5

pib <- (pred_ITPb - test_ITP$totalIncurred)^2
pib <- mean(pib)^0.5
 
pob <- (pred_OPDb - test_OPD$totalIncurred)^2
pob <- mean(pob)^0.5

pmb <- (pred_MDb - test_MD$totalIncurred)^2
pmb <- mean(pmb)^0.5


plot(pred_waterb, test_water$totalIncurred, xlim = c(0,50000), ylim = c(0,50000), xlab = 'Predicted', ylab = 'Observed', main = 'Boosting Water Claims Predicted vs Observed')

# plot rmse comparison
peril <- c(rep('Water',2),rep('Accident Loss or Damage',2),rep('Fire',2),rep('Impact by Third Party',2),rep('Other Property Damage',2),rep('Malicious Damage',2))
method <- rep(c('Random Forest','Boosting'),6)
rmsee <- c(pw,pwb,pa,pab,pf,pfb,pi,pib,po,pob,pm,pmb)
rmse <- cbind(peril,method,rmsee)
rmse <- as.data.frame(rmse)
rmse$rmsee <- as.numeric(rmse$rmsee)

ggplot(rmse, aes(fill=method, y=rmsee, x=peril)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(y = 'RMSE', x = 'Peril', title = 'Random Forest vs Boosting RMSE')


# neural network Test RMSe
sqrt(0.003571874)*(max(train_ALD$totalIncurred)-min(train_ALD$totalIncurred)) +min(train_ALD$totalIncurred)

sqrt(0.01758203)*(max(train_water$totalIncurred)-min(train_water$totalIncurred)) +min(train_water$totalIncurred)

sqrt(0.004747883)*(max(train_Fire$totalIncurred)-min(train_Fire$totalIncurred)) +min(train_Fire$totalIncurred)

sqrt(0.005122598)*(max(train_ITP$totalIncurred)-min(train_ITP$totalIncurred)) +min(train_ITP$totalIncurred)

sqrt(0.004624145)*(max(train_OPD$totalIncurred)-min(train_OPD$totalIncurred)) +min(train_OPD$totalIncurred)

sqrt(0.003101574)*(max(train_MD$totalIncurred)-min(train_MD$totalIncurred)) +min(train_MD$totalIncurred)

