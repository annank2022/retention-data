########################################################
title: "551 Final project"
author: "Kennth Annan"
date: "05/05/2021"  
################################################

library(dplyr)
setwd("C:\\Users\\toshiba\\Desktop\\STAT551\\Finals.project.551")
data=read.csv("LendingClub2021.csv",header=TRUE, sep=",", na.strings=c("","NA"))
#head(data)
#summary(data)
#summary(data$MonthsSinceLastRec)
#remove duplicate values
nrow(data)##  26562 obs and 31 variable    
dim(data)
mydata <- data[which(!duplicated(data$LOAN_ID)),]## check duplicate ID
nrow(mydata)## no duplciate ID
#custRetention <- mydata[,-c(15:17)]
summary(mydata)
## cjange some variabls to factors
data$IssuedDate=as.factor(data$IssuedDate)## IssuedDate)
data$State=as.factor(data$State)## state
data$HomeOwnership=as.factor(data$HomeOwnership)##HomeOwnership
data$EarliestCREDITLine=as.factor(data$EarliestCREDITLine)##EarliestCREDITLine
data$Education=as.factor(data$Education)##eduaction
data$EmploymentLength=as.factor(data$EmploymentLength)##EmploymentLength
#data$appl_fico_band=as.factor(data$appl_fico_band)#
data$MFMonth=as.factor(data$MFMonth)#MFMonth
data$vintage=as.factor(data$vintage)#vintage
data$LoanPurpose=as.factor(data$LoanPurpose)##LoanPurpose
dat=data
summary(dat)

dat3=dat %>% mutate(appl_fico_band=ifelse(appl_fico_band<=729,"Good", ifelse(appl_fico_band<=799,"VeryGood","exellent")))
dat3$appl_fico_band=as.factor(dat3$appl_fico_band)#
summary(dat3$appl_fico_band)## code the fico score variable and transform it.

head(dat3)
summary(dat3$Education)##26254 NAs for edu
levels(dat3$Education)

## visiual display of the missing
library(naniar)
library(ggplot2)
gg_miss_var(dat3)## plot to show the missing records of the variables after keeping the observations with 80% complete cases.

#Replace unusual obs(999) in MonthsSinceDQ by NAs

#Replace unusual obs(999) in MonthsSinceDQ by NAs
 
dat3$MonthsSinceDQ[dat3$MonthsSinceDQ== 999] <- NA
   
## missing impuatation

## for contious variables
#replace MonthsSinceDQ and NAs by zero b,cos there are few obs and mean might not be a good idea. 
dat3$MonthsSinceDQ[is.na(dat3$MonthsSinceDQ)] <- 0
dat3$MonthsSinceLastRec[is.na(dat3$MonthsSinceLastRec)] <- 0

## for categorcal variables
library(tidyverse)
 dat3 <- dat3%>%mutate_if(is.factor, fct_explicit_na, na_level = 'missing')
 summary(dat3)

 #new plot shows that there are no Nas
 #library(naniar)
#library(ggplot2)
#gg_miss_var(data)## plot to show the missing records of the variables after keeping the observations with 80% complete cases.
 
 summary(dat3$MonthsSinceDQ)## summary shows no missng value
 
 
 ## create New attributes

 
a= as.numeric(format(as.Date(dat3$EarliestCREDITL, format="%m/%d/%Y"),"%Y"))#dat$EarliestCREDITL
b= as.numeric( format(as.Date(dat3$IssuedDate, format="%m/%d/%Y"),"%Y"))## extraxt year for dat$IssuedDate
  
  #dat$IssuedDate-dat$EarliestCREDITL
  d=b-a
  dat3$year=b-a## no yrs a peerson has hold a credit card
  # dat$Crbal.ratio=dat$OpenCREDITLines/dat$TotalCREDITLines## credit balance ratio
  # dat$Resbal.ratio=dat$RevolvingCREDITBalance/dat$Amount.Requested##RevolvingCREDITBalance ratio
   #
   #summary(dat$Crbal.ratio)
   
   summary(dat3)
    levels(dat3$EmploymentLength)[12]="missing" ## rename the NAs in employmenyt lenth as missing
   
    dim(dat3)
    
    ## remove variables with zero mean and one of the redundant variables TERm and ID
    
    head(dat3)
    pdat=dat3[,-c(1,2,15,16,17,19,23,24)]
    ## code edu as Yes or No
    pdat$Education <- ifelse(pdat$Education == "missing", 'No', 'Yes')
   pdat<- pdat%>%mutate(Education=as.factor(Education))
    #pdat$Education=as.factor( pdat$Education)
    
    head(pdat)
    pdat1=pdat[,-c(2,4,7,20)] #delete issue date, state, earlieest date,mfmomnth
head(pdat1)
 # pdat1$MonthlyIncome= log(pdat1$MonthlyIncome)## log the monthly incomne
   #pdat1$Amount.Requested= log(pdat1$Amount.Requested)## log the monthly incomne
 
var(dat3$MOB)
var(dat3$AccountsDQ)
var(dat3$DelinquentAmount)
var(dat3$DQ2yrs)
var(dat3$PublicRec)
var(dat3$currentpolicy)
var(dat3$term)
dim(pdat1)
summary(pdat1$Education)
pdat2=pdat1[,-c(1,15,16,18)]# new data for building the models
#################################################################################



set.seed(201111)
smp_size <- floor(0.6 *nrow(pdat2))
# ## set the seed to make your partition reproducible
train_ind <- sample(seq_len(nrow(pdat2)), size = smp_size)
train <-pdat2[train_ind, ]
test <- pdat2[-train_ind, ]
#summary(train)
dim(train)
dim(test)






#significant variables
library(leaps)
 select.var <- regsubsets(target ~.,
                    data = train, really.big = T)## use the regsubsets to select significant variables
#fit a mars model
 summary(select.var)
######################################################################










#brary(randomForest)
 #set.seed(101)
#modelRF <- randomForest(target ~.,
                    #data = pdat1, ntree=20, importance = TRUE)#


#important <- importance(modelRF, type=1 )
#fit a mars model


## data explorations

## continous  predictors Vs response



attach(pdat1)
par(mfrow=c(1,3))
boxplot(log(Amount.Requested)~target,xlab = "target",main =" target Vs Amount.Requested" ,col=c("Navy","red"),cex=0.6)
boxplot(log(MonthlyIncome)~target,xlab = "target",main =" target VS MonthlyIncome " ,col=c("Navy","red"),cex=0.6)
#boxplot(OpenCREDITLines,xlab = "target",main =" target Vs OpenCREDITLines" ,col=c("Navy","red"),cex=0.9)
boxplot(TotalCREDITLines~target,xlab = "target",main =" target Vs TotalCREDITLines" ,col=c("Navy","red"),cex=0.6)
boxplot(DTI~target,xlab = "target",main ="boxplot of target Vs DTI" ,col=c("Navy","red"),cex=0.6)
boxplot(InterestRate~target,xlab = "target",main ="boxplot of target Vs InterestRate" ,col=c("Navy","red"),cex=0.6)

boxplot(RevolvingCREDITBalance~target,xlab = "target",main =" target Vs RevolvingCREDITBal" ,col=c("Navy","red"),cex=0.6)
boxplot(RevolvingLineUtilization~target,xlab = "target",main =" target Vs RevolvingLineUtilization" ,col=c("Navy","red"),cex=0.6)

boxplot(year~target,xlab = "target",main =" numer of years of credit VS target" ,col=c("Navy","red"),cex=0.6)

boxplot(Inquiries6M~target,xlab = "target",main ="  Inquiries6M Vs target" ,col=c("Navy","red"),cex=0.6)




## histogram



attach(dat)
par(mfrow=c(1,2))
hist(DTI,xlab = "DTI",prob=FALSE,main ="DTI",breaks = 15,col="red" )
hist(log(Amount.Requested),xlab = "Amount.Requested in logs",main ="Amount.Requested" ,breaks = 15 ,col="Navy")
par(mfrow=c(1,3))
hist(log(MonthlyIncome),xlab = "MonthlyIncome in logs",main =" MonthlyIncome plot" ,breaks = 15 ,col="blue")

hist(OpenCREDITLines,xlab = "OpenCREDITLinese",main ="Histogram of OpenCREDITLines" ,breaks = 15 ,col="green")
hist(TotalCREDITLines,xlab = "TotalCREDITLines",main ="Histogram of TotalCREDITLines" ,breaks = 15 ,col="green")
hist(log(RevolvingCREDITBalance),xlab = "RevolvingCREDITBalance in logs",main =" RevolvingCREDITBalance plot" ,breaks = 15 ,col="green")
hist(RevolvingLineUtilization,xlab = "RevolvingLineUtilization",main =" RevolvingLineUtilization plot" ,breaks = 15 ,col="green")
hist(year,xlab = "year",main =" number of years of credit " ,breaks = 15 ,col="green")
#hist(Closed,xlab = "Closed",main ="Histogram of Closed" ,breaks = 15 ,col="red")

hist(InterestRate,xlab = "InterestRate",main ="Histogram of InterestRate" ,breaks = 15 ,col="green")
hist(target,breaks = 15, col=c("blue", "red"))
legend("topright", c("Not default", "Default"), fill=c("red", "blue"), 
       border= c("black","black"), cex=0.6)
#########################################################################






## caegorical


attach(pdat1)
#summary(train$Inquiries6M)
targ.LoanPurpose <- aggregate(target, by = list(LoanPurpose), FUN = mean)
freq.LoanPurpose <- aggregate(target, by = list(LoanPurpose), FUN = length)
par(mfrow = c(2,2))
plot(freq.State, xlab = 'LoanPurpose', ylab = 'Frequency', main = 'LoanPurpose Plot' )
plot(targ.State, xlab = 'LoanPurpose', ylab = 'target', main = 'LoanPurpose Vs target')
#LendingClub$target[LendingClub$appl_fico_band=='820-824']
#cbind(state = targ.State$Group.1, freq = freq.State$x, targ = targ.State$x)
par(mfrow = c(1,2))
targ.HomeOwnership <- aggregate(target, by = list(HomeOwnership), FUN = mean)
freq.HomeOwnership <- aggregate(target, by = list(HomeOwnership), FUN = length)
#par(mfrow = c(2,2))
plot(freq.HomeOwnership, xlab = 'HomeOwnership', ylab = 'Frequency', main = 'HomeOwnership Plot' )
plot(targ.HomeOwnership, xlab = 'HomeOwnership', ylab = 'target', main = 'HomeOwnership Vs target')





#attach(pdat1)
#summary(train$Inquiries6M)
#pdat1$FICO.ranks=pdat1$appl_fico_band
attach(pdat1)
#summary(train$Inquiries6M)
targ.EmploymentLength <- aggregate(target, by = list(EmploymentLength), FUN = mean)
freq.EmploymentLength <- aggregate(target, by = list(EmploymentLength), FUN = length)
par(mfrow = c(2,2))
plot(freq.EmploymentLength, xlab = 'EmploymentLength', ylab = 'Frequency', main = 'EmploymentLength Plot' )
plot(targ.EmploymentLength, xlab = 'EmploymentLength', ylab = 'target', main = 'EmploymentLength Vs target')
#LendingClub$target[LendingClub$appl_fico_band=='820-824']
#cbind(state = targ.State$Group.1, freq = freq.State$x, targ = targ.State$x)
par(mfrow = c(1,2))
targ.appl_fico_band <- aggregate(target, by = list(appl_fico_band), FUN = mean)
freq.appl_fico_band <- aggregate(target, by = list(appl_fico_band), FUN = length)
#par(mfrow = c(2,2))
plot(freq.appl_fico_band, xlab = 'FICO rankings', ylab = 'Frequency', main = 'FICO rankings Plot' )
plot(targ.appl_fico_band, xlab = 'FICO rankings', ylab = 'target', main = 'FICO rankings Vs target')
#cbind(state = targ.State$Group.1, freq = freq.State$x, targ = targ.State$x)

###################################################################################################



attach(train)
cor(InterestRate,RevolvingLineUtilization,Inquiries6M,MonthlyIncome,target)
cor(train[,c(1,4,8,9,16)])
knitr::kable(cor(train[,c(1,4,8,9,16)])) ## none of these contionous prdictors are highly correlated

library(corrplot)
corrplot(cor(train[,c(3,7,8,14)]), method = 'ellipse', type = "lower") ## plot that show that none of the chosen predictors are highly correlated with themselver as well as target. 
 
####################################################################################

## bin regressors for train set


#library(Rprofet)
Inquiries6M_custom=WOE_custom(train, 'Inquiries6M', target='target', breaks=c(-Inf,0,3,5,Inf), right_bracket = F, color = "#0066CC")## create variable for FICO## breaks base on FICO ranges from 0 to 810
head(Inquiries6M_custom)
#summary(train$Inquiries6M)
 
library(Rprofet)
RevolvingLineUtilization_custom=WOE_custom(train, 'RevolvingLineUtilization', target='target', breaks=c(-Inf,0,30,60,80,Inf), right_bracket = F, color = "#0066CC")## create variable for FICO## breaks base on FICO ranges from 0 to 810
head(RevolvingLineUtilization_custom)
summary((train$RevolvingLineUtilization)) 


train.bin=cbind(train,RevolvingLineUtilization_custom,Inquiries6M_custom) ## new train data


library(Rprofet)
Inquiries6M_custom=WOE_custom(test, 'Inquiries6M', target='target', breaks=c(-Inf,0,3,5,Inf), right_bracket = F, color = "#0066CC")## create variable for FICO## breaks base on FICO ranges from 0 to #810)
head(Inquiries6M_custom)
#summary(train$InterestRate)
 
library(Rprofet)
RevolvingLineUtilization_custom=WOE_custom(test, 'RevolvingLineUtilization', target='target', breaks=c(-Inf,0,30,60,80,Inf), right_bracket = F, color = "#0066CC")## create variable for FICO## breaks base on FICO ranges from 0 to 810
head(RevolvingLineUtilization_custom)
#summary((train$RevolvingLineUtilization)) 


test.bin=cbind(test,RevolvingLineUtilization_custom,Inquiries6M_custom) ## new test data

##############################################################################




## Results for two regression

## logit model


logit.bins.fit=glm(target~log(MonthlyIncome)+Inquiries6M_custom+RevolvingLineUtilization_custom+LoanPurpose, family = "binomial",data=train.bin)
summary(logit.bins.fit)
logit.bins.pred=predict(logit.bins.fit,newdata = test.bin, type = "response")

length(logit.bins.pred)
## gains table


logit.prednew=cbind(test.bin$target,logit.bins.pred)
#write.csv(logit.prednew,file="logit.prednewHW5.csv")
library(gains)
bins.logitgains=gains(test.bin$target,logit.bins.pred)

SSE.log=sum((test.bin$target -logit.bins.pred)^2)
v1=sqrt (SSE.log/10625  ) # the logit is better than the tree  
v1
plot(bins.logitgains)
############################################################################





## MARS MODEL

library(earth)
myearth=earth(target ~., data=train, glm=list(family=binomial),degree=1)
summary(myearth)
#summary(train)
logit.mars=predict(myearth,newdata = test, type = "response")## test pred

library(vip)
vip(myearth)### choose variables in order importance 


#tree.pred1=(predict(tree,newdata=train))
MARS.prednew=cbind(test$target,logit.mars)
#write.csv(MARS.prednew,file="MARS.prednew.csv")
#data=cbind(logit.mars,)
library(gains)
logitgmars=gains(test$target,logit.mars)

#knitr::kable(myearth$coefficients, caption = "Table 1: Summary of the MARS Model")

SSE.lm=sum((test$target -logit.mars)^2)
v=sqrt (SSE.lm/10625  ) # the logit is better than the tree  
v

plot(logitgmars)

#########################################################################


### plots , lift 



par(mfrow=c(1,2))
library(pROC)
test_roc1 <- roc(test.bin$target ~ logit.bins.pred, plot = TRUE, print.auc = TRUE,
                main="logit model ROC ",col="Navy",
                xlab="False postive rate",ylab="TPR (sensitivity)")

library(pROC)
test_roc2 <- roc(test$target ~ logit.mars, plot = TRUE, print.auc = TRUE,
                main="MARS model AUC ",col="Navy",
                xlab="False positive rate",ylab="TPR (sensitivity)")


## Gains plot
par(mfrow=c(1,2))
plot(c(0,logitgmars$depth),c(0,logitgmars$cume.pct.of.total*100),col="red",type="l",xlab="percent of customers",ylab="percent response",main="commulative Gains chart plot")
points(c(0,bins.logitgains$depth),c(0,bins.logitgains$cume.pct.of.total*100),col="blue",type="o")
abline(a=0, b=1, lty=2,col="black")
legend("topleft", legend = c("MARS", "logit", "Baseline"),
       col = c("red", "blue", "black"), lty=1:1, cex = 0.8)


#lift plot

plot(c(0,logitgmars$depth),c(0,logitgmars$cume.lift/100),col="red",type="l",xlab="percent of customers",ylab=" Lift",main=" lift chart plot",xlim=c(10,100),ylim=c(0,4))
points(c(0,bins.logitgains$depth),c(0,bins.logitgains$cume.lift/100),col="blue",type="o",xlim=c(10,100),ylim=c(0,4))
points(seq(10,100,by=10),rep(1,10),col="black",type="l",lty=2)
#abline(a=0, b=1, lty=2,col="black")
legend("topleft", legend = c("MARS", "logit", "Baseline"),
       col = c("red", "blue", "black"), lty=1:1, cex = 0.8)

seq(1, 9, by = 2)
###################################################################################






library(ROCR)

testpreds.mars <- prediction(logit.mars, test$target ,label.ordering = NULL)## mars
trainpreds.logit <- prediction(logit.bins.pred, test.bin$target, label.ordering = NULL)## logit
#slotNames(mypreds)

roc.testperf <- performance(testpreds.mars, measure = "tpr", x.measure="fpr")## MARAS
roc.trainperf <- performance(trainpreds.logit, measure = "tpr", x.measure="fpr")## LOGIT


plot(roc.trainperf, main = "ROC for Logistic vs MARS -validation dataset", col="red") #plotting logidticROC curve
points(roc.testperf@y.values[[1]]~roc.testperf@x.values[[1]], type="l", col="blue") #plotting tree ROC curve
abline(a=0, b=1, lty=2)
legend("topleft", legend = c("logit AUC=0.623", "mars AUC=0.632", "Baseline"),
       col = c("red", "blue", "black"), lty=1:1, cex = 0.8,pch=16,bty="n")
auc.train.perf <- performance(testpreds.mars, measure = "auc")
auc.test.perf <- performance(trainpreds.logit, measure = "auc")
#AUC
auc.train.perf@y.values[[1]]## mars
auc.test.perf@y.values[[1]]##logit

roc.trainperf@alpha.values[[1]]
############################################################################




## Ks-plot

par(mfrow=c(1,2))
#auc.train.perf <- performance(testpreds.mars, measure = "auc")
#auc.test.perf <- performance(trainpreds.logit, measure = "auc")
#AUC
#auc.train.perf@y.values[[1]]## mars
#auc.test.perf@y.values[[1]]##logit
#par(mfrow=c(1,2))

MARSks=length(roc.testperf@alpha.values[[1]])## MARS

MARSkspct=c(1:MARSks)/MARSks

plot(MARSkspct,roc.testperf@x.values[[1]], lty=2,xlab="percentle", ylab="True postive rate/False postive rate",main="KS plot of MARS VS logit model")
lines(MARSkspct,roc.testperf@x.values[[1]],col="green")
roc.trainperf

logitks=length(roc.trainperf@alpha.values[[1]])## logit

logitpct=c(1:logitks)/logitks


lines(MARSkspct,roc.testperf@y.values[[1]],col="blue")## Mars model
lines(logitpct,roc.trainperf@y.values[[1]],col="red")## logit model
legend("topleft", legend = c("logit", "mars", "Baseline"),
       col = c("red", "blue", "black"), lty=1:1, cex = 0.8,pch=16,bty="n")


train_group <- c(rep("sample1", length(logit.bins.pred)), rep("sample2", length(logit.mars)))
train_dat <- data.frame(KSD = c(logit.bins.pred,logit.mars), group = train_group)
# create ECDF of data
train_cdf1 <- ecdf(logit.bins.pred) 
train_cdf2 <- ecdf(logit.mars) 
# find min and max statistics to draw line between points of greatest distance
train_minMax <- seq(min(logit.bins.pred, logit.mars), max(logit.bins.pred, logit.mars), length.out=length(logit.bins.pred)) 
train_x0 <- train_minMax[which( abs(train_cdf1(train_minMax) - train_cdf2(train_minMax)) == max(abs(train_cdf1(train_minMax) - train_cdf2(train_minMax))) )] 
train_y0 <- train_cdf1(train_x0) 
train_y1 <- train_cdf2(train_x0) 



# Plotting KS
plot(train_cdf1, verticals=TRUE, do.points=FALSE, col="red", 
     main="KS curve of Logit VS MARS model",xlab="percentle", ylab="True postive rate/False postive rate") 
plot(train_cdf2, verticals=TRUE, do.points=FALSE, col="blue", add=TRUE) 
## alternatine, use standard R plot of ecdf 
#plot(f.a, col="blue") 
#lines(f.b, col="green") 

points(c(train_x0, train_x0), c(train_y0, train_y1), pch=16, col="red") 
segments(train_x0, train_y0, train_x0, train_y1, col="red", lty="dotted") 

#abline(a=0, b=1, lty=2)
legend("topleft", legend = c("logit", "mars"),
       col = c("red", "blue"), lty=1:1, cex = 0.8,pch=16,bty="n")
############################################################################














## KS-plot
par(mfrow=c(1,2))
MARSks=length(roc.testperf@alpha.values[[1]])## MARS

MARSkspct=c(1:MARSks)/MARSks

plot(MARSkspct,roc.testperf@x.values[[1]], lty=2,xlab="percentle", ylab="True postive rate/False postive rate",main="KS plot of MARS model")
lines(MARSkspct,roc.testperf@x.values[[1]],col="green")
#roc.trainperf



lines(MARSkspct,roc.testperf@y.values[[1]],col="blue")## Mars model
#lines(logitpct,roc.trainperf@y.values[[1]],col="red")## logit model
#legend("topleft", legend = c("logit", "mars", "Baseline"),
       #col = c("red", "blue", "black"), lty=1:1, cex = 0.8,pch=16,bty="n")


## logit
logitks=length(roc.trainperf@alpha.values[[1]])## logit

logitpct=c(1:logitks)/logitks


plot(logitpct,roc.trainperf@x.values[[1]],col="blue", lty=1,xlab="percentle", ylab="True postive rate/False postive rate",main="KS plot of logit Model")
#lines(MARSkspct,roc.testperf@x.values[[1]],col="green")

lines(logitpct,roc.trainperf@y.values[[1]],col="red")    
########################################################################################
## new code for KS



#----KS charts----

par(mfrow=c(1,2))
test <- as.data.frame(cbind(roc.trainperf@x.values[[1]], roc.trainperf@y.values[[1]]))
Percentile <- NULL
Difference <- NULL 
for (i in 1:nrow(test)){
  test[i, 3] = i/nrow(test)
  test[i, 4]= abs(test[i,2]-test[i,1])
}
colnames(test) <- c("FPR", "TPR", "Percentile", "Difference")
plot(test$Percentile, test$TPR, type = "l", col = "blue", 
     main = "KS plot for Logit model", 
     ylab = "TPR/FPR", 
     xlab = "Percentile")
points(test$Percentile, test$FPR, type = "l", col = "red")
abline(0,1, lty =2)
abline(v= 0.19221, lty = 2,col="gray")
legend("bottomright", legend = c("FPR", "TPR", "logit KS= 0.17876", "Baseline"),
       col = c("red", "blue","gray", "black"), lty=1:1, cex = 0.8,pch=16,bty="n")

test <- as.data.frame(cbind(roc.testperf@x.values[[1]], roc.testperf@y.values[[1]]))
Percentile <- NULL
Difference <- NULL 
for (i in 1:nrow(test)){
  test[i, 3] = i/nrow(test)
  test[i, 4]= abs(test[i,2]-test[i,1])
}
colnames(test) <- c("FPR", "TPR", "Percentile", "Difference")
plot(test$Percentile, test$TPR, type = "l", col = "blue", 
     main = "KS plot for MARS model", 
     ylab = "TPR/FPR", 
     xlab = "Percentile")
points(test$Percentile, test$FPR, type = "l", col = "red")
abline(0,1, lty =2)
abline(v=0.19148, lty = 2,col="gray")
legend("bottomright", legend = c("FPR", "TPR", "mars KS=0.19148", "Baseline"),
       col = c("red", "blue","gray", "black"), lty=1:1, cex = 0.8,pch=16,bty="n")

ks.logit=round(max(roc.trainperf@y.values[[1]]-roc.trainperf@x.values[[1]]),5)## logit KS
ks.mars=round(max(roc.testperf@y.values[[1]]-roc.testperf@x.values[[1]]),5)## Mars ks
ks.logit
ks.mars
###################################################################
















######################################################################################
### Extra plots

## home ownership




library(ggthemes)
library(ggplot2)
library(GGally)
library(RColorBrewer)
#dat %>%
  #group_by(LoanPurpose) %>%
dat %>%
  group_by(HomeOwnership) %>%
  summarize(freq = n()) %>%
  ggplot(aes(reorder(HomeOwnership, freq), y = freq, fill = freq)) +   
  geom_bar(stat = "identity", position = "dodge", width=0.4) +
  xlab("HomeOwnership") +
  ylab("Frequency") +
  theme_fivethirtyeight() + 
  coord_flip() +
  theme(legend.position ='none', axis.text.x = element_text(size = 7)) + 
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(11,'Spectral'))) +
  ggtitle("HomeOwnership")




#packageVersion("ggthemes")
library(ggthemes)
library(ggplot2)
library(GGally)
library(RColorBrewer)
dat %>%
  group_by(HomeOwnership) %>%
  summarize(freq = n()) %>%
  ggplot(aes(reorder(HomeOwnership, freq), y = freq, fill = freq)) +   
  geom_bar(stat = "identity", position = "dodge") +
  xlab("HomeOwnership") +
  ylab("Frequency") +
  theme_fivethirtyeight() + 
  theme(legend.position ='none', axis.text.x = element_text(size = 15)) + 
  geom_text(aes(label = freq), vjust = -0.1, size = 4.5) +
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  ggtitle("Home Ownership plot")




library(ggthemes)
library(ggplot2)
library(GGally)
library(RColorBrewer)
#dat %>%
  #group_by(LoanPurpose) %>%
dat %>%
  group_by(LoanPurpose) %>%
  summarize(freq = n()) %>%
  ggplot(aes(reorder(LoanPurpose, freq), y = freq, fill = freq)) +   
  geom_bar(stat = "identity", position = "dodge", width=0.4) +
  xlab("LoanPurpose") +
  ylab("Frequency") +
  theme_fivethirtyeight() + 
  coord_flip() +
  theme(legend.position ='none', axis.text.x = element_text(size = 7)) + 
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(11,'Spectral'))) +
  ggtitle("LoanPurpose")




#dat$EmploymentLength
dat %>%
  group_by(EmploymentLength) %>%
  summarize(freq = n()) %>%
  top_n(50) %>%
  ggplot(aes(reorder(EmploymentLength, freq), y = freq, fill = freq)) +   
  geom_bar(stat = "identity", position = "dodge") +
  xlab("EmploymentLength") +
  ylab("Frequency") +
  coord_flip() +
  theme_fivethirtyeight() + 
  theme(legend.position ='none', axis.text.y = element_text(size = 12)) + 
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  ggtitle("EmploymentLength ")



#dat$EmploymentLength
#Inquiries6M
#dat$Education
pdat1$appl_fico_band
pdat1 %>%
  group_by(appl_fico_band) %>%
  summarize(freq = n()) %>%
  top_n(50) %>%
  ggplot(aes(reorder(appl_fico_band, freq), y = freq, fill = freq)) +   
  geom_bar(stat = "identity", position = "dodge") +
  xlab("appl_fico_band") +
  ylab("Frequency") +
  coord_flip() +
  theme_fivethirtyeight() + 
  theme(legend.position ='none', axis.text.y = element_text(size = 12)) + 
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  ggtitle("FICO Rankings")



dat %>%
  group_by(appl_fico_band) %>%
  summarize(freq = n()) %>%
  ggplot(aes(reorder(appl_fico_band, freq), y = freq, fill = freq)) +   
  geom_bar(stat = "identity", position = "dodge", width = 0.4) +
  xlab("appl_fico_band") +
  ylab("Frequency") +
  theme_fivethirtyeight() + 
  theme(legend.position ='none', axis.text.x = element_text(size = 15)) + 
  geom_text(aes(label = freq), vjust = -0.1, size = 4.5) +
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  ggtitle("appl_fico_band")


#dat$EmploymentLength
#dat$Education
dat$State
dat %>%
  group_by(State) %>%
  summarize(freq = n()) %>%
  top_n(50) %>%
  ggplot(aes(reorder(State, freq), y = freq, fill = freq)) +   
  geom_bar(stat = "identity", position = "dodge") +
  xlab("State") +
  ylab("Frequency") +
  coord_flip() +
  theme_fivethirtyeight() + 
  theme(legend.position ='none', axis.text.y = element_text(size = 12)) + 
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  ggtitle("State")




#dat$EmploymentLength
#Inquiries6M
#dat$Education
#dat$appl_fico_band
#AccountsDQ
dat %>%
  group_by(Inquiries6M) %>%
  summarize(freq = n()) %>%
  top_n(50) %>%
  ggplot(aes(reorder(Inquiries6M, freq), y = freq, fill = freq)) +   
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Inquiries6M") +
  ylab("Frequency") +
  coord_flip() +
  theme_fivethirtyeight() + 
  theme(legend.position ='none', axis.text.y = element_text(size = 12)) + 
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  ggtitle("Inquiries6M")

####################################################################################################






#Reference
#https://stackoverflow.com/questions/8161836/how-do-i-replace-na-values-with-zeros-in-an-r-dataframe
#https://stackoverflow.com/questions/11036989/replace-all-0-values-to-na
#https://stackoverflow.com/questions/36568070/extract-year-from-date/53340717
#https://rpubs.com/jgulum/5155
#https://www.rpubs.com/mn743893/475854
#https://rpubs.com/daniel_alaiev/290261
