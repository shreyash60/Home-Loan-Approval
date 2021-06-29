library(readxl)
data <- read_excel("C:/Users/Shreya Nayak/Downloads/DigiHack Dataset-og.xlsx")
View(data)

dim(data)
str(data)
summary(data)
install.packages("dplyr")
library(dplyr)
glimpse(data)
colSums(is.na(data))

x = boxplot(data$Age)
length(x$out)
table(data$SEX)
table(data$Target)
sum(duplicated(data))
length(unique(data$ID))
nrow(data)

# duplicate values are deleted in Excel
# changed data has been imported now

library(readxl)
hackathon <- read.csv("C:/Users/Shreya Nayak/Downloads/Digihack dataset-clean.csv")
View(hackathon)

data1 = hackathon 
dim(data1)
length(unique(data1$ID))

data1$Customer_type=as.factor(data1$Customer_type)
data1$SEX=as.factor(data1$SEX)
data1$Type_of_industry=as.factor(data1$Type_of_industry)
data1$Marital_Status=as.factor(data1$Marital_Status)
data1$Org_Type=as.factor(data1$Org_Type)

str(data1)

install.packages("ggstatsplot")
library(ggstatsplot)

boxplot(data1$Age)
hist(data1$Age)     # remove age less than 20
plot(data1$Age)

# couldnt predict
# gross Income , Month_in_city

boxplot(data1$)
hist(data1$Age)     # remove age less than 20
plot(data1$Age)

xyplot(data1$Gross_income ~ data1$Target)
xyplot(data1$Net_income ~ data1$Gross_income)

scatter.smooth(data1$Gross_income,data1$Net_income)

boxplot(data1$ENQ_3)
plot(data1$ENQ_3)
hist(data1$ENQ_3)
max(data1$ENQ_3)

data2 = data1%>%
  filter(Age >20)
data2$Gross_income = round(data2$Gross_income,2)
data2$Net_income = round(data2$Net_income,2)

boxplot(data2$Age)
View(data2)

hist(data2$Gross_income,breaks = 100)
a =subset(data2, data2$Gross_income <1000)
nrow(a)

write.csv(data2,file.choose())

str(data2)
glimpse(data2)
colSums(is.na(data2))

colSums(is.na(data3))
sum(is.na(data3))
View(data3)S


install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("grDevices")
library(grDevices)

pal = colorRamp(c("red","yellow"))
boxplot(data$Age,col = pal(0.2),xlab = "Customer's Age")

p = colorRamp(c("blue","yellow"))
scatter.smooth(data$Gross_income,data$Net_income,col = p(0.7),cex = 1,pch = 19,xlab = "Gross Income",ylab ="Net Income")

d = colorRamp(c("brown","pink"))
hist(data$Gross_income,col = d(0.6),breaks = 100,xlim = c(0,1.5e+06),xlab = "Gross_Income", main = "")

h = colorRamp(c("yellow","red"))
hist(data$Net_income,col = h(0.6),breaks = 100,xlim = c(0,1.5e+06),xlab = "Net_Income", main = "")

plot(tab, col = d(0.5),main = "Defaluter and Non Defaluter in different Customer_type")



library(Hmisc)
sum(colSums(is.na(data4)))
data4$Bank_balance=as.numeric(data4$Bank_balance)
impute(data4$Bank_balance,mean)
data4$bank_balance=impute(data4$Bank_balance,mean)
data4
data4=data4[,-14]
attach(data4)

# binary logistic regression

data4 = read.csv(file.choose(),header=T)
str(data4)
data4 = data4[,-c(1,2,3,8)]
View(data4)
str(data4)

data4$Customer_type=as.factor(data4$Customer_type)
data4$SEX=as.factor(data4$SEX)
data4$Type_of_industry=as.factor(data4$Type_of_industry)
data4$Marital_Status=as.factor(data4$Marital_Status)
data4$Org_Type=as.factor(data4$Org_Type)
data4$Target = as.factor(data4$Target)
str(data4)

#install.packages("caret")
library(caret)
set.seed(2222)
index=createDataPartition(data4$Target,p=0.75,list=F)
traindata=data4[index,]
testdata=data4[-index,]

lrmodel<-glm(Target ~.,family=binomial,data=data4)
summary(lrmodel) 

null<-glm(Target ~ 1, family=binomial,data=data4) 
anova(null,traindata, test="Chisq")


lr1 = glm(Target ~Months_in_city+ ENQ_2+ ENQ_3 +ENQ_4 +ENQ_6+ACCOUNT_21 ,family=binomial,data=traindata)
summary(lr1) 
null<-glm(Target ~ 1, family=binomial,data=traindata) 
anova(null,traindata, test="Chisq")

traindata$predprob<-round(fitted(lr1),2)
head(traindata)

#Logistic regression in R classification table
#install.packages("gmodels")
library(gmodels)
CrossTable(traindata$Target,fitted(lr1)>0.5)

table(traindata$Target,fitted(lr1)>0.5)


#ROC curve in R
#install.packages("ROCR")
library(ROCR) 
traindata$predprob<-fitted(lr1)
pred<-prediction(traindata$predprob,traindata$Target)
perf<-performance(pred,"tpr","fpr")
plot(perf)
abline(0,1)
auc<-performance(pred,"auc")
auc@y.values

data4$predprob<-fitted(lrmodel)
pred<-prediction(data4$predprob,data4$Target)
perf<-performance(pred,"tpr","fpr")
plot(perf)
abline(0,1)
auc<-performance(pred,"auc")
auc@y.values

View(testdata)
tab1 = table(test,testdata$Target)
traindata$predY<-ifelse(traindata$predprob>0.5,1,0)
traindata$predY = as.factor(traindata$predY)
traindata$Target = as.factor(traindata$Target)
confusionMatrix(traindata$predY,traindata$Target,positive="1")
traindata$predY<-ifelse(traindata$predprob>0.5,1,0)
testdata$predprob<-predict(lr1,testdata)
testdata$predY=ifelse(testdata$predprob>0.5,1,0)
testdata$predY = as.factor(testdata$predY)
testdata$Target = as.factor(testdata$Target)
confusionMatrix(testdata$predY,testdata$Target,positive = "1")
library(car)
vif(lr1)
plot(lr1,which=4,id.n=3)
install.packages("broom")
lr1.data=augment(lr1)%>%
  mutate(index=1:n())
plot(lr1$residuals)
leveneTest(lr1)
## vif no multicollinearity and no Pattern observed in the Residual Plot
