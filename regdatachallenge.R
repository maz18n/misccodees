library(ggplot2)
library(ggcorrplot)
library(MASS)
library(corrgram)
library(dplyr)
library(PerformanceAnalytics)
library(corrplot)
library(caret)
#load challenge data to cdata
cdata= IA.main
colnames(cdata)
View(cdata)
#remove unwanted variables
cdata <- subset(cdata, select = -X )
cdata <- subset(cdata, select = -mukey )
cdata <- subset(cdata, select = -Unnamed..0 )
cdata <- subset(cdata, select = -OBJECTID_1 )
#visulaise data 
correlations <- cor(cdata)
corrplot(correlations, order = "hclust")
#store target variable
target =  cdata$rootznaws
##remove target variable
cdata <- subset( cdata, select = -rootznaws )
View(cdata)
###correlations are plotted
correlations <- cor(cdata)
corrplot(correlations, order = "hclust")
highCorr <- findCorrelation(correlations, cutoff = .75)
length(highCorr)
highCorr
filteredcdata <- cdata[, -highCorr]
dim(filteredcdata)
cdata1 =filteredcdata

##adding back target variable
cdata1$rootznaws = target
View(cdata1)
reg1= lm(rootznaws~., data=cdata1)
##checking multicollinearity
vif(reg1)
summary(reg1)
##all vif values less then 4, hence no multicollinearity existing in the data

# remove  variables which are not significant
reg1= lm(rootznaws~.-musumcpct -tk150_999a, data=cdata1)

##regression diagnostics

opar= par()
par(mfrow=c(2,2))
plot(reg1) 
par(opar)

###Check for interaction terms
res = step(reg1,~.^2) 
res$anova
reg2= lm(rootznaws~.-musumcpct -tk150_999a + rootznemc:droughty, data=cdata1)
reg2 = lm(medv~.+rm:lstat+rad:lstat,data=Boston)
AIC(reg1,reg2)
anova(reg1,reg2)
summary(reg2)

##check for non constant variance
library(car)
ncvTest(reg1)
# significant pvalue thus no heteroskdasticity
### Influential observations
d=cooks.distance(reg1)
round(d,4)
cutoff= 4/nrow(cdata1)
length(d[d>cutoff])

##weighted Least square regression to balance the influential observations
w= ifelse(d<cutoff,1,cutoff/d)
reg2= lm(rootznaws~.-musumcpct -pwsl1pomu -tk150_999a+ rootznemc:droughty, data=cdata1,weights = w)
reg3= rlm(rootznaws~.-musumcpct -pwsl1pomu -tk150_999a+ rootznemc:droughty, data=cdata1)
summary(reg2)
plot(density(w))
d1 = cooks.distance(lm2)
length((d1[d1>cutoff]))


opar= par()
par(mfrow=c(2,2))
plot(reg2) 
par(opar)

plot(reg2,which=4,cook.levels = cutoff) 


####Regression structure
boxCox(reg1, family="yjPower", plotit = TRUE)
boxTidwell(rootznaws~., data=cdata1)

#K- fold crossvalidation of linear regression model

library(boot)
glm.fit=glm(rootznaws~. ,data = cdata1)

cv.error.10= rep(0 ,10)

for (i in 1:10)
{
  glm.fit=glm(rootznaws~.,data = cdata1)
  cv.error.10[i]=cv.glm (cdata1,glm.fit ,K=10) $delta [1]
}
cv.error.10
mean(cv.error.10)

for (i in 1:10)
{
  glm.fit=glm(rootznaws~.,data = cdata)
  cv.error.10[i]=cv.glm (cdata,glm.fit ,K=10) $delta [1]
}
cv.error.10
mean(cv.error.10)

for (i in 1:50)
{
  glm.fit=glm(rootznaws~.-musumcpct -pwsl1pomu -tk150_999a+ rootznemc:droughty,data = cdata1)
  cv.error.10[i]=cv.glm(cdata,glm.fit ,K=10)$delta [1]
}
cv.error.10
mean(cv.error.10)
summary(glm.fit)
###best model has less mean cross validation error
plot(cdata1$rootznaws)
nrow(cdata1)

IA.test$result = predict (glm.fit,IA.test, interval ="prediction")

##random forest regression tree
library(MASS)
library(rpart)
set.seed (1)
train = sample(1:nrow(cdata), nrow(cdata)/2)
fit <- rpart(rootznaws~.,method="anova", data=cdata1)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  	

# plot tree 
plot(fit, uniform=TRUE, 
     main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postcript plot of tree 
post(fit, file = "c:/tree2.ps", 
     title = "Regression Tree for Mileage ")

library(randomForest)
require(randomForest)
library(rfUtilities)
fit <- randomForest(rootznaws~.,mtry=3, data=cdata1)
print(fit) # view results 
importance(fit) # importance of each predictor


rf.fit <- randomForest(rootznaws~.,mtry=3, data=cdata1)
cv.error.10[i]=cv.randomForest(cdata,glm.fit ,K=10)$delta[1]
rf.cv <- rf.crossValidation(rf.mdl, airquality[,1:10], p=0.10, n=99, ntree=501) 
rf.cv <- rf.crossValidation(rf.fit, cdata1[,1:10], p=0.10, n=50, ntree=50) 
rf.cv
par(mfrow=c(2,2))
plot(rf.cv)  
plot(rf.cv, stat = "mse")
plot(rf.cv, stat = "var.exp")
##Boosted tree
colnames(cdata1)
boost.mdl =gbm(rootznaws~.,data=cdata1, distribution="gaussian",n.trees =5000 , interaction.depth =4)
boost.mdl
summary(boost.mdl)
plot(boost.mdl, i = "droughty")


##new prediction
IA.test$rootznaws = predict(rf.fit,newdata =IA.test)
hist(IA.test$rootznaws)
hist(IA.test$result)
plot(density(IA.test$result))
plot(density(IA.test$rootznaws))
boxplot(IA.test$result)
boxplot(IA.test$rootznaws)


