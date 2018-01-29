library(ggplot2)
library(ggcorrplot)
library(MASS)
library(corrgram)
library(dplyr)
library(PerformanceAnalytics)
library(corrplot)
library(caret)
library(plotly)
library(car)
library(gbm)
library(boot)
library(plyr)
library(xgboost)
library(Metrics)
#load challenge data to cdata
X201705_Arundo_take_home_challenge_training_set <- read.csv("C:/Users/maz/Downloads/201705_Arundo_take_home_challenge_training_set.csv")
cdata= X201705_Arundo_take_home_challenge_training_set
colnames(cdata)
View(cdata)
sapply(cdata, function(x) sum(is.na(x)))#no missing values
##convert date to weekday, to check if any seasonality effect is there
weekday=weekdays(as.Date(cdata$date,'%d-%m-%Y'))
cdata$weekday = as.factor(weekday)
cdata$events = as.factor(cdata$events)
cdata$calendar_code = as.factor(cdata$calendar_code)
cdata$weekday
#remove unwanted variables
cdata <- subset(cdata, select = -date )

#visulaise data 
correlations <- cor(cdata[,2:6])
corrplot(correlations, order = "hclust")
plot(density(cdata$request_count))

plot_ly(cdata, x = ~calendar_code, y = ~request_count,  
        type = "box")%>%  layout(boxmode = "group")

plot_ly(cdata, x = ~events, y = ~request_count,  
        type = "box")%>%  layout(boxmode = "group")

plot_ly(cdata, x = ~weekday, y = ~request_count,  
        type = "box")%>%  layout(boxmode = "group")


View(cdata)
#store target variable
target =  cdata$request_count
##remove target variable
cdata <- subset(cdata, select = -request_count )
View(cdata)
###correlations are plotted
correlations <- cor(cdata[,2:5])
highCorr <- findCorrelation(correlations, cutoff = .8)
length(highCorr)
highCorr
#higly corrleated variable is removed
filteredcdata <- cdata[, -highCorr]
dim(filteredcdata)
cdata1 =filteredcdata
cdata1 = cdata
View(cdata1)
##adding back target variable
cdata1$request_count = target
cdata$request_count = target
##sparse events are combined to form a single events
cdata1 = mutate(cdata1,eventsnew= events)
cdata1$eventsnew[cdata1$events %in% c("Fog", "Fog-Rain", "Fog-Rain-Snow", "Fog-Snow", "Rain-Snow")] = "Fog-Rain-Snow"
cdata1$eventsnew[cdata1$events == "Rain-Thunderstorm" ] = "Rain"

plot_ly(cdata1, x = ~eventsnew, y = ~request_count,  
        type = "box")%>%  layout(boxmode = "group")
cdata1 <- subset(cdata1, select = -events )
View(cdata1)
table(cdata1$eventsnew)

###modlling
reg1= lm(request_count~., data=cdata1)


##checking multicollinearity
vif(reg1)
##all vif values less then 4, hence no multicollinearity existing in the data
##check for non constant variance

ncvTest(reg2)
##error is homescadasitic as per ncv test
summary(reg1)

##regression diagnostics

opar= par()
par(mfrow=c(2,2))
plot(reg1) 
par(opar)

d=cooks.distance(reg2)
round(d,4)
cutoff= 4/nrow(cdata1)
length(d[d>cutoff])

###Check for interaction terms
res = step(reg1,~.^2) 
res$anova
##interactions term identifed and added the model
reg2= lm(request_count~.+ site_count:weekday+ min_temp:weekday , data=cdata1)
AIC(reg1,reg2)
anova(reg1,reg2)
summary(reg2) # interaction term signicicantly improved R square

opar= par()
par(mfrow=c(2,2))
plot(reg2) 
par(opar)

#Leverage plot shows influential values thus, weighted least square regression is doen
# significant pvalue thus no heteroskdasticity
### Influential observations
d=cooks.distance(reg2)
round(d,4)
cutoff= 4/nrow(cdata1)
length(d[d>cutoff])
##11 influential observations

##weighted Least square regression to balance the influential observations
w= ifelse(d<cutoff,1,cutoff/d) # weight initialised

reg3= lm(request_count~.+ site_count:weekday+ min_temp:weekday , data=cdata1,weights = w)
summary(reg3) # rsquare  improved

opar= par()
par(mfrow=c(2,2))
plot(reg3) 
par(opar)


####Regression structure 


boxCox(reg3, family="yjPower", plotit = TRUE) # lambda = 1 thus no y transformation requried
##zeros removed for boxtidwell transformation
tidwelldata = cdata1[cdata1$request_count != 0,]
boxTidwell(request_count~site_count,data = tidwelldata)  ##lambda = 1.71 hence  transformation of site count reqd
# power term added to regression 
reg4= lm(request_count~. + I(site_count^(1.7)) + site_count:weekday+ min_temp:weekday , data=cdata1,weights = w)
summary(reg4)

opar= par()
par(mfrow=c(2,2))
plot(reg4) 
par(opar)




### K- fold cross validation  done to check the best model among the build models
ctrl <- trainControl(method = "cv", number = 10)


reg1 <- train(request_count~., data=cdata1, method = "lm", trControl = ctrl)
reg2 <- train(request_count~.+ site_count:weekday+ min_temp:weekday, data=cdata1, method = "lm", trControl = ctrl)
reg3 <- train(request_count~.+ site_count:weekday+ min_temp:weekday, data=cdata1,weights = w, method = "lm", trControl = ctrl)
reg4 <- train(request_count~. + I(site_count^(1.7)) + site_count:weekday+ min_temp:weekday , data=cdata1,weights = w, method = "lm", trControl = ctrl)
reg1
reg2
reg3
reg4

## reg3 has least RMSE compared to all others. Reg4 is overfitting ie it has increase variance . reg1 reg2 has increase bias compared reg3

## make predictions
#load  test data
ctest=X201705_Arundo_take_home_challenge_test_set
##modify with necessary changes the  test data for prediction
ctest = mutate(ctest,eventsnew= events)
ctest$eventsnew[ctest$events %in% c("Fog", "Fog-Rain", "Fog-Rain-Snow", "Fog-Snow", "Rain-Snow")] = "Fog-Rain-Snow"
ctest$eventsnew[ctest$events == "Rain-Thunderstorm" ] = "Rain"
ctest= subset(ctest, select = -events)
weekday=weekdays(as.Date(ctest$date,'%d-%m-%Y'))
ctest$weekday = as.factor(weekday)
ctest$eventsnew = as.factor(ctest$eventsnew)
ctest$calendar_code = as.factor(ctest$calendar_code)


##prediction stored as below
predicted_request_counts=predict(reg3,ctest, interval= "prediction")
write.csv(predicted_request_counts,"predicted_request_counts.csv")

####check alternative models 
library(MASS)
library(randomForest)
require(randomForest)
library(rfUtilities)
fit <- randomForest(request_count~.,mtry=3, data=cdata,importance =TRUE)  #RMSE is more 3793 , hence randomforst rejected

rf.cv <- rf.crossValidation(fit, cdata, p=0.10, n=20, ntree=501)
print(fit) # view results 
importance(fit) # importance of each predictor

##Boosted tree
colnames(cdata1)
boost.mdl =gbm(request_count~.,data=cdata1, distribution="gaussian",n.trees =5000 , interaction.depth =4)
boost.mdl
summary(boost.mdl)
y.boost=predict(boost.mdl ,newdata =cdata1,n.trees =5000)
mean((y.boost -cdata$request_count)^2)  # error is more

##mean per day
by_day = group_by(cdata1,weekday)
mean_by_day = summarise(by_day, mean(request_count))
mean_by_day

# Create custom summary function in proper format for caret
custom_summary = function(data, lev = NULL, model = NULL){
  out = rmse(data[, "obs"], data[, "pred"])
  names(out) = c("rmse")
  out
}


# Create control object
control = trainControl(method = "cv",  # Use cross validation
                       number = 10,     # 10-folds
                       summaryFunction = custom_summary                      
)

# Create grid of tuning parameters
grid = expand.grid(nrounds=c(100,200,300), # Test 4 values for boosting rounds
                   max_depth= c(3,4),           # Test 2 values for tree depth
                   eta=c( 0.025,0.05,0.1),      # Test 3 values for learning rate
                   gamma= c(0.1), 
                   colsample_bytree = c(0.4,0.6), 
                   min_child_weight = c(1),
                   subsample= c(1))

grid = expand.grid(nrounds=c(300,500,600), # Test 4 values for boosting rounds
                   max_depth= c(3,4),           # Test 2 values for tree depth
                   eta=c( 0.025,0.05,0.1),      # Test 3 values for learning rate
                   gamma= c(0.1), 
                   colsample_bytree = c(0.8,1), 
                   min_child_weight = c(1),
                   subsample= c(1))
View(cdata1)
xgb_tree_model =  train(request_count~.,      # Predict SalePrice using all features
                        data=cdata1,
                        method="xgbTree",
                        trControl=control,
                        tuneGrid=grid, 
                        metric="rmse",     # Use custom performance metric
                        maximize = FALSE)   # Minimize the metric
xgb_tree_model$results
xgb_tree_model$bestTune
varImp(xgb_tree_model)
test_predictions = predict(xgb_tree_model, newdata=test)
test_predictions
submission = read.csv("sample_submission.csv")
submission$SalePrice = test_predictions
write.csv(submission, "home_prices_xgb_sub1.csv", row.names=FALSE)

###Splines




