train = read.csv("train.csv", stringsAsFactors = FALSE) 
test = read.csv("test.csv", stringsAsFactors = FALSE) 
cdata= IA.main1
IA.result =  filter(IA, is.na(rootznaws) )
#remove unwanted variables
cdata <- subset(cdata, select = -X )
cdata <- subset(cdata, select = -mukey )
cdata <- subset(cdata, select = -Unnamed..0 )
cdata <- subset(cdata, select = -OBJECTID_1 )

IA.result <- subset(IA.result, select = -X )
IA.result <- subset(IA.result, select = -mukey )
IA.result<- subset(IA.result, select = -Unnamed..0 )
IA.result <- subset(IA.result, select = -OBJECTID_1 )

IA.test <- subset(IA.test, select = -X )
IA.test <- subset(IA.test, select = -mukey )
IA.test<- subset(IA.test, select = -Unnamed..0 )
IA.test <- subset(IA.test, select = -OBJECTID_1 )
IA.test <- subset(IA.test, select = -rootznaws)
target2=IA.test$rootznaws
target2
# Remove the target variable not found in test set
View(cdata)
train= cdata
rootznaws= train$rootznaws
train$rootznaws = NULL
IA.result$rootznaws = NULL
dim(train)
dim(IA.result)
test = IA.result
# Combine data sets
full_data = rbind(train,test)

# Convert character columns to factor, filling NA values with "missing"
for (col in colnames(full_data)){
  if (typeof(full_data[,col]) == "character"){
    new_col = full_data[,col]
    new_col[is.na(new_col)] = "missing"
    full_data[col] = as.factor(new_col)
  }
}

# Separate out our train and test sets
train = full_data[1:nrow(train),]
train$rootznaws = rootznaws 
test = full_data[(nrow(train)+1):nrow(full_data),]

for (col in colnames(train)){
  if(is.numeric(train[,col])){
    if( abs(cor(train[,col],train$rootznaws)) > 0.5){
      print(col)
      print( cor(train[,col],train$rootznaws) )
    }
  }
}


for (col in colnames(train)){
  if(is.numeric(train[,col])){
    plot(density(train[,col]), main=col)
  }
}



library(caret)
library(plyr)
library(xgboost)
library(Metrics)
# Create custom summary function in proper format for caret
custom_summary = function(data, lev = NULL, model = NULL){
  out = rmse(data[, "obs"], data[, "pred"])
  names(out) = c("rmse")
  out
}
# Create control object
control = trainControl(method = "cv",  # Use cross validation
                       number = 10,     # 5-folds
                       summaryFunction = custom_summary                      
)

# Create grid of tuning parameters
grid = expand.grid(nrounds=c(500,600,700,800), # Test 4 values for boosting rounds
                   max_depth= c(8,9),           # Test 2 values for tree depth
                   eta=c( 0.025,.05),      # Test 3 values for learning rate
                   gamma= c(0.1), 
                   colsample_bytree = c(0.8), 
                   min_child_weight = c(1),
                   subsample= c(1))

xgb_tree_model =  train(rootznaws~.,      # Predict SalePrice using all features
                        data=train,
                        method="xgbTree",
                        trControl=control, 
                        nthreads = 4,
                        tuneGrid=grid, 
                        metric="rmse",     # Use custom performance metric
                        maximize = FALSE)   # Minimize the metric
train
xgb_tree_model$results
xgb_tree_model$bestTune

varImp(xgb_tree_model)
test_predictions = predict(xgb_tree_model, newdata=test)
View(train)

param = list(max.depth=9,eta=1,nthread = 4, silent=1, objective = "reg:linear", eval_metric = "rmse")
cv.res = xgb.cv(params = param,data = data.matrix(train), nfold = 10, label = train$rootznaws, nround = 800,early_stop_round = 3)
bst = xgboost(params = param, data =data.matrix(train), label = train$rootznaws,nround = 40)
ptrain = predict(bst, data.matrix(IA.test), outputmargin = TRUE)
(sum((ptrain-target2)^2/801))^0.5

test_predictions
submission = read.csv("sample_submission.csv")
submission$SalePrice = test_predictions
write.csv(submission, "home_prices_xgb_sub1.csv", row.names=FALSE)