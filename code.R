library(dplyr)
library(ggplot2)

data = read.csv("E:/Amex Assignment/train_bFQbE3f/train.csv", stringsAsFactors = F)

colnames(data)

lapply(data, class)

# columns to drop directly

drop_directly = c("id")

# looking at number of Na(s) in each variable
na_count = data.frame(100*sapply(data, function(x){sum(is.na(x))})/nrow(data))
colnames(na_count) = c("Percentage_of_NAs")

barplot(na_count$Percentage_of_NAs, names.arg =  row.names(na_count))

# categorical variables with NAs replaced with 0
data$personal_loan_active[is.na(data$personal_loan_active)] = 0
data$personal_loan_closed[is.na(data$personal_loan_closed)] = 0
data$vehicle_loan_active[is.na(data$vehicle_loan_active)] = 0
data$vehicle_loan_closed[is.na(data$vehicle_loan_closed)] = 0

# replacing nas with 0 in some variables based on count data
data$dc_count_apr[is.na(data$dc_count_apr)] = 0
data$dc_count_may[is.na(data$dc_count_may)] = 0
data$dc_count_jun[is.na(data$dc_count_jun)] = 0

data$dc_cons_apr[data$dc_count_apr == 0] = 0
data$dc_cons_may[data$dc_count_may == 0] = 0
data$dc_cons_jun[data$dc_count_jun == 0] = 0

# columns with high levels of missing data
drop_high_nas = c("investment_1", "investment_2", "investment_3", "investment_4")

# convert variables to categorical
data$personal_loan_active = as.factor(data$personal_loan_active)
data$personal_loan_closed = as.factor(data$personal_loan_closed)
data$vehicle_loan_active = as.factor(data$vehicle_loan_active)
data$vehicle_loan_closed = as.factor(data$vehicle_loan_closed)
data$account_type = as.factor(data$account_type)
data$gender = as.factor(data$gender)
data$loan_enq[data$loan_enq != "Y"] = "N"
data$loan_enq = as.factor(data$loan_enq)

# Dropping columns
drop_cols = c(drop_directly, drop_high_nas)
col_index = which(colnames(data) %in% drop_cols)

data2 = data[,-col_index]

# looking at number of Na(s) in each variable
na_count2 = data.frame(100*sapply(data2, function(x){sum(is.na(x))})/nrow(data2))
colnames(na_count2) = c("Percentage_of_NAs")

barplot(na_count2$Percentage_of_NAs, names.arg =  row.names(na_count2))

# Imputation with mean values
numeric_cols = which((sapply(data2, class) == "numeric" | sapply(data2, class) == "integer") & na_count2$Percentage_of_NAs > 0)

data3 = data2
for (i in numeric_cols)
{
  data3[is.na(data2[,i]),i] = mean(data2[,i], na.rm = T)
}

# looking at number of Na(s) in each variable
na_count3 = data.frame(100*sapply(data3, function(x){sum(is.na(x))})/nrow(data3))
colnames(na_count3) = c("Percentage_of_NAs")

barplot(na_count3$Percentage_of_NAs, names.arg =  row.names(na_count3))

# Make count variables integer to avoid confusion
count_cols = grep("_count", colnames(data3))

for (i in count_cols)
{
  data3[,i] = as.integer(data3[,i])
}

############Adding new variables##########
## 1

# Total dc and cc cons for 3 months
data3$cc_cons_sum = data3$cc_cons_apr + data3$cc_cons_may + data3$cc_cons_jun
data3$dc_cons_sum = data3$dc_cons_apr + data3$dc_cons_may + data3$dc_cons_jun

# Total dc and cc count for 3 months
data3$cc_count_sum = data3$cc_count_apr + data3$cc_count_may + data3$cc_count_jun
data3$dc_count_sum = data3$dc_count_apr + data3$dc_count_may + data3$dc_count_jun

# Total dc and cc cons average for 3 months
data3$cc_cons_avg = 0
data3$cc_cons_avg[data3$cc_count_sum != 0] = data3$cc_cons_sum[data3$cc_count_sum != 0]/data3$cc_count_sum[data3$cc_count_sum != 0]

data3$dc_cons_avg = 0
data3$dc_cons_avg[data3$dc_count_sum != 0] = data3$dc_cons_sum[data3$dc_count_sum != 0]/data3$dc_count_sum[data3$dc_count_sum != 0]

# Total dc cons average for each month
data3$dc_cons_apr_avg = 0
data3$dc_cons_apr_avg[data3$dc_count_apr != 0] = data3$dc_cons_apr[data3$dc_count_apr != 0]/data3$dc_count_apr[data3$dc_count_apr != 0]

data3$dc_cons_may_avg = 0
data3$dc_cons_may_avg[data3$dc_count_may != 0] = data3$dc_cons_may[data3$dc_count_may != 0]/data3$dc_count_may[data3$dc_count_may != 0]

data3$dc_cons_jun_avg = 0
data3$dc_cons_jun_avg[data3$dc_count_jun != 0] = data3$dc_cons_jun[data3$dc_count_jun != 0]/data3$dc_count_jun[data3$dc_count_jun != 0]

# Total cc cons average for each month
data3$cc_cons_apr_avg = 0
data3$cc_cons_apr_avg[data3$cc_count_apr != 0] = data3$cc_cons_apr[data3$cc_count_apr != 0]/data3$cc_count_apr[data3$cc_count_apr != 0]

data3$cc_cons_may_avg = 0
data3$cc_cons_may_avg[data3$cc_count_may != 0] = data3$cc_cons_may[data3$cc_count_may != 0]/data3$cc_count_may[data3$cc_count_may != 0]

data3$cc_cons_jun_avg = 0
data3$cc_cons_jun_avg[data3$cc_count_jun != 0] = data3$cc_cons_jun[data3$cc_count_jun != 0]/data3$cc_count_jun[data3$cc_count_jun != 0]

## 2

# Total dc and cc cons for 3 months
data3$credit_amount_sum = data3$credit_amount_apr + data3$credit_amount_may + data3$credit_amount_jun
data3$debit_amount_sum = data3$debit_amount_apr + data3$debit_amount_may + data3$debit_amount_jun

# Total dc and cc count for 3 months
data3$credit_count_sum = data3$credit_count_apr + data3$credit_count_may + data3$credit_count_jun
data3$debit_count_sum = data3$debit_count_apr + data3$debit_count_may + data3$debit_count_jun

# Total dc and cc cons average for 3 months
data3$credit_amount_avg = 0
data3$credit_amount_avg[data3$credit_count_sum != 0] = data3$credit_amount_sum[data3$credit_count_sum != 0]/data3$credit_count_sum[data3$credit_count_sum != 0]

data3$debit_amount_avg = 0
data3$debit_amount_avg[data3$debit_count_sum != 0] = data3$debit_amount_sum[data3$debit_count_sum != 0]/data3$debit_count_sum[data3$debit_count_sum != 0]

# Total dc cons average for each month
data3$debit_amount_apr_avg = 0
data3$debit_amount_apr_avg[data3$debit_count_apr != 0] = data3$debit_amount_apr[data3$debit_count_apr != 0]/data3$debit_count_apr[data3$debit_count_apr != 0]

data3$debit_amount_may_avg = 0
data3$debit_amount_may_avg[data3$debit_count_may != 0] = data3$debit_amount_may[data3$debit_count_may != 0]/data3$debit_count_may[data3$debit_count_may != 0]

data3$debit_amount_jun_avg = 0
data3$debit_amount_jun_avg[data3$debit_count_jun != 0] = data3$debit_amount_jun[data3$debit_count_jun != 0]/data3$debit_count_jun[data3$debit_count_jun != 0]

# Total cc cons average for each month
data3$credit_amount_apr_avg = 0
data3$credit_amount_apr_avg[data3$credit_count_apr != 0] = data3$credit_amount_apr[data3$credit_count_apr != 0]/data3$credit_count_apr[data3$credit_count_apr != 0]

data3$credit_amount_may_avg = 0
data3$credit_amount_may_avg[data3$credit_count_may != 0] = data3$credit_amount_may[data3$credit_count_may != 0]/data3$credit_count_may[data3$credit_count_may != 0]

data3$credit_amount_jun_avg = 0
data3$credit_amount_jun_avg[data3$credit_count_jun != 0] = data3$credit_amount_jun[data3$credit_count_jun != 0]/data3$credit_count_jun[data3$credit_count_jun != 0]

## 3

data3$perc_credit_amount_apr = 0
data3$perc_credit_amount_apr[data3$max_credit_amount_apr!= 0] = data3$credit_amount_apr[data3$max_credit_amount_apr!= 0]/data3$max_credit_amount_apr[data3$max_credit_amount_apr!= 0]

data3$perc_credit_amount_may = 0
data3$perc_credit_amount_may[data3$max_credit_amount_may!= 0] = data3$credit_amount_may[data3$max_credit_amount_may!= 0]/data3$max_credit_amount_may[data3$max_credit_amount_may!= 0]

data3$perc_credit_amount_jun = 0
data3$perc_credit_amount_jun[data3$max_credit_amount_jun!= 0] = data3$credit_amount_jun[data3$max_credit_amount_jun!= 0]/data3$max_credit_amount_jun[data3$max_credit_amount_jun!= 0]

## 4

data3$cc_cons_apr_prop = 0
data3$cc_cons_apr_prop[data3$cc_cons_sum != 0] = data3$cc_cons_apr[data3$cc_cons_sum != 0]/data3$cc_cons_sum[data3$cc_cons_sum != 0]

data3$cc_cons_may_prop = 0
data3$cc_cons_may_prop[data3$cc_cons_sum != 0] = data3$cc_cons_may[data3$cc_cons_sum != 0]/data3$cc_cons_sum[data3$cc_cons_sum != 0]

data3$cc_cons_jun_prop = 0
data3$cc_cons_jun_prop[data3$cc_cons_sum != 0] = data3$cc_cons_jun[data3$cc_cons_sum != 0]/data3$cc_cons_sum[data3$cc_cons_sum != 0]

data3$dc_cons_apr_prop = 0
data3$dc_cons_apr_prop[data3$dc_cons_sum != 0] = data3$dc_cons_apr[data3$dc_cons_sum != 0]/data3$dc_cons_sum[data3$dc_cons_sum != 0]

data3$dc_cons_may_prop = 0
data3$dc_cons_may_prop[data3$dc_cons_sum != 0] = data3$dc_cons_may[data3$dc_cons_sum != 0]/data3$dc_cons_sum[data3$dc_cons_sum != 0]

data3$dc_cons_jun_prop = 0
data3$dc_cons_jun_prop[data3$dc_cons_sum != 0] = data3$dc_cons_jun[data3$dc_cons_sum != 0]/data3$dc_cons_sum[data3$dc_cons_sum != 0]

data3$credit_amount_apr_prop = 0
data3$credit_amount_apr_prop[data3$credit_amount_sum != 0] = data3$credit_amount_apr[data3$credit_amount_sum != 0]/data3$credit_amount_sum[data3$credit_amount_sum != 0]

data3$credit_amount_may_prop = 0
data3$credit_amount_may_prop[data3$credit_amount_sum != 0] = data3$credit_amount_may[data3$credit_amount_sum != 0]/data3$credit_amount_sum[data3$credit_amount_sum != 0]

data3$credit_amount_jun_prop = 0
data3$credit_amount_jun_prop[data3$credit_amount_sum != 0] = data3$credit_amount_jun[data3$credit_amount_sum != 0]/data3$credit_amount_sum[data3$credit_amount_sum != 0]

data3$debit_amount_apr_prop = 0
data3$debit_amount_apr_prop[data3$debit_amount_sum != 0] = data3$debit_amount_apr[data3$debit_amount_sum != 0]/data3$debit_amount_sum[data3$debit_amount_sum != 0]

data3$debit_amount_may_prop = 0
data3$debit_amount_may_prop[data3$debit_amount_sum != 0] = data3$debit_amount_may[data3$debit_amount_sum != 0]/data3$debit_amount_sum[data3$debit_amount_sum != 0]

data3$debit_amount_jun_prop = 0
data3$debit_amount_jun_prop[data3$debit_amount_sum != 0] = data3$debit_amount_jun[data3$debit_amount_sum != 0]/data3$debit_amount_sum[data3$debit_amount_sum != 0]

## 5

data3$d_by_c_cons_ratio_apr = 0
data3$d_by_c_cons_ratio_apr[data3$cc_cons_apr != 0] = data3$dc_cons_apr[data3$cc_cons_apr != 0]/data3$cc_cons_apr[data3$cc_cons_apr != 0]

data3$d_by_c_cons_ratio_may = 0
data3$d_by_c_cons_ratio_may[data3$cc_cons_may != 0] = data3$dc_cons_may[data3$cc_cons_may != 0]/data3$cc_cons_may[data3$cc_cons_may != 0]

data3$d_by_c_amount_ratio_jun = 0
data3$d_by_c_amount_ratio_jun[data3$credit_amount_jun != 0] = data3$debit_amount_jun[data3$credit_amount_jun != 0]/data3$credit_amount_jun[data3$credit_amount_jun != 0]

data3$d_by_c_amount_ratio_apr = 0
data3$d_by_c_amount_ratio_apr[data3$credit_amount_apr != 0] = data3$debit_amount_apr[data3$credit_amount_apr != 0]/data3$credit_amount_apr[data3$credit_amount_apr != 0]

data3$d_by_c_amount_ratio_may = 0
data3$d_by_c_amount_ratio_may[data3$credit_amount_may != 0] = data3$debit_amount_may[data3$credit_amount_may != 0]/data3$credit_amount_may[data3$credit_amount_may != 0]

data3$d_by_c_amount_ratio_jun = 0
data3$d_by_c_amount_ratio_jun[data3$credit_amount_jun != 0] = data3$debit_amount_jun[data3$credit_amount_jun != 0]/data3$credit_amount_jun[data3$credit_amount_jun != 0]

############Adding new variables complete##########

# Modelling
library(caret)

set.seed(12345)

intrain = createDataPartition(data3$cc_cons, p = 0.8, list = F)

train = data3[intrain,]
test = data3[-intrain,]

### XGBoost

trainctrl <- trainControl(method = "cv", number = 4, verboseIter = TRUE, allowParallel = T)
tune_grid <- expand.grid(
  nrounds = seq(from = 300, to = 400, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = c(0.8,1),
  min_child_weight = 1,
  subsample = c(0.75,1)
)

model_xgb = train(log(cc_cons + 1) ~., data = train, method = "xgbTree", metric = "RMSE", trControl = trainctrl, verbose = T, tuneGrid = tune_grid)
model_xgb

pred = predict(model_xgb, test)

pred = exp(pred) - 1
library(MLmetrics)

100*RMSLE(pred, test$cc_cons)

# # model_xgb_basic = model_glm_tuned
# saveRDS(model_xgb_basic, "model_xgb_basic.rds")
# # model_xgb_final = model_xgb
# saveRDS(model_xgb_final, "model_xgb_final.rds")
# # model_xgb_ultra_final = model_xgb
# saveRDS(model_xgb_ultra_final, "model_xgb_ultra_final.rds")
# 
# saveRDS(model_rf, "model_rf2.rds")

### GLM

trainctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE, allowParallel = T)

model_glm = train(log(cc_cons + 1) ~., data = train, method = "glmnet", metric = "RMSE", trControl = trainctrl, family = "gaussian", tuneLength = 10)
model_glm

pred = predict(model_glm, test)
#pred[(pred<0)] = 0

pred = exp(pred) - 1
library(MLmetrics)

100*RMSLE(pred, test$cc_cons)

### Boosted GLM

trainctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE, allowParallel = T)

model_boost_glm = train(log(cc_cons + 1) ~., data = train, method = "glmboost", metric = "RMSE", trControl = trainctrl, tuneLength = 10)
model_boost_glm

pred = predict(model_boost_glm, test)

pred = ceiling(exp(pred) - 1)
library(MLmetrics)

100*RMSLE(pred, test$cc_cons)

### Random Forest

library(randomForest)

model_rf = randomForest(log(cc_cons + 1) ~., data = train, do.trace = T, ntree = 350)

pred = predict(model_rf, test)
pred = ceiling(exp(pred) - 1)

library(MLmetrics)

100*RMSLE(pred, test$cc_cons)

## SVM was taking a lot of time
# SVM model
# library(e1071)
# 
# trainctrl <- trainControl(method = "cv", number = 3, verboseIter = TRUE, allowParallel = T)
# 
# model_glm = train(log(cc_cons + 1) ~., data = train, method = "svmLinear", metric = "RMSE", trControl = trainctrl)
# model_glm
# 
# pred = predict(model_glm, test)
# #pred[(pred<0)] = 0
# 
# pred = exp(pred) - 1
# library(MLmetrics)
# 
# 100*RMSLE(pred, test$cc_cons)

######### Combining results from multiple models
pred_rf = predict(model_rf, test)
pred_xgb = predict(model_xgb, test)
pred_glmnet = predict(model_glm, test)

all_preds = data.frame(RF = pred_rf, XGB = pred_xgb, GLM = pred_glmnet, Target = test$cc_cons)

# Prediction over multiple model results
intrain2 = createDataPartition(all_preds$Target, p = 0.75, list = F)

train2 = all_preds[intrain2, ]
test2 = all_preds[-intrain2,]

tune_grid <- expand.grid(
  nrounds = seq(from = 300, to = 400, by = 50),
  eta = c(0.01, 0.025, 0.05),
  max_depth = c(2, 3, 4),
  gamma = 0,
  colsample_bytree = c(0.8),
  min_child_weight = 1,
  subsample = c(1)
)
# Stacking using XGBTree

model_stack = train(log(Target + 1) ~., data = train2, method = "xgbTree", trControl = trainctrl, metric = "RMSE", tuneGrid = tune_grid)

pred = predict(model_stack, test2)

pred = ceiling(exp(pred) - 1)

100*RMSLE(pred, test2$Target)

saveRDS(model_stack, "model_stack_xgb.rds")

# Stacking using GLM

model_stack2 = train(log(Target + 1) ~., data = train2, method = "glmnet", trControl = trainctrl, metric = "RMSE")

pred = predict(model_stack2, test2)

pred = ceiling(exp(pred) - 1)

100*RMSLE(pred, test2$Target)

saveRDS(model_stack2, "model_stack_glm.rds")


