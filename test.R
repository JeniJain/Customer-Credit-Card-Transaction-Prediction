# Prediction on test set
test_data = read.csv("E:/Amex Assignment/test_9K3DBWQ.csv", stringsAsFactors = F)

colnames(test_data)

lapply(test_data, class)

# columns to drop directly

drop_directly = NULL

# looking at number of Na(s) in each variable
na_count = data.frame(100*sapply(test_data, function(x){sum(is.na(x))})/nrow(test_data))
colnames(na_count) = c("Percentage_of_NAs")

barplot(na_count$Percentage_of_NAs, names.arg =  row.names(na_count))

# categorical variables with NAs replaced with 0
test_data$personal_loan_active[is.na(test_data$personal_loan_active)] = 0
test_data$personal_loan_closed[is.na(test_data$personal_loan_closed)] = 0
test_data$vehicle_loan_active[is.na(test_data$vehicle_loan_active)] = 0
test_data$vehicle_loan_closed[is.na(test_data$vehicle_loan_closed)] = 0

# replacing nas with 0 in some variables based on count data
test_data$dc_count_apr[is.na(test_data$dc_count_apr)] = 0
test_data$dc_count_may[is.na(test_data$dc_count_may)] = 0
test_data$dc_count_jun[is.na(test_data$dc_count_jun)] = 0

test_data$dc_cons_apr[test_data$dc_count_apr == 0] = 0
test_data$dc_cons_may[test_data$dc_count_may == 0] = 0
test_data$dc_cons_jun[test_data$dc_count_jun == 0] = 0

# columns with high levels of missing data
drop_high_nas = c("investment_1", "investment_2", "investment_3", "investment_4")

# convert variables to categorical
test_data$personal_loan_active = as.factor(test_data$personal_loan_active)
test_data$personal_loan_closed = as.factor(test_data$personal_loan_closed)
test_data$vehicle_loan_active = as.factor(test_data$vehicle_loan_active)
test_data$vehicle_loan_closed = as.factor(test_data$vehicle_loan_closed)
test_data$account_type = as.factor(test_data$account_type)
test_data$gender = as.factor(test_data$gender)
test_data$loan_enq[test_data$loan_enq != "Y"] = "N"
test_data$loan_enq = as.factor(test_data$loan_enq)

# Dropping columns
drop_cols = c(drop_directly, drop_high_nas)
col_index = which(colnames(test_data) %in% drop_cols)

test_data2 = test_data[,-col_index]

# looking at number of Na(s) in each variable
na_count2 = data.frame(100*sapply(test_data2, function(x){sum(is.na(x))})/nrow(test_data2))
colnames(na_count2) = c("Percentage_of_NAs")

barplot(na_count2$Percentage_of_NAs, names.arg =  row.names(na_count2))

# Imputation with mean values
numeric_cols = which((sapply(test_data2, class) == "numeric" | sapply(test_data2, class) == "integer") & na_count2$Percentage_of_NAs > 0)

for (i in numeric_cols)
{
  test_data2[is.na(test_data2[,i]),i] = mean(test_data2[,i], na.rm = T)
}

# looking at number of Na(s) in each variable
na_count3 = data.frame(100*sapply(test_data2, function(x){sum(is.na(x))})/nrow(test_data2))
colnames(na_count3) = c("Percentage_of_NAs")

barplot(na_count3$Percentage_of_NAs, names.arg =  row.names(na_count3))

# Make count variables integer to avoid confusion
count_cols = grep("_count", colnames(test_data2))

for (i in count_cols)
{
  test_data2[,i] = as.integer(test_data2[,i])
}

############Adding new variables##########
## 1

# Total dc and cc cons for 3 months
test_data2$cc_cons_sum = test_data2$cc_cons_apr + test_data2$cc_cons_may + test_data2$cc_cons_jun
test_data2$dc_cons_sum = test_data2$dc_cons_apr + test_data2$dc_cons_may + test_data2$dc_cons_jun

# Total dc and cc count for 3 months
test_data2$cc_count_sum = test_data2$cc_count_apr + test_data2$cc_count_may + test_data2$cc_count_jun
test_data2$dc_count_sum = test_data2$dc_count_apr + test_data2$dc_count_may + test_data2$dc_count_jun

# Total dc and cc cons average for 3 months
test_data2$cc_cons_avg = 0
test_data2$cc_cons_avg[test_data2$cc_count_sum != 0] = test_data2$cc_cons_sum[test_data2$cc_count_sum != 0]/test_data2$cc_count_sum[test_data2$cc_count_sum != 0]

test_data2$dc_cons_avg = 0
test_data2$dc_cons_avg[test_data2$dc_count_sum != 0] = test_data2$dc_cons_sum[test_data2$dc_count_sum != 0]/test_data2$dc_count_sum[test_data2$dc_count_sum != 0]

# Total dc cons average for each month
test_data2$dc_cons_apr_avg = 0
test_data2$dc_cons_apr_avg[test_data2$dc_count_apr != 0] = test_data2$dc_cons_apr[test_data2$dc_count_apr != 0]/test_data2$dc_count_apr[test_data2$dc_count_apr != 0]

test_data2$dc_cons_may_avg = 0
test_data2$dc_cons_may_avg[test_data2$dc_count_may != 0] = test_data2$dc_cons_may[test_data2$dc_count_may != 0]/test_data2$dc_count_may[test_data2$dc_count_may != 0]

test_data2$dc_cons_jun_avg = 0
test_data2$dc_cons_jun_avg[test_data2$dc_count_jun != 0] = test_data2$dc_cons_jun[test_data2$dc_count_jun != 0]/test_data2$dc_count_jun[test_data2$dc_count_jun != 0]

# Total cc cons average for each month
test_data2$cc_cons_apr_avg = 0
test_data2$cc_cons_apr_avg[test_data2$cc_count_apr != 0] = test_data2$cc_cons_apr[test_data2$cc_count_apr != 0]/test_data2$cc_count_apr[test_data2$cc_count_apr != 0]

test_data2$cc_cons_may_avg = 0
test_data2$cc_cons_may_avg[test_data2$cc_count_may != 0] = test_data2$cc_cons_may[test_data2$cc_count_may != 0]/test_data2$cc_count_may[test_data2$cc_count_may != 0]

test_data2$cc_cons_jun_avg = 0
test_data2$cc_cons_jun_avg[test_data2$cc_count_jun != 0] = test_data2$cc_cons_jun[test_data2$cc_count_jun != 0]/test_data2$cc_count_jun[test_data2$cc_count_jun != 0]

## 2

# Total dc and cc cons for 3 months
test_data2$credit_amount_sum = test_data2$credit_amount_apr + test_data2$credit_amount_may + test_data2$credit_amount_jun
test_data2$debit_amount_sum = test_data2$debit_amount_apr + test_data2$debit_amount_may + test_data2$debit_amount_jun

# Total dc and cc count for 3 months
test_data2$credit_count_sum = test_data2$credit_count_apr + test_data2$credit_count_may + test_data2$credit_count_jun
test_data2$debit_count_sum = test_data2$debit_count_apr + test_data2$debit_count_may + test_data2$debit_count_jun

# Total dc and cc cons average for 3 months
test_data2$credit_amount_avg = 0
test_data2$credit_amount_avg[test_data2$credit_count_sum != 0] = test_data2$credit_amount_sum[test_data2$credit_count_sum != 0]/test_data2$credit_count_sum[test_data2$credit_count_sum != 0]

test_data2$debit_amount_avg = 0
test_data2$debit_amount_avg[test_data2$debit_count_sum != 0] = test_data2$debit_amount_sum[test_data2$debit_count_sum != 0]/test_data2$debit_count_sum[test_data2$debit_count_sum != 0]

# Total dc cons average for each month
test_data2$debit_amount_apr_avg = 0
test_data2$debit_amount_apr_avg[test_data2$debit_count_apr != 0] = test_data2$debit_amount_apr[test_data2$debit_count_apr != 0]/test_data2$debit_count_apr[test_data2$debit_count_apr != 0]

test_data2$debit_amount_may_avg = 0
test_data2$debit_amount_may_avg[test_data2$debit_count_may != 0] = test_data2$debit_amount_may[test_data2$debit_count_may != 0]/test_data2$debit_count_may[test_data2$debit_count_may != 0]

test_data2$debit_amount_jun_avg = 0
test_data2$debit_amount_jun_avg[test_data2$debit_count_jun != 0] = test_data2$debit_amount_jun[test_data2$debit_count_jun != 0]/test_data2$debit_count_jun[test_data2$debit_count_jun != 0]

# Total cc cons average for each month
test_data2$credit_amount_apr_avg = 0
test_data2$credit_amount_apr_avg[test_data2$credit_count_apr != 0] = test_data2$credit_amount_apr[test_data2$credit_count_apr != 0]/test_data2$credit_count_apr[test_data2$credit_count_apr != 0]

test_data2$credit_amount_may_avg = 0
test_data2$credit_amount_may_avg[test_data2$credit_count_may != 0] = test_data2$credit_amount_may[test_data2$credit_count_may != 0]/test_data2$credit_count_may[test_data2$credit_count_may != 0]

test_data2$credit_amount_jun_avg = 0
test_data2$credit_amount_jun_avg[test_data2$credit_count_jun != 0] = test_data2$credit_amount_jun[test_data2$credit_count_jun != 0]/test_data2$credit_count_jun[test_data2$credit_count_jun != 0]

## 3

test_data2$perc_credit_amount_apr = 0
test_data2$perc_credit_amount_apr[test_data2$max_credit_amount_apr!= 0] = test_data2$credit_amount_apr[test_data2$max_credit_amount_apr!= 0]/test_data2$max_credit_amount_apr[test_data2$max_credit_amount_apr!= 0]

test_data2$perc_credit_amount_may = 0
test_data2$perc_credit_amount_may[test_data2$max_credit_amount_may!= 0] = test_data2$credit_amount_may[test_data2$max_credit_amount_may!= 0]/test_data2$max_credit_amount_may[test_data2$max_credit_amount_may!= 0]

test_data2$perc_credit_amount_jun = 0
test_data2$perc_credit_amount_jun[test_data2$max_credit_amount_jun!= 0] = test_data2$credit_amount_jun[test_data2$max_credit_amount_jun!= 0]/test_data2$max_credit_amount_jun[test_data2$max_credit_amount_jun!= 0]

## 4

test_data2$cc_cons_apr_prop = 0
test_data2$cc_cons_apr_prop[test_data2$cc_cons_sum != 0] = test_data2$cc_cons_apr[test_data2$cc_cons_sum != 0]/test_data2$cc_cons_sum[test_data2$cc_cons_sum != 0]

test_data2$cc_cons_may_prop = 0
test_data2$cc_cons_may_prop[test_data2$cc_cons_sum != 0] = test_data2$cc_cons_may[test_data2$cc_cons_sum != 0]/test_data2$cc_cons_sum[test_data2$cc_cons_sum != 0]

test_data2$cc_cons_jun_prop = 0
test_data2$cc_cons_jun_prop[test_data2$cc_cons_sum != 0] = test_data2$cc_cons_jun[test_data2$cc_cons_sum != 0]/test_data2$cc_cons_sum[test_data2$cc_cons_sum != 0]

test_data2$dc_cons_apr_prop = 0
test_data2$dc_cons_apr_prop[test_data2$dc_cons_sum != 0] = test_data2$dc_cons_apr[test_data2$dc_cons_sum != 0]/test_data2$dc_cons_sum[test_data2$dc_cons_sum != 0]

test_data2$dc_cons_may_prop = 0
test_data2$dc_cons_may_prop[test_data2$dc_cons_sum != 0] = test_data2$dc_cons_may[test_data2$dc_cons_sum != 0]/test_data2$dc_cons_sum[test_data2$dc_cons_sum != 0]

test_data2$dc_cons_jun_prop = 0
test_data2$dc_cons_jun_prop[test_data2$dc_cons_sum != 0] = test_data2$dc_cons_jun[test_data2$dc_cons_sum != 0]/test_data2$dc_cons_sum[test_data2$dc_cons_sum != 0]

test_data2$credit_amount_apr_prop = 0
test_data2$credit_amount_apr_prop[test_data2$credit_amount_sum != 0] = test_data2$credit_amount_apr[test_data2$credit_amount_sum != 0]/test_data2$credit_amount_sum[test_data2$credit_amount_sum != 0]

test_data2$credit_amount_may_prop = 0
test_data2$credit_amount_may_prop[test_data2$credit_amount_sum != 0] = test_data2$credit_amount_may[test_data2$credit_amount_sum != 0]/test_data2$credit_amount_sum[test_data2$credit_amount_sum != 0]

test_data2$credit_amount_jun_prop = 0
test_data2$credit_amount_jun_prop[test_data2$credit_amount_sum != 0] = test_data2$credit_amount_jun[test_data2$credit_amount_sum != 0]/test_data2$credit_amount_sum[test_data2$credit_amount_sum != 0]

test_data2$debit_amount_apr_prop = 0
test_data2$debit_amount_apr_prop[test_data2$debit_amount_sum != 0] = test_data2$debit_amount_apr[test_data2$debit_amount_sum != 0]/test_data2$debit_amount_sum[test_data2$debit_amount_sum != 0]

test_data2$debit_amount_may_prop = 0
test_data2$debit_amount_may_prop[test_data2$debit_amount_sum != 0] = test_data2$debit_amount_may[test_data2$debit_amount_sum != 0]/test_data2$debit_amount_sum[test_data2$debit_amount_sum != 0]

test_data2$debit_amount_jun_prop = 0
test_data2$debit_amount_jun_prop[test_data2$debit_amount_sum != 0] = test_data2$debit_amount_jun[test_data2$debit_amount_sum != 0]/test_data2$debit_amount_sum[test_data2$debit_amount_sum != 0]

## 5

test_data2$d_by_c_cons_ratio_apr = 0
test_data2$d_by_c_cons_ratio_apr[test_data2$cc_cons_apr != 0] = test_data2$dc_cons_apr[test_data2$cc_cons_apr != 0]/test_data2$cc_cons_apr[test_data2$cc_cons_apr != 0]

test_data2$d_by_c_cons_ratio_may = 0
test_data2$d_by_c_cons_ratio_may[test_data2$cc_cons_may != 0] = test_data2$dc_cons_may[test_data2$cc_cons_may != 0]/test_data2$cc_cons_may[test_data2$cc_cons_may != 0]

test_data2$d_by_c_amount_ratio_jun = 0
test_data2$d_by_c_amount_ratio_jun[test_data2$credit_amount_jun != 0] = test_data2$debit_amount_jun[test_data2$credit_amount_jun != 0]/test_data2$credit_amount_jun[test_data2$credit_amount_jun != 0]

test_data2$d_by_c_amount_ratio_apr = 0
test_data2$d_by_c_amount_ratio_apr[test_data2$credit_amount_apr != 0] = test_data2$debit_amount_apr[test_data2$credit_amount_apr != 0]/test_data2$credit_amount_apr[test_data2$credit_amount_apr != 0]

test_data2$d_by_c_amount_ratio_may = 0
test_data2$d_by_c_amount_ratio_may[test_data2$credit_amount_may != 0] = test_data2$debit_amount_may[test_data2$credit_amount_may != 0]/test_data2$credit_amount_may[test_data2$credit_amount_may != 0]

test_data2$d_by_c_amount_ratio_jun = 0
test_data2$d_by_c_amount_ratio_jun[test_data2$credit_amount_jun != 0] = test_data2$debit_amount_jun[test_data2$credit_amount_jun != 0]/test_data2$credit_amount_jun[test_data2$credit_amount_jun != 0]

# Predicting based all models

model_xgb_basic = readRDS("model_xgb_basic.rds")

pred = predict(model_xgb_basic, test_data2)
pred = ceiling(exp(pred) - 1)

result = data.frame(id = test_data$id, cc_cons = pred)
write.csv(result, "submission_xgb_basic.csv",row.names = F)

model_xgb_final = readRDS("model_xgb_final.rds")

pred = predict(model_xgb_final, test_data2)
pred = ceiling(exp(pred) - 1)

result = data.frame(id = test_data$id, cc_cons = pred)
write.csv(result, "submission_xgb_final.csv",row.names = F)

model_xgb_ultra_final = readRDS("model_xgb_ultra_final.rds")

pred = predict(model_xgb_ultra_final, test_data2)
pred = ceiling(exp(pred) - 1)

result = data.frame(id = test_data$id, cc_cons = pred)
write.csv(result, "submission_xgb_ultra_final.csv",row.names = F)

model_rf2 = readRDS("model_rf2.rds")

pred = predict(model_rf2, test_data2)
pred = ceiling(exp(pred) - 1)

result = data.frame(id = test_data$id, cc_cons = pred)
write.csv(result, "submission_rf2.csv",row.names = F)

# Stacks

pred_rf = predict(model_rf, test_data2)
pred_xgb = predict(model_xgb_ultra_final, test_data2)
pred_glmnet = predict(model_glm, test_data2)

all_preds = data.frame(RF = pred_rf, XGB = pred_xgb, GLM = pred_glmnet)

model_stack_xgb = readRDS("model_stack_xgb2.rds")

pred = predict(model_stack_xgb, all_preds)
pred = ceiling(exp(pred) - 1)

result = data.frame(id = test_data$id, cc_cons = pred)
write.csv(result, "submission_stack_xgb2.csv",row.names = F)

model_stack_glm = readRDS("model_stack_glm2.rds")

pred = predict(model_stack_glm, all_preds)
pred = ceiling(exp(pred) - 1)

result = data.frame(id = test_data$id, cc_cons = pred)
write.csv(result, "submission_stack_glm2.csv",row.names = F)

# Stacks

pred_rf = predict(model_rf, test_data2)
pred_xgb = predict(model_xgb_ultra_final, test_data2)
pred_glmnet = predict(model_glm, test_data2)
pred_boosted_glmnet = predict(model_boost_glm, test_data2)

all_preds = data.frame(RF = pred_rf, XGB = pred_xgb, GLM = pred_glmnet, GLM_BOOST = pred_boosted_glmnet)

model_stack_xgb = readRDS("model_stack_xgb4.rds")

pred = predict(model_stack_xgb, all_preds)
pred = ceiling(exp(pred) - 1)

result = data.frame(id = test_data$id, cc_cons = pred)
write.csv(result, "submission_stack_xgb4.csv",row.names = F)

model_stack_glm = readRDS("model_stack_glm4.rds")

pred = predict(model_stack_glm, all_preds)
pred = ceiling(exp(pred) - 1)

result = data.frame(id = test_data$id, cc_cons = pred)
write.csv(result, "submission_stack_glm4.csv",row.names = F)

model_stack_glm = readRDS("model_stack_glm_boost2.rds")

pred = predict(model_stack_glm, all_preds)
pred = ceiling(exp(pred) - 1)

result = data.frame(id = test_data$id, cc_cons = pred)
write.csv(result, "submission_stack_boost2.csv",row.names = F)

