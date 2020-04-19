setwd("C:/Users/Narasimha/repos/cs636/HW4/")
library(data.table)
library(caret)
library(pROC)
library(plyr)
library(boot)

split_number = 0.5
folds = 5

# Load data
train = fread('train_v2.csv')
transactions = fread('transactions_v2.csv')
setkey(train,"msno")
setkey(transactions,"msno")

# Join Data by "msno"
merged_data = merge(train, transactions, by="msno")

# Get indicies
y = merged_data[, 1]
y = y[[1]]

# Extract random sample of indices for test data
set.seed(42)
test_inds = createDataPartition(y = 1:length(y), p = split_number, list = F)

# Split data into test/train using indices
X_train = merged_data[-test_inds, ]
X_test = merged_data[test_inds, ]

# Fitting to the model
model <- glm(is_churn ~ payment_method_id+payment_plan_days+plan_list_price+actual_amount_paid+is_auto_renew+is_cancel, data = X_train, family = "binomial")


# Predicting probabilities
prob=predict(model,X_test,type="response")

# Gathering predictions and calculating prediction error
preds = round(prob)
pred_error = mean(preds != X_test$is_churn)
cat('The classification error is', pred_error,'\n')

# Calculating ROC
X_test$prob <- prob
g <- roc(is_churn ~ prob, data = X_test, levels = c(0, 1), direction = "<")

# Plotting ROC and calculating AUC
plot(g, main='ROC')
print(auc(g))

# Training the model using cross_validation control and obtaining the prediction error.
# Note: This error will be closer to the true classification error because it is an iterative test of the model using random sampling
# of the train data.
kf = cv.glm(data=X_train, glmfit=model, K=folds)
cat('The cross-validation estimate of prediction error is', kf$delta[1])


# Getting submission file
submission = fread('sample_submission_v2.csv')
setkey(submission,"msno")

# Join Data by "msno"
sub_data = merge(x = submission, y = transactions, by = "msno", all.x = TRUE)

# Predictions for submission data set
new_probs = predict(model,sub_data,type="response")
new_preds = round(new_probs)

# Appending data set to submission data
sub_data$is_churn = new_preds

# Setting up output of predictions. Cleaning any NA predictions
out = sub_data[, c(1,2)]
out = unique(out)
out$is_churn[is.na(out$is_churn)] = 0

# Exporting to csv
write.csv(out,"submission.csv", row.names = FALSE)

