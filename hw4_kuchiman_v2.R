setwd("C:/Users/Narasimha/repos/cs636/HW4/")
library(data.table)
library(caret)
library(pROC)
library(plyr)
library(boot)

split_number = 0.5
folds = 5

# Load data
train = fread('train.csv')
members = fread('members_v3.csv')
setkey(train,"msno")
setkey(members,"msno")

# Join Data by "msno"
merged_data = merge(train, members, by="msno")
merged_data <- merged_data[,-(1),drop=FALSE]

# Converting gender column to a scalar. Replacing blank rows with 0.5
merged_data$gender <- revalue(merged_data$gender, c("female"=1))
merged_data$gender <- revalue(merged_data$gender, c("male"=0))
merged_data$gender[merged_data$gender == ""] = 0.5

# Converting gender and is_churn into a factor
merged_data$is_churn = as.factor(merged_data$is_churn)
merged_data$gender = as.factor(merged_data$gender)

# Converting registration_init_time into time delta of today - registration_init_time
merged_data$registration_init_time = as.numeric(
  Sys.Date() - as.Date(sprintf("%08d",merged_data$registration_init_time),
                       origin="1970-01-01",
                       format="%Y%m%d"))

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
model <- glm(is_churn ~ ., data = X_train, family = "binomial")


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
