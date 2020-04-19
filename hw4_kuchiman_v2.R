setwd("C:/Users/Narasimha/repos/cs636/HW4/")
library(data.table)
library(caret)
library(pROC)

split_number = 0.5

# Load data
train = fread('train.csv')
members = fread('members_v3.csv')
setkey(train,"msno")
setkey(members,"msno")

# Join Data by "msno"
merged_data = merge(train, members, by="msno")
merged_data <- merged_data[,-(1),drop=FALSE]

merged_data$gender <- revalue(merged_data$gender, c("female"=1))
merged_data$gender <- revalue(merged_data$gender, c("male"=0))
merged_data$gender[merged_data$gender == ""] = 0.5
merged_data$registration_init_time = as.numeric(Sys.Date() - as.Date(sprintf("%08d",merged_data$registration_init_time), origin="1970-01-01", format="%Y%m%d"))


# Get indicies
y = merged_data[, 1]
y = y[[1]]

# Extract random sample of indices for test data
set.seed(42)
test_inds = createDataPartition(y = 1:length(y), p = split_number, list = F)

# Split data into test/train using indices
X_train = merged_data[-test_inds, ]
X_test = merged_data[test_inds, ]


model <- glm(is_churn ~ ., data = X_train, family = "binomial")

# Predicting probabilities
prob=predict(model,X_test,type="response")
X_test$prob <- prob
g <- roc(is_churn ~ prob, data = X_test)

# Plotting ROC and calculating AUC
plot(g, main='ROC')
print(auc(g))

# Training the model using cross_validation control
train_control = trainControl(method="cv", number=5)
model = train(is_churn ~ ., data=X_train, method="glm", family=binomial, trControl=train_control)
