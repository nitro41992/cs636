
# A) Kaggle Data Exploration -------------------------------------------------------

setwd('C:/Users/Narasimha/repos/cs636-1/HW3')
data = data.table::fread('transactions.csv')

# 1)
# payment_method_id, payment_plan_days, plan_list_price, actual_amount_paid, transaction_date and membership_expire_date are quantitative (numerical)
# is_cancel and is_auto_renew are categorical

# 2)
par(mfrow=c(3,2), mar = c(5, 5, 0.5, 0.5), mgp = c(5, 1, 0))
hist(data$payment_method_id)
hist(data$payment_plan_days)
hist(data$plan_list_price)
hist(data$actual_amount_paid)
hist(data$transaction_date)
hist(data$membership_expire_date)
 
# 3)
print(table(data$is_cancel))
print(table(data$is_auto_renew))

# B) -------------------------------------------------------

# 2.4 
# install.packages("UsingR")
library(UsingR)

Central <- central.park
print(table(Central$WX))


print(table(Central$WX, exclude = FALSE))

# The table with exclude = FALSE is better because it shows how sparse the original data is.
# Without considering the NAs, we would not have considered the distribution of the data correctly.

# 2.8
npdb <- npdb
sort(table(npdb$state), decreasing = TRUE)

# 2.9

head(table(npdb$ID), n=30) # Only displaying the first 30

table(table(npdb$ID))

# table(table(npdb$ID)) creates a table showing the number of doctors based on the number of malpractice records.
# Its clear that the majority of doctors had only a few claims while there were a few that had greater than 10
# claims and one who had up to 73 claims.


