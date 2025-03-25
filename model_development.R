df <- read.csv('preprocessing.csv')
set.seed(31)
library(rsample)
split <- initial_split(df, prop = 0.60, strata = Class)
training <- training(split)
test <- testing(split)
dim(training)
dim(test)
table(training$Class)
###Balancing Methods###
#ROSE
install.packages("ROSE")
library("ROSE")
set.seed(31)
balanced_rose<-ROSE(Class ~ ., data = training)$data
table(balanced_rose$Class)

#ovun - both - N is based on twice the size of the zero class
set.seed(31)
balanced_ovun<-ovun.sample(Class ~ ., data = training, method = "both", N = 4824, seed = 31)$data
table(balanced_ovun$Class)

#ovun - under - N is twice the minority class
set.seed(31)
balanced_under<-ovun.sample(Class ~ ., data = training, method = "under", N = 356, seed = 31)$data
table(balanced_under$Class)

#ovun - over - N is twice the majority class
set.seed(31)
balanced_over<-ovun.sample(Class ~ ., data = training, method = "over", N = 4824, seed = 31)$data
table(balanced_over$Class)


###Feature Selection Methods###
## chi-square test
ct <- table(df$chest_pain, df$class)
ct
chisq.test(ct)

# Assuming 'df' is your data frame with 4318 rows and 57 columns,
# and 'target' is the target variable (e.g., classification outcome)

# Convert the target variable to a factor (if it's not already)
df$Class <- as.factor(df$Class)

# Create an empty vector to store p-values
p_values <- numeric(ncol(df) - 1)  # Excluding the target column

# Loop over each feature (column) and perform Chi-squared test
for(i in 1:(ncol(df) - 1)) {
  # Perform Chi-squared test between target and each feature
  chi_test <- chisq.test(table(df$Class, df[,i]))
  
  # Store the p-value
  p_values[i] <- chi_test$p.value
}

# Create a data frame of features and their p-values
feature_p_values <- data.frame(
  Feature = colnames(df)[-ncol(df)],  # Exclude target column
  P_Value = p_values
)

# Sort the features by p-value (smallest p-value first)
feature_p_values_sorted <- feature_p_values[order(feature_p_values$P_Value), ]

# View the top features with lowest p-values (strongest associations with the target)
print(feature_p_values_sorted)

#need to set a threshold for important features.
df_chi_square_selected<- feature_p_values_sorted$Feature[feature_p_values_sorted$P_Value < 0.05]
df_chi_square_selected


###Naive Bayesian###


###Linear Regression###


###Decision Tree###


###Random Forest###


###Linear Discriminant Analysis###