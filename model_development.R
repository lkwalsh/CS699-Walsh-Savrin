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
#need to do feature selection on each of the methods above

### chi-square test ### may not be best since assume expectations over 5 and not having that creates issues
#ROSE


# 
# 
# # Convert the target variable to a factor (if it's not already)
# balanced_rose$Class <- as.factor(balanced_rose$Class)
# 
# # Create an empty vector to store p-values
# p_values_rose <- numeric(ncol(balanced_rose) - 1)  # Excluding the target column
# 
# # Loop over each feature (column) and perform Chi-squared test
# for(i in 1:(ncol(balanced_rose) - 1)) {
#   # Perform Chi-squared test between target and each feature
#   chi_test_rose <- chisq.test(table(balanced_rose$Class, balanced_rose[,i]))
#   
#   # Store the p-value
#   p_values_rose[i] <- chi_test_rose$p.value
# } #get warnings here about accuracy of the test results
# 
# # Create a data frame of features and their p-values
# feature_p_values_rose <- data.frame(
#   Feature = colnames(balanced_rose)[-ncol(balanced_rose)],  # Exclude target column
#   P_Value = p_values_rose
# )
# 
# # Sort the features by p-value (smallest p-value first) - do we need the sorting
# feature_p_values_sorted_rose <- feature_p_values_rose[order(feature_p_values_rose$P_Value), ]
# feature_p_values_rose
# #need to set a threshold for important features. standard is at 0.05. Will do that unless it removes too many
# df_chi_square_selected_rose<- feature_p_values_sorted_rose$Feature[feature_p_values_sorted_rose$P_Value < 0.05]
# df_chi_square_selected_rose
# 
# summary(balanced_rose)
# 

##Gini Index
library(rpart)

#ROSE
model_rose <- rpart(Class ~ ., data = balanced_rose, method = "class")
feature_importance_rose <- model_rose$variable.importance
print(feature_importance_rose)
selected_features_rose <- names(feature_importance_rose[feature_importance_rose > 0.01])

# Display selected features
print(selected_features_rose)
balanced_rose_selected <- balanced_rose[, c(selected_features_rose, "Class")]
print(balanced_rose_selected)

#OVUN - both
model_ovun <- rpart(Class ~ ., data = balanced_ovun, method = "class")
feature_importance_ovun <- model_ovun$variable.importance
print(feature_importance_ovun)
selected_features_ovun <- names(feature_importance_ovun[feature_importance_ovun > 0.01])

# Display selected features
print(selected_features_ovun)
balanced_ovun_selected <- balanced_ovun[, c(selected_features_ovun, "Class")]
print(balanced_ovun_selected)
table(balanced_ovun_selected$Class)

#OVUN - Under
model_under <- rpart(Class ~ ., data = balanced_under, method = "class")
feature_importance_under <- model_under$variable.importance
print(feature_importance_under)
selected_features_under <- names(feature_importance_under[feature_importance_under > 0.01])

# Display selected features
print(selected_features_under)
balanced_under_selected <- balanced_under[, c(selected_features_under, "Class")]
print(balanced_under_selected)
table(balanced_under_selected$Class)

#OVUN - over
model_over <- rpart(Class ~ ., data = balanced_over, method = "class")
feature_importance_over <- model_over$variable.importance
print(feature_importance_over)
selected_features_over <- names(feature_importance_over[feature_importance_over > 0.01])

# Display selected features
print(selected_features_over)
balanced_over_selected <- balanced_over[, c(selected_features_over, "Class")]
print(balanced_over_selected)
table(balanced_over_selected$Class)

##PCA - use loadings to identify features


###Naive Bayesian###


###Linear Regression###


###Decision Tree###


###Random Forest###


###Linear Discriminant Analysis###

system("git status")
system("git add model_development.R")
system('git commit -m "Updated model_development.R with latest changes"')
system("git push origin main")
