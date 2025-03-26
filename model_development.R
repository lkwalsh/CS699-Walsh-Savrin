setwd("C:\\Users\\linds\\OneDrive\\Documents\\GitHub\\CS699-Walsh-Savrin")
system("git status")
system("git add preprocessing_code.R")

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
set.seed(31)
model_rose <- rpart(Class ~ ., data = balanced_rose, method = "class")
feature_importance_rose <- model_rose$variable.importance
print(feature_importance_rose)
selected_features_rose <- names(feature_importance_rose[feature_importance_rose > 0.01])
# Display selected features
print(selected_features_rose)
balanced_rose_selected <- balanced_rose[, c(selected_features_rose, "Class")]
print(balanced_rose_selected)

#OVUN - both
set.seed(31)
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
set.seed(31)
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
set.seed(31)
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

#ROSE
set.seed(31)
# Standardize the data (if it's not already standardized)
scaled_features_rose <- scale(balanced_rose[, -which(names(balanced_rose) == "Class")])
# Perform PCA
pca_result_rose <- prcomp(scaled_features_rose, center = TRUE, scale. = TRUE)
# Check how much variance each principal component explains
summary(pca_result_rose)
# Get PCA loadings (coefficients of the original features for each principal component)
loadings_rose <- pca_result_rose$rotation
print(loadings_rose)
# Get the loadings for the first principal component
pc1_loadings_rose <- loadings_rose[, 1]
# Sort by absolute value to find the most influential features
sorted_pc1_loadings_rose <- sort(abs(pc1_loadings_rose), decreasing = TRUE)
# Display the top features contributing to PC1
top_features_pc1_rose <- names(sorted_pc1_loadings_rose)
print(top_features_pc1_rose)
# Set a threshold for feature selection based on loadings
threshold <- 0.1  # Example threshold
important_features_rose <- names(pc1_loadings_rose[abs(pc1_loadings_rose) > threshold])
# Print the selected features
print(important_features_rose)
# Select the subset of original features based on the identified important ones
final_data_rose <- balanced_rose[, c("Class", important_features_rose)]
# Check the selected data
print(head(final_data_rose))

# OVUN
set.seed(31)
# Standardize the data (if it's not already standardized)
scaled_features_ovun <- scale(balanced_ovun[, -which(names(balanced_ovun) == "Class")])
# Perform PCA
pca_result_ovun <- prcomp(scaled_features_ovun, center = TRUE, scale. = TRUE)
# Check how much variance each principal component explains
summary(pca_result_ovun)
# Get PCA loadings (coefficients of the original features for each principal component)
loadings_ovun <- pca_result_ovun$rotation
# Get the loadings for the first principal component
pc1_loadings_ovun <- loadings_ovun[, 1]
# Sort by absolute value to find the most influential features
sorted_pc1_loadings_ovun <- sort(abs(pc1_loadings_ovun), decreasing = TRUE)
# Display the top features contributing to PC1
top_features_pc1_ovun <- names(sorted_pc1_loadings_ovun)
# Set a threshold for feature selection based on loadings
threshold <- 0.1  # Example threshold
important_features_ovun <- names(pc1_loadings_ovun[abs(pc1_loadings_ovun) > threshold])
# Print the selected features
print(important_features_ovun)
# Select the subset of original features based on the identified important ones
final_data_ovun <- balanced_ovun[, c("Class", important_features_ovun)]
# Check the selected data
print(head(final_data_ovun))

# UNDER
set.seed(31)
scaled_features_under <- scale(balanced_under[, -which(names(balanced_under) == "Class")])
pca_result_under <- prcomp(scaled_features_under, center = TRUE, scale. = TRUE)
summary(pca_result_under)
loadings_under <- pca_result_under$rotation
pc1_loadings_under <- loadings_under[, 1]
sorted_pc1_loadings_under <- sort(abs(pc1_loadings_under), decreasing = TRUE)
top_features_pc1_under <- names(sorted_pc1_loadings_under)
important_features_under <- names(pc1_loadings_under[abs(pc1_loadings_under) > threshold])
print(important_features_under)
final_data_under <- balanced_under[, c("Class", important_features_under)]
print(head(final_data_under))

# OVER
set.seed(31)
scaled_features_over <- scale(balanced_over[, -which(names(balanced_over) == "Class")])
pca_result_over <- prcomp(scaled_features_over, center = TRUE, scale. = TRUE)
summary(pca_result_over)
loadings_over <- pca_result_over$rotation
pc1_loadings_over <- loadings_over[, 1]
sorted_pc1_loadings_over <- sort(abs(pc1_loadings_over), decreasing = TRUE)
top_features_pc1_over <- names(sorted_pc1_loadings_over)
important_features_over <- names(pc1_loadings_over[abs(pc1_loadings_over) > threshold])
print(important_features_over)
final_data_over <- balanced_over[, c("Class", important_features_over)]
print(head(final_data_over))

## LASSO - attempting this because chi square did not work well. We should check with prof
# because this was not in our lecture
install.packages()
library(glmnet)

#ROSE
set.seed(31)
# Convert data to matrix form
x_rose <- model.matrix(Class ~ . - 1, data = balanced_rose)
y_rose <- balanced_rose$Class
# Fit Lasso (alpha = 1) using cross-validation
cv_lasso_rose <- cv.glmnet(x_rose, y_rose, family = "binomial", alpha = 1, type.measure = "class")
# Get coefficients at best lambda
coef_lasso_rose <- coef(cv_lasso_rose, s = "lambda.min")
print(coef_lasso_rose)
# Extract non-zero coefficients (excluding intercept)
selected_lasso_rose <- rownames(coef_lasso_rose)[coef_lasso_rose[,1] != 0]
selected_lasso_rose <- setdiff(selected_lasso_rose, "(Intercept)")
# Subset original data to selected features
balanced_rose_lasso_selected <- balanced_rose[, c(selected_lasso_rose, "Class")]
print(head(balanced_rose_lasso_selected))

# OVUN
set.seed(31)
x_ovun <- model.matrix(Class ~ . - 1, data = balanced_ovun)
y_ovun <- balanced_ovun$Class
cv_lasso_ovun <- cv.glmnet(x_ovun, y_ovun, family = "binomial", alpha = 1, type.measure = "class")
coef_lasso_ovun <- coef(cv_lasso_ovun, s = "lambda.min")
selected_lasso_ovun <- rownames(coef_lasso_ovun)[coef_lasso_ovun[,1] != 0]
selected_lasso_ovun <- setdiff(selected_lasso_ovun, "(Intercept)")
balanced_ovun_lasso_selected <- balanced_ovun[, c(selected_lasso_ovun, "Class")]

# UNDER
set.seed(31)
x_under <- model.matrix(Class ~ . - 1, data = balanced_under)
y_under <- balanced_under$Class
cv_lasso_under <- cv.glmnet(x_under, y_under, family = "binomial", alpha = 1, type.measure = "class")
coef_lasso_under <- coef(cv_lasso_under, s = "lambda.min")
selected_lasso_under <- rownames(coef_lasso_under)[coef_lasso_under[,1] != 0]
selected_lasso_under <- setdiff(selected_lasso_under, "(Intercept)")
balanced_under_lasso_selected <- balanced_under[, c(selected_lasso_under, "Class")]

# OVER
set.seed(31)
x_over <- model.matrix(Class ~ . - 1, data = balanced_over)
y_over <- balanced_over$Class
cv_lasso_over <- cv.glmnet(x_over, y_over, family = "binomial", alpha = 1, type.measure = "class")
coef_lasso_over <- coef(cv_lasso_over, s = "lambda.min")
selected_lasso_over <- rownames(coef_lasso_over)[coef_lasso_over[,1] != 0]
selected_lasso_over <- setdiff(selected_lasso_over, "(Intercept)")
balanced_over_lasso_selected <- balanced_over[, c(selected_lasso_over, "Class")]


# 
# 

install.packages()
library(caret)

###Naive Bayesian###


###Linear Regression###


###Decision Tree###


###Random Forest###


###Linear Discriminant Analysis###

###KNN###

###Neural Network###

###Logistic Regression###

###Linear SVM###

###Adaboost###

system("git status")
system("git add model_development.R")
system('git commit -m "Updated model_development.R with latest changes"')
system("git push origin main")
