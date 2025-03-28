#Notes:
##May need to adjust the trainControl. If so will need to add the model label to that piece to.
##Only need one trainControl and one grid to train the models on. Set the gird with the model label at the start
##Command + F 'nb' to use the same format as below but change the model label. Should allow for easier effort than rewriting everything

#Will need to adjust the model train code (3rd line in each option) but should be able to keep it near the same.
##Have to change the method and adjust the tuneGrid (which should be in the replacement mentioned above)
##I haven't tried converting it to a new model yet but I'm hoping it is a smooth transition

#As long as this code is open in the same environment as the set generation code it should be able to access from the environment without drowning this file.


test$Class<- as.factor(test$Class)

train_control <- trainControl(method = "cv", number = 10)

# Define Hyperparameter Grid for Naive Bayes
nb_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),  # Kernel Density Estimation
  fL= c(0, 0.5, 1, 2, 5),  # Laplace smoothing factor
  adjust = c(1, 1.5, 2)  # Bandwidth adjustment for kernel smoothing
)


#Option 1
balanced_rose_selected$Class <- as.factor(balanced_rose_selected$Class)
set.seed(31)
nb_model_rose_gini <- train(Class ~ ., data = balanced_rose_selected,method = "nb", trControl = train_control, tuneGrid = nb_grid)
nb_best_params_rose_gini <- nb_model$bestTune
print(nb_best_params_rose_gini)
nb_pred_rose_gini <- predict(nb_model_rose_gini, newdata = test)
nb_performance_measures_rose_gini  <- confusionMatrix(data=nb_pred_rose_gini, 
                                         reference = test$Class)
nb_performance_measures_rose_gini

#Option 2 - balanced_ovun_selected
set.seed(31)
balanced_ovun_selected$Class <- as.factor(balanced_ovun_selected$Class)
nb_model_ovun_gini <- train(Class ~ ., data = balanced_ovun_selected,method = "nb", trControl = train_control, tuneGrid = nb_grid)
nb_best_params_ovun_gini <- nb_model_ovun_gini$bestTune
print(nb_best_params_ovun_gini)
nb_pred_ovun_gini <- predict(nb_model_ovun_gini, newdata = test)
nb_performance_measures_ovun_gini  <- confusionMatrix(data=nb_pred_ovun_gini,reference = test$Class)
nb_performance_measures_ovun_gini

#Option 3 - balanced_under_selected
set.seed(31)
balanced_under_selected$Class <- as.factor(balanced_under_selected$Class)
nb_model_under_gini <- train(Class ~ ., data = balanced_under_selected,method = "nb", trControl = train_control, tuneGrid = nb_grid)
nb_best_params_under_gini <- nb_model_under_gini$bestTune
print(nb_best_params_under_gini)
nb_pred_under_gini <- predict(nb_model_under_gini, newdata = test)
nb_performance_measures_under_gini  <- confusionMatrix(data=nb_pred_under_gini, 
                                                   reference = test$Class)
nb_performance_measures_under_gini

#Option 4 - balanced_over_selected
set.seed(31)
balanced_over_selected$Class <- as.factor(balanced_over_selected$Class)
nb_model_over_gini <- train(Class ~ ., data = balanced_over_selected,method = "nb", trControl = train_control, tuneGrid = nb_grid)
nb_best_params_over_gini <- nb_model_over_gini$bestTune
print(nb_best_params_over_gini)
nb_pred_over_gini <- predict(nb_model_over_gini, newdata = test)
nb_performance_measures_over_gini  <- confusionMatrix(data=nb_pred_over_gini, 
                                                    reference = test$Class)
nb_performance_measures_over_gini

#Option 5 -final_data_rose
set.seed(31)
final_data_rose$Class <- as.factor(final_data_rose$Class)
nb_model_rose_PCA <- train(Class ~ ., data = final_data_rose,method = "nb", trControl = train_control, tuneGrid = nb_grid)
nb_best_params_rose_PCA <- nb_model_rose_PCA$bestTune
print(nb_best_params_rose_PCA)
nb_pred_rose_PCA <- predict(nb_model_rose_PCA, newdata = test)
nb_performance_measures_rose_PCA  <- confusionMatrix(data=nb_pred_rose_PCA, 
                                                   reference = test$Class)
nb_performance_measures_rose_PCA

#Option 6 - final_data_ovun
set.seed(31)
final_data_ovun$Class <- as.factor(final_data_ovun$Class)
nb_model_ovun_PCA <- train(Class ~ ., data = final_data_ovun,method = "nb", trControl = train_control, tuneGrid = nb_grid)
nb_best_params_ovun_PCA <- nb_model_ovun_PCA$bestTune
print(nb_best_params_ovun_PCA)
nb_pred_ovun_PCA <- predict(nb_model_ovun_PCA, newdata = test)
nb_performance_measures_ovun_PCA  <- confusionMatrix(data=nb_pred_ovun_PCA, 
                                                  reference = test$Class)
nb_performance_measures_ovun_PCA

#Option 7 - final_data_under
set.seed(31)
final_data_under$Class <- as.factor(final_data_under$Class)
nb_model_under_PCA <- train(Class ~ ., data = final_data_under,method = "nb", trControl = train_control, tuneGrid = nb_grid)
nb_best_params_under_PCA <- nb_model_under_PCA$bestTune
print(nb_best_params_under_PCA)
nb_pred_under_PCA <- predict(nb_model_under_PCA, newdata = test)
nb_performance_measures_under_PCA  <- confusionMatrix(data=nb_pred_under_PCA, 
                                                  reference = test$Class)
nb_performance_measures_under_PCA

#Option 8 - final_data_over
set.seed(31)
final_data_over$Class <- as.factor(final_data_over$Class)
nb_model_over_PCA <- train(Class ~ ., data = final_data_over,method = "nb", trControl = train_control, tuneGrid = nb_grid)
nb_best_params_over_PCA <- nb_model_over_PCA$bestTune
print(nb_best_params_over_PCA)
nb_pred_over_PCA <- predict(nb_model_over_PCA, newdata = test)
nb_performance_measures_over_PCA  <- confusionMatrix(data=nb_pred_over_PCA, 
                                                  reference = test$Class)
nb_performance_measures_over_PCA

#Option 9 - balanced_rose_lasso_selected
balanced_rose_lasso_selected$Class <- as.factor(balanced_rose_lasso_selected$Class)
set.seed(31)
nb_model_rose_lasso <- train(Class ~ ., data = balanced_rose_selected,method = "nb", trControl = train_control, tuneGrid = nb_grid)
nb_best_params_rose_lasso <- nb_model_rose_lasso$bestTune
print(nb_best_params_rose_lasso)
nb_pred_rose_lasso <- predict(nb_model_rose_lasso, newdata = test)
nb_performance_measures_rose_lasso  <- confusionMatrix(data=nb_pred_rose_lasso, 
                                                   reference = test$Class)
nb_performance_measures_rose_lasso

#Option 10 - balanced_ovun_lasso_selected
balanced_ovun_lasso_selected$Class <- as.factor(balanced_ovun_lasso_selected$Class)
set.seed(31)
nb_model_ovun_lasso <- train(Class ~ ., data = balanced_ovun_selected,method = "nb", trControl = train_control, tuneGrid = nb_grid)
nb_best_params_ovun_lasso <- nb_model_ovun_lasso$bestTune
print(nb_best_params_ovun_lasso)
nb_pred_ovun_lasso <- predict(nb_model_ovun_lasso, newdata = test)
nb_performance_measures_ovun_lasso  <- confusionMatrix(data=nb_pred_ovun_lasso, 
                                                    reference = test$Class)
nb_performance_measures_ovun_lasso

#Option 11 - balanced_under_lasso_selected
balanced_under_lasso_selected$Class <- as.factor(balanced_under_lasso_selected$Class)
set.seed(31)
nb_model_under_lasso <- train(Class ~ ., data = balanced_under_selected,method = "nb", trControl = train_control, tuneGrid = nb_grid)
nb_best_params_under_lasso <- nb_model_under_lasso$bestTune
print(nb_best_params_under_lasso)
nb_pred_under_lasso <- predict(nb_model_under_lasso, newdata = test)
nb_performance_measures_under_lasso  <- confusionMatrix(data=nb_pred_under_lasso, 
                                                    reference = test$Class)
nb_performance_measures_under_lasso

#Option 12 - balanced_over_lasso_selected
balanced_over_lasso_selected$Class <- as.factor(balanced_over_lasso_selected$Class)
set.seed(31)
nb_model_over_lasso <- train(Class ~ ., data = balanced_over_selected,method = "nb", trControl = train_control, tuneGrid = nb_grid)
nb_best_params_over_lasso <- nb_model_over_lasso$bestTune
print(nb_best_params_over_lasso)
nb_pred_over_lasso <- predict(nb_model_over_lasso, newdata = test)
nb_performance_measures_over_lasso  <- confusionMatrix(data=nb_pred_over_lasso, 
                                                    reference = test$Class)
nb_performance_measures_over_lasso

system("git status")
system("git add model_guide(NB).R")
system('git commit -m "Updated model_guide(NB).R with latest changes"')
system("git push origin main")

