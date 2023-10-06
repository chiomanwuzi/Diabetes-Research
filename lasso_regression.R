
#lasso regression 

set.seed(127)
k_fold_lasso<- cv.glmnet(x_train_scale,y_train_scale, alpha = 1, nfolds = 10)
#find the optimal lambda value by finding the minimum MSE optimal_lambda_lasso<- k_fold_lasso$lambda.min optimal_lambda_lasso_err<-k_fold_lasso$lambda.1se optimal_lambda_lasso
optimal_lambda_lasso_err plot(k_fold_lasso)


set.seed(271)
lasso_model<- glmnet(x_train, y_train, alpha = 1 , lambda = optimal_lambda_lasso_err, standardize = TRUE, family = 'gaussian', standardize.response = FALSE)
lasso_r_squared<- lasso_model$dev.ratio lasso_r_squared
print(lasso_model)
#########################################
lasso_train_predict<- predict(lasso_model, s = optimal_lambda_lasso, newx = x_train) colnames(lasso_train_predict)[1]<- "predictions"
lasso_test_predict<- predict(lasso_model, s = optimal_lambda_lasso, newx = x_test) colnames(lasso_test_predict)[1]<- "predictions"
lasso_train_rmse<- rmse(train_set_full$diabetes_prevelance,lasso_train_predict) lasso_test_rmse<- rmse(test_set_full$diabetes_prevelance,lasso_test_predict)

#Merge the Actual Values from the Testing Sets
train_lasso_1<- merge(lasso_train_predict, cluster_1_set, by = 0)
train_lasso_1_rmse<- rmse(train_lasso_1$diabetes_prevelance, train_lasso_1$predictions)
train_lasso_2<- merge(lasso_train_predict, cluster_2_set, by = 0)
train_lasso_2_rmse<- rmse(train_lasso_2$diabetes_prevelance, train_lasso_2$predictions)
train_lasso_3<- merge(lasso_train_predict, cluster_3_set, by = 0)
train_lasso_3_rmse<- rmse(train_lasso_3$diabetes_prevelance, train_lasso_3$predictions)
train_lasso_4<- merge(lasso_train_predict, cluster_4_set, by = 0)
train_lasso_4_rmse<- rmse(train_lasso_4$diabetes_prevelance, train_lasso_4$predictions)
train_lasso_5<- merge(lasso_train_predict, cluster_5_set, by = 0)
train_lasso_5_rmse<- rmse(train_lasso_5$diabetes_prevelance, train_lasso_5$predictions)
train_lasso_6<- merge(lasso_train_predict, cluster_6_set, by = 0)
train_lasso_6_rmse<- rmse(train_lasso_6$diabetes_prevelance, train_lasso_6$predictions)
train_lasso_7<- merge(lasso_train_predict, cluster_7_set, by = 0)
train_lasso_7_rmse<- rmse(train_lasso_7$diabetes_prevelance, train_lasso_7$predictions)
lasso_training_rmse<- as.data.frame(c(train_lasso_1_rmse, train_lasso_2_rmse, train_lasso_3_rmse, train_lasso_4_rmse, train_lasso_5_rmse, train_lasso_6_rmse, train_lasso_7_rmse))
colnames(lasso_training_rmse)[1]<- "lasso_train_rmse"
############TEST RMSE##############################
test_lasso_1<- merge(lasso_test_predict, cluster_1_set, by = 0)
test_lasso_1_rmse<- rmse(test_lasso_1$diabetes_prevelance, test_lasso_1$predictions)
test_lasso_2<- merge(lasso_test_predict, cluster_2_set, by = 0)
test_lasso_2_rmse<- rmse(test_lasso_2$diabetes_prevelance, test_lasso_2$predictions)
test_lasso_3<- merge(lasso_test_predict, cluster_3_set, by = 0)
test_lasso_3_rmse<- rmse(test_lasso_3$diabetes_prevelance, test_lasso_3$predictions)
#sqrt(mean((test_lasso_3$predictions - test_lasso_3$diabetes_prevelance) ^ 2))

test_lasso_4<- merge(lasso_test_predict, cluster_4_set, by = 0)
test_lasso_4_rmse<- rmse(test_lasso_4$diabetes_prevelance, test_lasso_4$predictions)
test_lasso_5<- merge(lasso_test_predict, cluster_5_set, by = 0)
test_lasso_5_rmse<- rmse(test_lasso_5$diabetes_prevelance, test_lasso_5$predictions)
test_lasso_6<- merge(lasso_test_predict, cluster_6_set, by = 0)
test_lasso_6_rmse<- rmse(test_lasso_6$diabetes_prevelance, test_lasso_6$predictions)
test_lasso_7<- merge(lasso_test_predict, cluster_7_set, by = 0)
test_lasso_7_rmse<- rmse(test_lasso_7$diabetes_prevelance, test_lasso_7$predictions)
lasso_testing_rmse<- as.data.frame(c(test_lasso_1_rmse, test_lasso_2_rmse, test_lasso_3_rmse, test_lasso_4_rmse, test_lasso_5_rmse, test_lasso_6_rmse, test_lasso_7_rmse))
colnames(lasso_testing_rmse)[1]<- "lasso_test_rmse"
############Variable Importance####################
lasso_var_imp<- as.data.frame(varImp(lasso_model, lambda = optimal_lambda_lasso)) lasso_var_imp<-data.frame(overall = lasso_var_imp$Overall, variable_name = rownames(lasso_var_imp))
lasso_var_imp<- lasso_var_imp[order(lasso_var_imp$overall, decreasing = T),] lasso_var_imp
lasso_var_imp<- subset(lasso_var_imp, lasso_var_imp$overall >0) colnames(lasso_var_imp)[1]<- "coefficents"
View(lasso_var_imp)