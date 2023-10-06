#ridge regression model


set.seed(127)
k_fold_ridge<- cv.glmnet(x_train_scale,y_train_scale,alpha = 0, nfolds= 40) #summary(k_fold_ridge)
#find the optimal lambda value by finding the minimum MSE optimal_lambda_ridge<- k_fold_ridge$lambda.min optimal_lambda_ridge #
plot(k_fold_ridge) 
ridge_model<- glmnet(x_train, y_train, alpha = 0, lambda = optimal_lambda_ridge, standardize = TRUE, family = 'gaussian', standardize.response = FALSE)
summary(ridge_model)
predict_test_ridge<- predict(ridge_model, s = optimal_lambda_ridge, newx = x_train) predict_test_ridge
ridge_r_squared<- ridge_model$dev.ratio ridge_r_squared
######TRAINING SET RMSE############

ridge_train_predict<- predict(ridge_model, s = optimal_lambda_ridge, newx = x_train) colnames(ridge_train_predict)[1]<- "predictions"
ridge_test_predict<- predict(ridge_model, s = optimal_lambda_ridge, newx = x_test) colnames(ridge_test_predict)[1]<- "predictions"
ridge_train_rmse<- rmse(train_set_full$diabetes_prevelance, ridge_train_predict) ridge_test_rmse<- rmse(test_set_full$diabetes_prevelance, ridge_test_predict)
#####MERGE THE TRAIN SET WITH THE CLUSTERS#############
train_ridge_1<- merge(ridge_train_predict, cluster_1_set, by = 0)
train_ridge_rmse_1<- rmse(train_ridge_1$diabetes_prevelance, train_ridge_1$predictions)
train_ridge_2<- merge(ridge_train_predict, cluster_2_set, by = 0)
train_ridge_rmse_2<- rmse(train_ridge_2$diabetes_prevelance, train_ridge_2$predictions)
train_ridge_3<- merge(ridge_train_predict, cluster_3_set, by = 0)
train_ridge_rmse_3<- rmse(train_ridge_3$diabetes_prevelance, train_ridge_3$predictions)
train_ridge_4<- merge(ridge_train_predict, cluster_4_set, by = 0)
train_ridge_rmse_4<- rmse(train_ridge_4$diabetes_prevelance, train_ridge_4$predictions)
train_ridge_5<- merge(ridge_train_predict, cluster_5_set, by = 0)
train_ridge_rmse_5<- rmse(train_ridge_5$diabetes_prevelance, train_ridge_5$predictions)
train_ridge_6<- merge(ridge_train_predict, cluster_6_set, by = 0)
train_ridge_rmse_6<- rmse(train_ridge_6$diabetes_prevelance, train_ridge_6$predictions)
train_ridge_7<- merge(ridge_train_predict, cluster_7_set, by = 0)
train_ridge_rmse_7<- rmse(train_ridge_7$diabetes_prevelance, train_ridge_7$predictions)
ridge_train_rmse_df<- as.data.frame(c(train_ridge_rmse_1,train_ridge_rmse_2, train_ridge_rmse_3, train_ridge_rmse_4, train_ridge_rmse_5, train_ridge_rmse_6, train_ridge_rmse_7))
colnames(ridge_train_rmse_df)[1]<- "train_ridge_rmse"

###################TEST SET MERGE WITH CLUSTERS AND RMSE##########
test_ridge_1<- merge(ridge_test_predict, cluster_1_set, by = 0)
test_ridge_rmse_1<- rmse(test_ridge_1$diabetes_prevelance, test_ridge_1$predictions)
test_ridge_2<- merge(ridge_test_predict, cluster_2_set, by = 0)
test_ridge_rmse_2<- rmse(test_ridge_2$diabetes_prevelance, test_ridge_2$predictions)
test_ridge_3<- merge(ridge_test_predict, cluster_3_set, by = 0)
test_ridge_rmse_3<- rmse(test_ridge_3$diabetes_prevelance, test_ridge_3$predictions)
test_ridge_4<- merge(ridge_test_predict, cluster_4_set, by = 0)
test_ridge_rmse_4<- rmse(test_ridge_4$diabetes_prevelance, test_ridge_4$predictions)
test_ridge_5<- merge(ridge_test_predict, cluster_5_set, by = 0)
test_ridge_rmse_5<- rmse(test_ridge_5$diabetes_prevelance, test_ridge_5$predictions)
test_ridge_6<- merge(ridge_test_predict, cluster_6_set, by = 0)
test_ridge_rmse_6<- rmse(test_ridge_6$diabetes_prevelance, test_ridge_6$predictions)
test_ridge_7<- merge(ridge_test_predict, cluster_7_set, by = 0)
test_ridge_rmse_7<- rmse(test_ridge_7$diabetes_prevelance, test_ridge_7$predictions)
ridge_test_rmse_df<- as.data.frame(c(test_ridge_rmse_1,test_ridge_rmse_2, test_ridge_rmse_3, test_ridge_rmse_4, test_ridge_rmse_5, test_ridge_rmse_6, test_ridge_rmse_7)) colnames(ridge_test_rmse_df)[1]<- "test_ridge_rmse"
############Variable Importance####################
ridge_var_imp<- as.data.frame(varImp(ridge_model, lambda = optimal_lambda_ridge)) ridge_var_imp<-data.frame(overall = ridge_var_imp$Overall, variable_name = rownames(ridge_var_imp))
ridge_var_imp<- ridge_var_imp[order(ridge_var_imp$overall, decreasing = T),] View(ridge_var_imp)
