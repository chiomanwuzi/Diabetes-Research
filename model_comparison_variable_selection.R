


``` ```{r}
######TRAIN RMSE ACROSS CLUSTERS##########
final_train_cluster<- cbind(step_cluster_train_rmse, back_cluster_train_rmse, train_tree_cluster_rmse, ridge_train_rmse_df, lasso_training_rmse, train_gbm_cluster_rmse)
colnames(final_train_cluster)[1]<- "step_train_rmse" View(final_train_cluster)
######TEST RMSE ACROSS CLUSTERS##########
final_test_cluster<- cbind(step_cluster_test_rmse, back_cluster_test_rmse, test_tree_cluster_rmse, ridge_test_rmse_df, lasso_testing_rmse, test_gbm_cluster_rmse)

colnames(final_test_cluster)[1]<- "step_test_rmse" View(final_test_cluster)


full_model_test<-cbind( step_test_rmse
                        ,test_rmse_back ,test_rmse_tree ,test_rmse_gbm ,ridge_test_rmse ,lasso_test_rmse)
full_model_train<-cbind( step_train_rmse ,train_rmse_back ,train_rmse_tree ,train_rmse_gbm ,ridge_train_rmse ,lasso_train_rmse)
full_model_r_squared<-cbind(step_r_adj , back_R_adj
                            , rf_model$rsq[1000]
                            , gmb_model$results$Rsquared[1]
                            , ridge_r_squared , lasso_r_squared)
colnames(full_model_r_squared)[3]<- "forest_r_squared" colnames(full_model_r_squared)[4]<- "gbm_r_squared"
###train predictions
full_train_diabetes<- as.data.frame(bind_cols(step_train, back_train_predictions, predict_rf_train, train_gbm, ridge_train_predict, lasso_train_predict, y_train)) colnames(full_train_diabetes)<- c("full_var_step_train_predict","full_var_back_train_predict","full_predict_rf_train", "full_train_gbm_predict", "full_ridge_train_predict","full_lasso_train_predict","full_train_actuals")
#test predictions

full_test_diabetes<- bind_cols(step_test, back_test_predictions, predict_rf_test, test_gbm, ridge_test_predict, lasso_test_predict, y_test)
colnames(full_test_diabetes)<- c("full_var_step_test_predict","full_var_back_test_predict", "full_predict_rf_test", "full_test_gbm_predict", "full_ridge_test_predict","full_lasso_test_predict", "full_test_actuals")
```
Create a dataframe Showing the number of times each variable appears ```{r}
install.packages("stringr") library(stringr) #library(data.table)
vars_step<- anova(step_regression)
vars_step<- as.data.frame(row.names(vars_step)) colnames(vars_step)[1]<- "variables_step"
vars_back<- anova(back_regression)
vars_back<- as.data.frame(row.names(vars_back)) colnames(vars_back)[1]<- "variables_back"
#vars_tree<- rf_var_imp_df
vars_tree<- tree_var_imp
vars_tree<- as.data.frame(vars_tree$variable)
vars_gbm<- gbm_var_imp
vars_gbm<- subset(vars_gbm, vars_gbm$overall > 0) vars_gbm<- as.data.frame(vars_gbm$variable_name)
vars_ridge<- ridge_var_imp
vars_ridge<- as.data.frame(vars_ridge$variable_name)
vars_lasso<- lasso_var_imp
vars_lasso<- as.data.frame(vars_lasso$variable_name)
variables_total<- as.data.frame(rbind.fill(vars_step, vars_back, vars_tree, vars_gbm, vars_ridge, vars_lasso))
write.csv(variables_total, "variables_final12.csv")

```
```{r}
full_train_diabetes<- full_train_diabetes %>%
  mutate(full_equal_weights = (step_train + back_train_predictions + predict_rf_train +
                                 train_gbm + ridge_train_predict + lasso_train_predict) / 6) ```
```{r} full_model_train
#take the reciprocal since the smaller the rmse the better full_rmse_weights<- (1/ full_model_train)
#calculate the weights of each of the model as a proportion of all four models full_rmse_weights<- full_rmse_weights/sum(full_rmse_weights)
full_train_diabetes <- full_train_diabetes %>% mutate(
  full_fit_predictions= (
    (full_var_step_train_predict * full_rmse_weights[,"step_train_rmse"]) + (full_var_back_train_predict * full_rmse_weights[,"train_rmse_back"]) + (full_predict_rf_train * full_rmse_weights[,"train_rmse_tree"]) + (full_train_gbm_predict * full_rmse_weights[,"train_rmse_gbm"]) + (full_ridge_train_predict * full_rmse_weights[,"ridge_train_rmse"]) + (full_lasso_train_predict * full_rmse_weights[,"lasso_train_rmse"])
  )
) ```
```{r}
full_train_prediction_model<- lm(full_train_actuals ~ full_var_step_train_predict * full_var_back_train_predict * full_predict_rf_train * full_train_gbm_predict * full_ridge_train_predict * full_lasso_train_predict , data = full_train_diabetes)

full_train_diabetes$full_model_based<- predict( full_train_prediction_model,
                                                newdata = full_train_diabetes
)
sqrt(mean((full_train_diabetes$full_model_based - full_train_diabetes$full_train_actuals) ^ 2)) ```
```{r}
#full equal weights predictions full_test_diabetes
full_test_diabetes<- full_test_diabetes %>% mutate (
  full_equal_weight_pred = (full_var_step_test_predict + full_var_back_test_predict + full_predict_rf_test + full_test_gbm_predict + full_ridge_test_predict + full_lasso_test_predict)/6)
#full fit based predictions testing full_test_diabetes <- full_test_diabetes %>%
mutate( full_fit_test_predictions = (
  (full_var_step_test_predict * full_rmse_weights[,"step_train_rmse"]) + (full_var_back_test_predict * full_rmse_weights[,"train_rmse_back"]) + (full_predict_rf_test * full_rmse_weights[,"train_rmse_tree"]) + (full_test_gbm_predict * full_rmse_weights[,"train_rmse_gbm"]) + (full_ridge_test_predict * full_rmse_weights[,"ridge_train_rmse"]) + (full_lasso_test_predict * full_rmse_weights[,"lasso_train_rmse"])
) )
#create a temp duplicate df tof the test set with the same column names as the inputs of the model
full_test_diabetes_temp<- full_test_diabetes
colnames(full_test_diabetes_temp)<- c("full_var_step_train_predict", "full_var_back_train_predict", "full_predict_rf_train", "full_train_gbm_predict", "full_ridge_train_predict", "full_lasso_train_predict")
full_test_diabetes$full_model_test_pred<- predict(full_train_prediction_model,
                                                  
                                                  full_test_diabetes_temp)
sqrt(mean((full_test_diabetes$full_test_actuals- full_test_diabetes$full_model_test_pred) ^ 2)) ```
```{r}
full_equal_weight_rmse<- sqrt(mean((full_test_diabetes$full_equal_weight_pred - full_test_diabetes$full_test_actual) ^ 2))
full_equal_weight_rmse
full_fit_weight_rmse<- sqrt(mean((full_test_diabetes$full_fit_test_predictions - full_test_diabetes$full_test_actuals) ^ 2))
full_fit_weight_rmse
full_model_based_predict<- as.data.frame(predict(full_train_prediction_model, newx = full_test_diabetes))
colnames(full_model_based_predict)[1]<- "predictions"
full_model_weight_rmse<- sqrt(mean((full_test_diabetes$full_model_test_pred - full_test_diabetes$full_test_actuals) ^ 2))
full_model_weight_rmse
```
```{r}
full_model_test_temp1<-cbind( test_rmse_tree
                              ,test_rmse_gbm ,ridge_test_rmse ,lasso_test_rmse)
full_model_train_temp1<-cbind( train_rmse_tree ,train_rmse_gbm ,ridge_train_rmse ,lasso_train_rmse)

###train predictions
full_train_diabetes_temp1<- as.data.frame(bind_cols(predict_rf_train, train_gbm, ridge_train_predict, lasso_train_predict, y_train))
colnames(full_train_diabetes_temp1)<- c("full_predict_rf_train", "full_train_gbm_predict", "full_ridge_train_predict","full_lasso_train_predict","full_train_actuals")
#test predictions
full_test_diabetes_temp1<- bind_cols(test_tree, test_gbm, ridge_test_predict, lasso_test_predict, y_test)
colnames(full_test_diabetes_temp1)<- c("full_predict_rf_test", "full_test_gbm_predict", "full_ridge_test_predict","full_lasso_test_predict", "full_test_actuals")
```
```{r}
full_train_diabetes_temp1<- full_train_diabetes_temp1 %>% mutate(full_equal_weights = (predict_rf_train + train_gbm + ridge_train_predict +
                                                                                         lasso_train_predict) / 4)
```
```{r}
full_model_train_temp1
#take the reciprocal since the smaller the rmse the better full_rmse_weights_temp1<- (1/ full_model_train_temp1)
#calculate the weights of each of the model as a proportion of all four models full_rmse_weights_temp1<- full_rmse_weights_temp1/sum(full_rmse_weights_temp1)
full_train_diabetes_temp1 <- full_train_diabetes_temp1 %>% mutate(
  full_fit_predictions_temp1 = (
    (full_predict_rf_train * full_rmse_weights_temp1[,"train_rmse_tree"]) + (full_train_gbm_predict * full_rmse_weights_temp1[,"train_rmse_gbm"]) + (full_ridge_train_predict * full_rmse_weights_temp1[,"ridge_train_rmse"]) + (full_lasso_train_predict * full_rmse_weights_temp1[,"lasso_train_rmse"])
    
  ) )
```
```{r}
full_train_prediction_model_temp1<- lm(full_train_actuals ~ full_predict_rf_train * full_train_gbm_predict * full_ridge_train_predict * full_lasso_train_predict , data = full_train_diabetes_temp1)
full_train_diabetes_temp1$full_model_based<- predict( full_train_prediction_model_temp1,
                                                      newdata = full_train_diabetes_temp1
)
sqrt(mean((full_train_diabetes_temp1$full_model_based - full_train_diabetes_temp1$full_train_actuals) ^ 2))
```
Fit the model averaged models to the testing dataset
```{r}
#full equal weights predictions full_test_diabetes_temp1
full_test_diabetes_temp1<- full_test_diabetes_temp1 %>% mutate (
  full_equal_weight_pred = (full_predict_rf_test + full_test_gbm_predict + full_ridge_test_predict + full_lasso_test_predict)/4)
#full fit based predictions testing
full_test_diabetes_temp1 <- full_test_diabetes_temp1 %>%
  mutate( full_fit_test_predictions = (
    (full_predict_rf_test * full_rmse_weights_temp1[,"train_rmse_tree"]) + (full_test_gbm_predict * full_rmse_weights_temp1[,"train_rmse_gbm"]) + (full_ridge_test_predict * full_rmse_weights_temp1[,"ridge_train_rmse"]) + (full_lasso_test_predict * full_rmse_weights_temp1[,"lasso_train_rmse"])
    
  ) )
#create a temp duplicate df tof the test set with the same column names as the inputs of the model
full_test_diabetes_temp2<- full_test_diabetes_temp1 colnames(full_test_diabetes_temp2)<- c( "full_predict_rf_train", "full_train_gbm_predict", "full_ridge_train_predict", "full_lasso_train_predict")
full_test_diabetes_temp1$full_model_test_pred<- predict(full_train_prediction_model_temp1, full_test_diabetes_temp2)
sqrt(mean((full_test_diabetes_temp1$full_test_actuals- full_test_diabetes_temp1$full_model_test_pred) ^ 2))
```
```{r}
rmse<- function (model, data) {
  x <- residuals(model, data)
  sqrt(mean(x^2, na.rm = TRUE)) }
full_equal_weight_rmse_temp1<- sqrt(mean((full_test_diabetes_temp1$full_equal_weight_pred - full_test_diabetes_temp1$full_test_actual) ^ 2))
full_equal_weight_rmse_temp1
full_fit_weight_rmse_temp1<- sqrt(mean((full_test_diabetes_temp1$full_fit_test_predictions - full_test_diabetes_temp1$full_test_actuals) ^ 2))
full_fit_weight_rmse_temp1
full_model_based_predict_temp1<- as.data.frame(predict(full_train_prediction_model_temp1, newx = full_test_diabetes_temp2))
colnames(full_model_based_predict_temp1)[1]<- "predictions"
full_model_weight_rmse<- sqrt(mean((full_test_diabetes_temp1$full_model_test_pred - full_test_diabetes_temp1$full_test_actuals) ^ 2))
full_model_weight_rmse
```

```{r}
final_averaged_test_rmse<- cbind(full_equal_weight_rmse_temp1, full_fit_weight_rmse_temp1, full_model_weight_rmse)
```
