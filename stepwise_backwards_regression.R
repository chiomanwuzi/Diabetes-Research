
library(tidyverse) library(broom)
augment(full_model) augment(min_model) augment(step_regression) plot(step_regression)
set.seed(127)
full_model<- lm(diabetes_prevelance ~., data = train_set_full) summary(full_model)
min_model<- lm(diabetes_prevelance ~ 1, data = train_set_full) summary(min_model)
##############using the test set and the training set
step_regression<- step(min_model, threshold = 0.05, direction = "both", scope = formula(full_model))
#anova(for_regression)
step_summary<- summary(step_regression) step_summary #View(as.data.frame(step_summary$coefficients))
#faster way to find significant terms vars_step<-anova(step_regression) vars_step<- row.names(vars_step)
#first output is the AIC step_regression$anova
step_r_adj<- step_summary$adj.r.squared step_r_adj
#How many variables are in the final model? step_summary$terms
#TEST RMSE#

step_test_rmse<- rmse(test_set_full$diabetes_prevelance, predict(step_regression, test_set)) step_test_rmse
#AIC step_regression$anova
#TRAIN RMSE#
step_train_rmse<- rmse(train_set_full$diabetes_prevelance, predict(step_regression, train_set)) step_train_rmse
step_train<-as.data.frame(predict(step_regression, train_set)) colnames(step_train)[1]<- "prediction" step_test<-as.data.frame(predict(step_regression, test_set)) colnames(step_test)[1]<- "prediction"
####P-Values####
step_p_values<- as.data.frame(summary(step_regression)$coefficients[,4]) colnames(step_p_values)[1]<- "p-values"
##Significant p-values
step_p_values<- subset(step_p_values, step_p_values$`p-values` <= .05) View(step_p_values)
##Coefficients
options(scipen=999)
step_p_coeff<- as.data.frame(summary(step_regression)$coefficients[,1]) colnames(step_p_coeff)[1]<- "Variable Coefficients"
#RANK THE MOST IMPORTANT VARIABLES IN ORDER OF IMPORTANCE# step_var_imp<- as.data.frame(varImp(step_regression)) step_var_imp<-data.frame(overall = step_var_imp$Overall, variable_name = rownames(step_var_imp))
step_var_imp<- step_var_imp[order(step_var_imp$overall, decreasing = T),] colnames(step_var_imp)[1]<- "absolute t-stat"
step_var_imp
temp_13 <- names(unlist(step_regression[[1]])) # get the shortlisted variable. temp_13[!temp_13 %in% "(Intercept)"] # remove intercept
##### TRAIN RMSE BY THE CLUSTERS###

step_1_train<- merge(step_train, cluster_1_set, by = 0)
train_step_1_rmse<- rmse(step_1_train$diabetes_prevelance, step_1_train$prediction)
step_2_train<- merge(step_train, cluster_2_set, by = 0) train_step_2_rmse<-rmse(step_2_train$diabetes_prevelance, step_2_train$prediction)
step_3_train<- merge(step_train, cluster_3_set, by = 0) train_step_3_rmse<-rmse(step_3_train$diabetes_prevelance, step_3_train$prediction)
step_4_train<- merge(step_train, cluster_4_set, by = 0) train_step_4_rmse<-rmse(step_4_train$diabetes_prevelance, step_4_train$prediction)
step_5_train<- merge(step_train, cluster_5_set, by = 0) train_step_5_rmse<-rmse(step_5_train$diabetes_prevelance, step_5_train$prediction)
step_6_train<- merge(step_train, cluster_6_set, by = 0) train_step_6_rmse<-rmse(step_6_train$diabetes_prevelance, step_6_train$prediction)
step_7_train<- merge(step_train, cluster_7_set, by = 0) train_step_7_rmse<-rmse(step_7_train$diabetes_prevelance, step_7_train$prediction)
step_cluster_train_rmse<- as.data.frame(c(train_step_1_rmse,train_step_2_rmse,train_step_3_rmse,train_step_4_rmse,train _step_5_rmse,train_step_6_rmse,train_step_7_rmse ))
colnames(step_cluster_train_rmse)[1]<- "step_train_rmse"
###### TEST RMSE BY THE CLUSTERS ###
step_1_test<- merge(step_test, cluster_1_set, by = 0) test_step_1_rmse<-rmse(step_1_test$diabetes_prevelance, step_1_test$prediction)
step_2_test<- merge(step_test, cluster_2_set, by = 0) test_step_2_rmse<-rmse(step_2_test$diabetes_prevelance, step_2_test$prediction)
step_3_test<- merge(step_test, cluster_3_set, by = 0) test_step_3_rmse<-rmse(step_3_test$diabetes_prevelance, step_3_test$prediction)
step_4_test<- merge(step_test, cluster_4_set, by = 0) test_step_4_rmse<-rmse(step_4_test$diabetes_prevelance, step_4_test$prediction)
step_5_test<- merge(step_test, cluster_5_set, by = 0) test_step_5_rmse<-rmse(step_5_test$diabetes_prevelance, step_5_test$prediction)
step_6_test<- merge(step_test, cluster_6_set, by = 0)

test_step_6_rmse<-rmse(step_6_test$diabetes_prevelance, step_6_test$prediction)
step_7_test<- merge(step_test, cluster_7_set, by = 0) test_step_7_rmse<-rmse(step_7_test$diabetes_prevelance, step_7_test$prediction)
step_cluster_test_rmse<- as.data.frame(c(test_step_1_rmse,test_step_2_rmse,test_step_3_rmse,test_step_4_rmse,test_step _5_rmse,test_step_6_rmse,test_step_7_rmse ))
colnames(step_cluster_test_rmse)[1]<- "step_test_rmse"
step_cluster_test_rmse
#####################################################
```
```{r}
#Backwards Regression #############################################
######
back_regression<- step(full_model, threshold = 0.05, direction = "back", scope = formula(min_model))
#anova(back_regression)
options(scipen=0)
back_regress_summary<- summary(back_regression) View(back_regress_summary)
#######ADJUSTED R SQUARED######## back_R_adj<-back_regress_summary$adj.r.squared back_R_adj #.9071694
#faster way to find significant terms anova(back_regression)
#AIC back_regression$anova
####P-Values####
back_p_values<- as.data.frame(summary(back_regression)$coefficients[,4]) colnames(back_p_values)[1]<- "p-values"
##Significant p-values
back_p_values<- subset(back_p_values, back_p_values$`p-values` <= .05) View(back_p_values)
############NUMBER OF VARIABLES IN THE FINAL MODEL##########

back_regress_summary$terms
#predictions using coefficients of backwards regression predict(back_regression)
back_test_predictions<- predict(back_regression, test_set) back_train_predictions<- predict(back_regression, train_set)
#TEST RMSE#
test_rmse_back<- rmse(test_set_full$diabetes_prevelance, predict(back_regression, test_set)) test_rmse_back
#TRAIN RMSE#
train_rmse_back<- rmse(train_set_full$diabetes_prevelance, predict(back_regression, train_set)) train_rmse_back
#1.583406
#find the number of variables vars_back<-anova(back_regression) vars_back<- row.names(vars_back)
##Coefficients
options(scipen=999)
back_coeff<- as.data.frame(summary(back_regression)$coefficients[,1]) colnames(back_coeff)[1]<- "Variable Coefficients"
#list the variables with the highest relative importance
back_var_imp<- as.data.frame(varImp(back_regression)) back_var_imp<-data.frame(overall = back_var_imp$Overall, variable_name = rownames(back_var_imp))
back_var_imp<- back_var_imp[order(back_var_imp$overall, decreasing = T),] colnames(back_var_imp)[1]<- "t-test stat"
View(back_var_imp)
###########COMBINE THE PREDICTIONS AND THE ACTUALS############ back_train<-as.data.frame(predict(back_regression, train_set)) colnames(back_train)[1]<- "prediction"
back_test<-as.data.frame(predict(back_regression, test_set)) colnames(back_test)[1]<- "prediction"
###########TRAIN##########
back_1_train<- merge(back_train, cluster_1_set, by = 0)

back_2_train<- merge(back_train, cluster_2_set, by = 0) back_3_train<- merge(back_train, cluster_3_set, by = 0) back_4_train<- merge(back_train, cluster_4_set, by = 0) back_5_train<- merge(back_train, cluster_5_set, by = 0) back_6_train<- merge(back_train, cluster_6_set, by = 0) back_7_train<- merge(back_train, cluster_7_set, by = 0)
back_1_train_rmse<- rmse(back_1_train$diabetes_prevelance, back_1_train$prediction) back_2_train_rmse<- rmse(back_2_train$diabetes_prevelance, back_2_train$prediction) back_3_train_rmse<- rmse(back_3_train$diabetes_prevelance, back_3_train$prediction) back_4_train_rmse<- rmse(back_4_train$diabetes_prevelance, back_4_train$prediction) back_5_train_rmse<- rmse(back_5_train$diabetes_prevelance, back_5_train$prediction) back_6_train_rmse<- rmse(back_6_train$diabetes_prevelance, back_6_train$prediction) back_7_train_rmse<- rmse(back_7_train$diabetes_prevelance, back_7_train$prediction)
back_cluster_train_rmse<- as.data.frame(c(back_1_train_rmse, back_2_train_rmse, back_3_train_rmse, back_4_train_rmse, back_5_train_rmse,back_6_train_rmse, back_7_train_rmse))
colnames(back_cluster_train_rmse)[1]<- "back_train_rmse"
############TEST#############
back_1_test<- merge(back_test, cluster_1_set, by = 0) back_2_test<- merge(back_test, cluster_2_set, by = 0) back_3_test<- merge(back_test, cluster_3_set, by = 0) back_4_test<- merge(back_test, cluster_4_set, by = 0) back_5_test<- merge(back_test, cluster_5_set, by = 0) back_6_test<- merge(back_test, cluster_6_set, by = 0) back_7_test<- merge(back_test, cluster_7_set, by = 0)
back_1_test_rmse<- rmse(back_1_test$diabetes_prevelance, back_1_test$prediction) back_2_test_rmse<- rmse(back_2_test$diabetes_prevelance, back_2_test$prediction) back_3_test_rmse<- rmse(back_3_test$diabetes_prevelance, back_3_test$prediction) back_4_test_rmse<- rmse(back_4_test$diabetes_prevelance, back_4_test$prediction) back_5_test_rmse<- rmse(back_5_test$diabetes_prevelance, back_5_test$prediction) back_6_test_rmse<- rmse(back_6_test$diabetes_prevelance, back_6_test$prediction) back_7_test_rmse<- rmse(back_7_test$diabetes_prevelance, back_7_test$prediction)
back_cluster_test_rmse<- as.data.frame(c(back_1_test_rmse, back_2_test_rmse, back_3_test_rmse, back_4_test_rmse, back_5_test_rmse,back_6_test_rmse, back_7_test_rmse)) colnames(back_cluster_test_rmse)[1]<- "back_test_rmse"
