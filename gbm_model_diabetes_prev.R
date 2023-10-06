
# gbm model

remove.packages(c("tidyr", "mice", "broom")) detach("package:mice", unload=TRUE) detach("package:broom", unload=TRUE) detach("package:tidyr", unload=TRUE) install.packages("tidyr")
install.packages("gbm") library(gbm)
require(tidyr) ```
```{r} set.seed(271)
gbm_model_1<- gbm(diabetes_prevelance ~ ., distribution = "gaussian",
                  data = train_set_full, n.trees = 10000, interaction.depth = 1, shrinkage = .001, cv.folds = 5,
                  n.cores = NULL, verbose = FALSE)
print(gbm_model_1)

gbm.perf(gbm_model_1, method = "cv")
gbm_control <- trainControl(method="repeatedcv", number=50, repeats=50)
gmb_model<- train(diabetes_prevelance ~ ., data=train_set_full, trControl = gbm_control ,method="gbm",verbose=F)
gmb_model$results[which.max(gmb_model$result[,4]),]
summary(gmb_model)
#get the final RMSE and the final R-squared gmb_model$results$Rsquared
train_gbm<- as.data.frame(predict(gmb_model, train_set))
train_gbm<- cbind(row.names(gmb_model$trainingData), train_gbm) row.names(train_gbm)<- train_gbm$`row.names(gmb_model$trainingData)` train_gbm$`row.names(gmb_model$trainingData)`<- NULL colnames(train_gbm)[1]<- "prediction"
test_gbm<- as.data.frame(predict(gmb_model, test_set)) test_gbm<- cbind(row.names(test_set), test_gbm) row.names(test_gbm)<- test_gbm$`row.names(test_set)` test_gbm$`row.names(test_set)`<- NULL colnames(test_gbm)[1]<- "prediction"
test_rmse_gbm<- rmse(test_set_full$diabetes_prevelance, test_gbm$prediction) train_rmse_gbm<- rmse(train_set_full$diabetes_prevelance, train_gbm$prediction)
gbm_var_imp<- varImp(gmb_model)
gbm_var_imp<- gbm_var_imp$importance gbm_var_imp<-data.frame(overall = gbm_var_imp$Overall, variable_name = rownames(gbm_var_imp))
gbm_var_imp<- gbm_var_imp[order(gbm_var_imp$overall, decreasing = T),] gbm_var_imp
var_gbm<- subset(gbm_var_imp ,gbm_var_imp$overall > 0) View(var_gbm)

#write.csv(gbm_var_imp, "gbm_vars.csv")
#######COMBINE THE TRAINING AND TESTING SETS WITH THE CLUSTERS DFS## train_gbm_1<- merge(train_gbm, cluster_1_set, by = 0)
train_gbm_1_rmse<- rmse(train_gbm_1$diabetes_prevelance, train_gbm_1$prediction)
train_gbm_2<- merge(train_gbm, cluster_2_set, by = 0)
train_gbm_2_rmse<- rmse(train_gbm_2$diabetes_prevelance, train_gbm_2$prediction)
train_gbm_3<- merge(train_gbm, cluster_3_set, by = 0)
train_gbm_3_rmse<- rmse(train_gbm_3$diabetes_prevelance, train_gbm_3$prediction)
train_gbm_4<- merge(train_gbm, cluster_4_set, by = 0)
train_gbm_4_rmse<- rmse(train_gbm_4$diabetes_prevelance, train_gbm_4$prediction)
train_gbm_5<- merge(train_gbm, cluster_5_set, by = 0)
train_gbm_5_rmse<- rmse(train_gbm_5$diabetes_prevelance, train_gbm_5$prediction)
train_gbm_6<- merge(train_gbm, cluster_6_set, by = 0)
train_gbm_6_rmse<- rmse(train_gbm_6$diabetes_prevelance, train_gbm_6$prediction)
train_gbm_7<- merge(train_gbm, cluster_7_set, by = 0)
train_gbm_7_rmse<- rmse(train_gbm_7$diabetes_prevelance, train_gbm_7$prediction)
train_gbm_cluster_rmse<- as.data.frame(c(train_gbm_1_rmse, train_gbm_2_rmse, train_gbm_3_rmse, train_gbm_4_rmse, train_gbm_5_rmse, train_gbm_6_rmse, train_gbm_7_rmse))
colnames(train_gbm_cluster_rmse)[1]<- "gbm_train_rmse"
#######TEST RMSE#####
test_gbm_1<- merge(test_gbm, cluster_1_set, by = 0)
test_gbm_1_rmse<- rmse(test_gbm_1$diabetes_prevelance, test_gbm_1$prediction)

test_gbm_2<- merge(test_gbm, cluster_2_set, by = 0)
test_gbm_2_rmse<- rmse(test_gbm_2$diabetes_prevelance, test_gbm_2$prediction)
test_gbm_3<- merge(test_gbm, cluster_3_set, by = 0)
test_gbm_3_rmse<- rmse(test_gbm_3$diabetes_prevelance, test_gbm_3$prediction)
test_gbm_4<- merge(test_gbm, cluster_4_set, by = 0)
test_gbm_4_rmse<- rmse(test_gbm_4$diabetes_prevelance, test_gbm_4$prediction)
test_gbm_5<- merge(test_gbm, cluster_5_set, by = 0)
test_gbm_5_rmse<- rmse(test_gbm_5$diabetes_prevelance, test_gbm_5$prediction)
test_gbm_6<- merge(test_gbm, cluster_6_set, by = 0)
test_gbm_6_rmse<- rmse(test_gbm_6$diabetes_prevelance, test_gbm_6$prediction)
test_gbm_7<- merge(test_gbm, cluster_7_set, by = 0)
test_gbm_7_rmse<- rmse(test_gbm_7$diabetes_prevelance, test_gbm_7$prediction)
test_gbm_cluster_rmse<- as.data.frame(c(test_gbm_1_rmse, test_gbm_2_rmse, test_gbm_3_rmse, test_gbm_4_rmse, test_gbm_5_rmse, test_gbm_6_rmse, test_gbm_7_rmse)) colnames(test_gbm_cluster_rmse)[1]<- "gbm_test_rmse"