
#random forest tuning and model

set.seed(766)
rf_model<- randomForest::randomForest(diabetes_prevelance ~., data = train_set_full, ntree = 1000, type = "regression", localImp = TRUE)
print(rf_model)
summary(rf_model) plot(rf_model)
#######WHAT ARE THE MOST SIGNIFICANT VARIABLES############
tree_var_imp<- as.data.frame(varImp(rf_model)) tree_var_imp<-data.frame(overall = tree_var_imp$Overall, variable_name = rownames(tree_var_imp))
tree_var_imp<- tree_var_imp[order(tree_var_imp$overall, decreasing = T),] tree_var_imp<- subset(tree_var_imp, tree_var_imp$overall >=1) tree_var_imp
varUsed(rf_model, count = FALSE)
###P-Values
tree_p_values<- as.data.frame(measure_importance(rf_model)) tree_p_values<- subset(tree_p_values, tree_p_values$p_value <= .05) row.names(tree_p_values)<- tree_p_values$variable tree_p_values$variable<- NULL
View(tree_p_values)
#######TRAINING SET AND TEST SET PREDICTIONS##############
predict_rf_test<- predict(rf_model, test_set) predict_rf_train<- predict(rf_model, train_set)
train_rmse_tree<- rmse(train_set_full$diabetes_prevelance, predict_rf_train) test_rmse_tree<- rmse(test_set_full$diabetes_prevelance, predict_rf_test)
varImpPlot(rf_model) rf_model$rsq

############################################################## train_tree<-as.data.frame(predict(rf_model, train_set)) colnames(train_tree)[1]<- "prediction" test_tree<-as.data.frame(predict(rf_model, test_set)) colnames(test_tree)[1]<- "prediction"
#######COMBINE THE TRAINING AND TESTING SETS WITH THE CLUSTERS DFS## train_tree_1<- merge(train_tree, cluster_1_set, by = 0)
train_tree_1_rmse<- rmse(train_tree_1$diabetes_prevelance, train_tree_1$prediction)
train_tree_2<- merge(train_tree, cluster_2_set, by = 0)
train_tree_2_rmse<- rmse(train_tree_2$diabetes_prevelance, train_tree_2$prediction)
train_tree_3<- merge(train_tree, cluster_3_set, by = 0)
train_tree_3_rmse<- rmse(train_tree_3$diabetes_prevelance, train_tree_3$prediction)
train_tree_4<- merge(train_tree, cluster_4_set, by = 0)
train_tree_4_rmse<- rmse(train_tree_4$diabetes_prevelance, train_tree_4$prediction)
train_tree_5<- merge(train_tree, cluster_5_set, by = 0)
train_tree_5_rmse<- rmse(train_tree_5$diabetes_prevelance, train_tree_5$prediction)
train_tree_6<- merge(train_tree, cluster_6_set, by = 0)
train_tree_6_rmse<- rmse(train_tree_6$diabetes_prevelance, train_tree_6$prediction)
train_tree_7<- merge(train_tree, cluster_7_set, by = 0)
train_tree_7_rmse<- rmse(train_tree_7$diabetes_prevelance, train_tree_7$prediction)
train_tree_cluster_rmse<- as.data.frame(c(train_tree_1_rmse, train_tree_2_rmse, train_tree_3_rmse, train_tree_4_rmse, train_tree_5_rmse, train_tree_6_rmse, train_tree_7_rmse))
colnames(train_tree_cluster_rmse)[1]<- "tree_train_rmse"
#######TEST RMSE#####
test_tree_1<- merge(test_tree, cluster_1_set, by = 0)
test_tree_1_rmse<- rmse(test_tree_1$diabetes_prevelance, test_tree_1$prediction)
test_tree_2<- merge(test_tree, cluster_2_set, by = 0)
test_tree_2_rmse<- rmse(test_tree_2$diabetes_prevelance, test_tree_2$prediction)
test_tree_3<- merge(test_tree, cluster_3_set, by = 0)
test_tree_3_rmse<- rmse(test_tree_3$diabetes_prevelance, test_tree_3$prediction)
test_tree_4<- merge(test_tree, cluster_4_set, by = 0)

test_tree_4_rmse<- rmse(test_tree_4$diabetes_prevelance, test_tree_4$prediction)
test_tree_5<- merge(test_tree, cluster_5_set, by = 0)
test_tree_5_rmse<- rmse(test_tree_5$diabetes_prevelance, test_tree_5$prediction)
test_tree_6<- merge(test_tree, cluster_6_set, by = 0)
test_tree_6_rmse<- rmse(test_tree_6$diabetes_prevelance, test_tree_6$prediction)
test_tree_7<- merge(test_tree, cluster_7_set, by = 0)
test_tree_7_rmse<- rmse(test_tree_7$diabetes_prevelance, test_tree_7$prediction)
test_tree_cluster_rmse<- as.data.frame(c(test_tree_1_rmse, test_tree_2_rmse, test_tree_3_rmse, test_tree_4_rmse, test_tree_5_rmse, test_tree_6_rmse, test_tree_7_rmse)) colnames(test_tree_cluster_rmse)[1]<- "tree_test_rmse"

#Variable Selection in Random Forests
install.packages("htmltools") library(htmltools)
install.packages("randomForestExplainer") library(randomForestExplainer)
options("scipen"=100)
rf_var_imp_df<- as.data.frame(measure_importance(rf_model))
plot_min_depth_distribution(min_depth_distribution(rf_model)) ```

var_imp_rf<-data.frame(varImp(rf_model, scale=T)["Overall"]) %>% dplyr::mutate(variable=rownames(.)) %>% dplyr::rename(importance_rf=Overall) %>% dplyr::arrange(-importance_rf) %>%
  dplyr::mutate(rank_rf=seq(1:nrow(.)))

temp_12<- varImp(gmb_model, scale=T)
var_imp_gbm<- as.data.frame(temp_12$importance) %>%
  dplyr::mutate(variable=rownames(.)) %>% dplyr::rename(importance_gbm=Overall) %>%
  
  dplyr::arrange(-importance_gbm) %>% dplyr::mutate(rank_gbm=seq(1:nrow(.)))
final_res=merge(var_imp_rf, var_imp_gbm, by="variable") final_res$rank_diff=final_res$rank_rf-final_res$rank_gbm final_res
