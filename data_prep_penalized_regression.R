#final model df

final_dataframe_scale<- final_dataframe
numeric_var_scale <- sapply(final_dataframe_scale, is.numeric) final_dataframe_scale[numeric_var_scale] <- lapply(final_dataframe_scale[numeric_var_scale], scale)
#creating training data set by selecting the output row values train_set_full_scale<-final_dataframe_scale[final_df_split,]

train_set_scale<- train_set_full_scale[, c(-59)]
#creating test data set by not selecting the output row values test_set_full_scale<-final_dataframe_scale[-final_df_split,] test_set_scale<- test_set_full_scale[, c(-59)]
#assign the x, and y of the training set
x_train_scale<- data.matrix(train_set_full_scale[, c(-59)]) y_train_scale<- train_set_full_scale$diabetes_prevelance
x_test_scale<- data.matrix(test_set_full_scale[, c(-59)]) y_test_scale<- test_set_full_scale$diabetes_prevelance