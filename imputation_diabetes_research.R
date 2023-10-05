
sum(is.na(nutrition_diabetes_merge))
# 40% of the data is missing sum(is.na(nutrition_diabetes_merge))/sum(!is.na(nutrition_diabetes_merge))
# count the number of missing values in each column colSums(is.na(nutrition_diabetes_merge))
# count the number of missing values in each column
countries_na<- as.data.frame(cbind(nutrition_diabetes_merge$country, as.data.frame(rowSums(is.na(nutrition_diabetes_merge)))))
# create a rounded percentage of the number of missing variables round((colSums(is.na(nutrition_diabetes_merge))/nrow(nutrition_diabetes_merge)) * 100) na_col_dist<- as.data.frame(round((colSums(is.na(nutrition_diabetes_merge))/nrow(nutrition_diabetes_merge) ) * 100)
)
colnames(na_col_dist)[1]<-"percent_missing"
# add the names of the column
na_col_dist<- cbind(column_names = rownames(na_col_dist), na_col_dist) rownames(na_col_dist)<- 1:nrow(na_col_dist)
# plotting the distribution of missing values
ggplot(data = na_col_dist, aes(rownames(na_col_dist),percent_missing)) +
  geom_bar(stat = "identity", aes(fill = "identity"), color = "purple", show.legend = FALSE) + ggtitle("Distribution of the Missing Values") +
  ylab("Percentage of Observations Missing")
# The number of variables with missing values nrow(subset(na_col_dist, na_col_dist$percent_missing == 0))
#looking at the number of missing variables at 30,40, and 60% na_more_30_col<- subset(na_col_dist, na_col_dist$percent_missing > 30) na_more_40_col<- subset(na_col_dist, na_col_dist$percent_missing > 40) na_more_50_col<- subset(na_col_dist, na_col_dist$percent_missing > 50) na_more_60_col<- subset(na_col_dist, na_col_dist$percent_missing > 60)
# plotting the distribution of columns with more than 50% missing values

na_50_col<- subset(na_col_dist, na_col_dist$percent_missing <= 50) na_more_50_col<- subset(na_col_dist, na_col_dist$percent_missing > 50)
# There are 49 columns that contain missing values over 50% nrow(na_more_50_col)
# Plotting the distribution of missing values with more than 50% missing values ggplot(data = na_more_50_col, aes(rownames(na_more_50_col),percent_missing)) +
geom_bar(stat = "identity", show.legend = FALSE) + ggtitle("Distribution of the Missing Values Greater than 50") + ylab("Percentage of Observations Missing")
# Remove all of the columns where the columns are just year columns
# The year columms are not giving any extra data and are just extraneous for the dataset. year_columns<- c(grep("year", names(nutrition_diabetes_merge))) nutrition_diabetes_merge <- nutrition_diabetes_merge[, -c(year_columns)]
#remove all columns related to the source of where the data was taken nutrition_diabetes_merge$Source_EBFtrend_2<- NULL nutrition_diabetes_merge$Source_EBFtrend_3<- NULL
#create a list of the column names that have more than 50% of inputs missing missing_50_columns<- c(na_more_50_col$column_names)
#remove the columns from the dataset that have more than 50% of inputs missing nutrition_diabetes_merge <- nutrition_diabetes_merge[, names(nutrition_diabetes_merge) %notin% missing_50_columns, drop = F]
ncol(nutrition_diabetes_merge)
```
```{r}
#rename the original dataframe nutrition_diabetes<- nutrition_diabetes_merge
```

```{r}
#make the country names into the row names rownames(nutrition_diabetes) <- nutrition_diabetes$country nutrition_diabetes[1]<- NULL
View(nutrition_diabetes) ```
```{r}
#understand the number of factors for all of the categorical variables
df_levels<- as.data.frame(apply(nutrition_diabetes, 2, function(x) length(unique(x)))) df_levels<<- cbind(column_names = rownames(df_levels), df_levels) rownames(df_levels)<- 1:as.numeric(nrow(df_levels))
colnames(df_levels)[2]<- "num_levels"
df_levels ```
```{r}
#set the seed so that all of the imputations can be replicated set.seed(27)
#create two dataframes: one containing the numeric variables and another containing the categorical variables
#numeric variable Data-frame
nutrition_numeric<- select_if(nutrition_diabetes, is.numeric)
#character variable Data-frame
nutrition_factor<- select_if(nutrition_diabetes, is.factor)
nutrition_factor$country<- rownames(nutrition_factor) nutrition_factor$country<- as.character(nutrition_factor$country)
factor_columns<- (as.numeric(ncol(nutrition_factor)))
nutrition_factor<- nutrition_factor[,c(1:factor_columns)]<-lapply(nutrition_factor[,c(1:factor_columns)], factor)
nutrition_factor$country<- as.character(nutrition_factor$country)

nutrition_factor<- as.data.frame(nutrition_factor)
rownames(nutrition_factor) <- nutrition_factor$country nutrition_factor$country<- NULL
View(nutrition_factor)
#remove the collinear columns
collinear_columns<- c("u5mr2012", "u5mr2013", "GDP_2", "GDP_3", "EBF_trend_3") nutrition_numeric<- nutrition_numeric[,!names(nutrition_numeric) %in% collinear_columns]
```
```{r}
set.seed(27)
nutrition_numeric_sans_prev<- nutrition_numeric[,-c(115)]
nutrition_impute_num<- miceRanger(nutrition_numeric_sans_prev, m = 1, valueSelector = "meanMatch", returnModels = TRUE)
nutrition_impute_num_data<- completeData(nutrition_impute_num)
nutrition_impute_num_data<- as.data.frame(nutrition_impute_num_data$Dataset_1)
nutrition_impute_num_data<- cbind(nutrition_impute_num_data, nutrition_numeric[,c(0,115)])
colnames(nutrition_impute_num_data)[115]<- "diabetes_prevelance"
#Time it takes to perform the imputation nutrition_impute_num$imputationTime
#models for each imputation
nutrition_impute_num_models<- nutrition_impute_num$finalModels ```
```{r}
#create a dataframe removing rows where rate of feamle education 5 is missing

set.seed(127)
fem_ed_impute<- randomForest::randomForest(nutrition_numeric_sans_prev$rate_femaleED_5 ~.,
                                           data = nutrition_numeric_sans_prev, ntree = 500,
                                           type = "regression",
                                           #mtry = 10,
                                           #nodesize = 5, importance = TRUE, na.action= na.roughfix)
                                           fem_predict<- as.data.frame(fem_ed_impute$predicted) #fem_predict_split<- fem_predict[-c(22,52,61,68,105,145,150),]
                                           #merge with the original prevalence
                                           fem_predict<- cbind(fem_predict, nutrition_numeric[,c(0,77)])
                                           colnames(fem_predict)[1]<- "predicted values" colnames(fem_predict)[2]<- "actual values"
                                           #create a dataset where the dependent variable is removed ed_impute_dependent<- nutrition_numeric_sans_prev[, -77] #predict(female_ed_impute, ed_impute_dependent)
                                           ``` ```{r}
                                           #combine the newly imputted numerical data with the factor data factor_impute_data<- cbind(nutrition_impute_num_data, row.names(nutrition_factor)) row.names(factor_impute_data) <- factor_impute_data$`row.names(nutrition_factor)` factor_impute_data$`row.names(nutrition_factor)`<- NULL
                                           factor_impute_data<- merge(factor_impute_data, nutrition_factor, by = 0)
                                           row.names(factor_impute_data)<- factor_impute_data$Row.names factor_impute_data$Row.names<- NULL
                                           ```
                                           
                                           ```{r}
                                           imputed_categorical_df<- cbind(stunt_imputed, weight_imputed, waste_imputed, anemia_imputed, iodine_imputed, feed_imputed, rtf_imputed, fort_imputed, hypertension_imputed, policy_imputed)
                                           ```
                                           ```{r}
                                           set.seed(27)
                                           nutrition_impute_char_cart<- mice(factor_impute_data, m = 2, method = "cart", nnet.MaxNWts = 3000, remove.collinear=FALSE)
                                           nutrition_impute_char_2<- complete(nutrition_impute_char_cart)
                                           ```
                                           ```{r}
                                           #replicate the cart imputation with factor variables set.seed(27)
                                           stunting_cart<- rpart(stunting_progress ~., data = factor_impute_data,
                                                                 cp = .002,
                                                                 method = "class", minsplit = 10, #na.action= na.rpart na.action = na.roughfix)
                                                                 #length(stunting_cart$where)
                                                                 rpart.plot(stunting_cart) #length(female_ed_cart$frame$var[female_ed_cart$frame$var=="<leaf>"])
                                                                 stunting_cart$frame$var == "leaf" rpart.rules(stunting_cart)
                                                                 
                                                                 terminal_nodes_list_factor<- c(row.names(as.data.frame(rpart.rules(stunting_cart))))
                                                                 #find the terminal node number for Andorra for (i in terminal_nodes_list_factor) {
                                                                 for (country in as.list(row.names(as.data.frame(subset.rpart(stunting_cart, factor_impute_data, i ))))) {
                                                                   i = as.numeric(i)
                                                                   if (country == "United Kingdom") {
                                                                     print (i) }
                                                                 } }
#take the output from above and plug the number below View(as.data.frame(subset.rpart(stunting_cart, factor_impute_data, 5)))
rpart.plot(stunting_cart)
``` ```{r}
nutrition_impute_num_data nutrition_impute_char_2
#imputed_df_temp<- merge(nutrition_impute_num_data, nutrition_impute_char_2, by = 0) ```
```{r}
corr_plot_num<-cor(nutrition_impute_num_data, use= "everything")
corrplot(corr_plot_num, method = "color") #cor.test(nutrition_impute_num_data, method = "kendall")
corr_plot_num<- as.data.frame(corr_plot_num) #View(corr_plot_num)

#take the absolute value of all of the correlation stats corr_plot_num<- abs(corr_plot_num)
perfect_corr_temp<- as.data.frame(which(corr_plot_num >= .95, arr.ind=T))
perfect_corr_temp<- subset(perfect_corr_temp, perfect_corr_temp$row != perfect_corr_temp$col)
perfect_corr_temp<- perfect_corr_temp[order(perfect_corr_temp$row),]
View(perfect_corr_temp)
# create mini correlation plots for the highly correlated variables
cor(nutrition_impute_num_data$totalpop2015, nutrition_impute_num_data$under5pop, use = "pairwise.complete.obs")
cor(nutrition_impute_num_data$under5pop, nutrition_impute_num_data$WRAanaemia_NUMBER, use = "pairwise.complete.obs")
cor(nutrition_impute_num_data$u5mr2009, nutrition_impute_num_data$u5mr2010, use = "pairwise.complete.obs") #0.9802605 remove u5mr2009
cor(nutrition_impute_num_data$u5mr2011, nutrition_impute_num_data$u5mr2010, use = "pairwise.complete.obs") #0.9850473 remove u5mr2010
cor(nutrition_impute_num_data$poverty2_3, nutrition_impute_num_data$poverty125_4, use = "pairwise.complete.obs") #0.6978783 no variable removal
cor(nutrition_impute_num_data$poverty2_4, nutrition_impute_num_data$poverty125_4, use = "pairwise.complete.obs") #0.8907929 no variable removal
cor(nutrition_impute_num_data$GDP_1, nutrition_impute_num_data$GDP_4, use = "pairwise.complete.obs") #0.9561021 no variable removal
cor(nutrition_impute_num_data$GDP_1, nutrition_impute_num_data$GDP_5, use = "pairwise.complete.obs") #0.9826582 remove GDP_1
cor(nutrition_impute_num_data$GDP_4, nutrition_impute_num_data$GDP_5, use = "pairwise.complete.obs") #0.9826582 remove GDP_1

cor(nutrition_impute_num_data$ow_female, nutrition_impute_num_data$ob_female, use = "pairwise.complete.obs") #0.9725733 remove ow_female
cor(nutrition_impute_num_data$prev_u5overweight, nutrition_impute_num_data$number_u5overweight, use = "pairwise.complete.obs" ) #0.04968424 no variable removal
cor(nutrition_impute_num_data$number_stunting_current, nutrition_impute_num_data$prev_stunting_current, use = "pairwise.complete.obs") #0.1493163 no variable removal
cor(nutrition_impute_num_data$prev_stunting_current, nutrition_impute_num_data$rate_stuntingtrend3, use = "pairwise.complete.obs") #0.6920061 no variable removal
cor(nutrition_impute_num_data$poverty125_3 , nutrition_impute_num_data$poverty125_4, use = "pairwise.complete.obs")
#0.655592 no variable removal
cor(nutrition_impute_num_data$poverty125_3 , nutrition_impute_num_data$poverty2_3, use = "pairwise.complete.obs")
cor(nutrition_impute_num_data$poverty125_3 , nutrition_impute_num_data$poverty2_4, use = "pairwise.complete.obs")
cor(nutrition_impute_num_data$poverty125_4 , nutrition_impute_num_data$poverty2_3, use = "pairwise.complete.obs")
cor(nutrition_impute_num_data$poverty125_4 , nutrition_impute_num_data$poverty2_4, use = "pairwise.complete.obs")
cor(nutrition_impute_num_data$rate_stuntingtrend3, nutrition_impute_num_data$rate_stuntingtrend4, use = "pairwise.complete.obs")
cor(nutrition_impute_num_data$GDP_1, nutrition_impute_num_data$GDP_4, use = "pairwise.complete.obs")
cor(nutrition_impute_num_data$GDP_4, nutrition_impute_num_data$GDP_5, use = "pairwise.complete.obs")

cor(nutrition_impute_num_data$prev_stunting_current, nutrition_impute_num_data$rate_stuntingtrend3, use = "pairwise.complete.obs")
cor(nutrition_impute_num_data$number_stunting_current, nutrition_impute_num_data$number_wasting, use = "pairwise.complete.obs")
cor(nutrition_impute_num_data$number_stunting_current, nutrition_impute_num_data$number_sev_wasting, use = "pairwise.complete.obs")
cor(nutrition_impute_num_data$rate_stuntingtrend3, nutrition_impute_num_data$prev_stunting_current, use = "pairwise.complete.obs")
cor(nutrition_impute_num_data$ow_male, nutrition_impute_num_data$ow_female, use = "pairwise.complete.obs")
cor(nutrition_impute_num_data$ow_bothsexes, nutrition_impute_num_data$ow_female, use = "pairwise.complete.obs")
#0.9438981 no variable removal
cor(nutrition_impute_num_data$ow_bothsexes, nutrition_impute_num_data$ow_male, use = "pairwise.complete.obs")
cor(nutrition_impute_num_data$totalpop2015, nutrition_impute_num_data$under5pop, use = "pairwise.complete.obs") #0.9588923 no variable removal
cor(nutrition_impute_num_data$BGboth, nutrition_impute_num_data$BGfemale, use = "pairwise.complete.obs") #0.9790776 remove BGboth
cor(nutrition_impute_num_data$BGboth, nutrition_impute_num_data$BGmale, use = "pairwise.complete.obs") #0.9610325 no variable removal
cor(nutrition_cor_temp$BPboth, nutrition_cor_temp$BPfemale, use = "pairwise.complete.obs") #0.9541184 no variable removal
cor(nutrition_impute_num_data$BPboth, nutrition_impute_num_data$BPmale, use = "pairwise.complete.obs") #0.9163336 no variable removal
cor(nutrition_impute_num_data$cholesterol_BOTH, nutrition_impute_num_data$cholesterol_MALE, use = "pairwise.complete.obs") #0.988534 remove cholesterol_BOTH
cor(nutrition_impute_num_data$fruitandveg_gram2010, nutrition_impute_num_data$fruitandveg_gram2011, use = "pairwise.complete.obs") #0.9767644

remove fruitandveg_gram2010
cor(nutrition_impute_num_data$rate_femaleED_1, nutrition_impute_num_data$rate_femaleED_2, use = "pairwise.complete.obs") #0.972859 remove rate_femaleED_1
cor(nutrition_impute_num_data$rate_femaleED_2, nutrition_impute_num_data$rate_femaleED_3, use = "pairwise.complete.obs") #0.9850756 remove rate_femaleED_2
cor(nutrition_impute_num_data$rate_femaleED_3, nutrition_impute_num_data$rate_femaleED_4, use = "pairwise.complete.obs") #0.9871316 remove rate_femaleED_3
cor(nutrition_impute_num_data$rate_femaleED_4, nutrition_impute_num_data$rate_femaleED_5, use = "pairwise.complete.obs") #0.9838261 remove rate_femaleED_4
cor(nutrition_impute_num_data$san_improved1990, nutrition_impute_num_data$san_improved2000, use = "pairwise.complete.obs") #0.9744573 remove san_improved1990
cor(nutrition_impute_num_data$san_opendef1990, nutrition_impute_num_data$san_opendef2000, use = "pairwise.complete.obs") # 0.9764377 remove san_opendef1990
cor(nutrition_impute_num_data$poverty125_3, nutrition_impute_num_data$poverty2_3, use = "pairwise.complete.obs") #0.9542385 no variable removal
cor(nutrition_impute_num_data$rate_stuntingtrend3, nutrition_impute_num_data$rate_stuntingtrend4, use = "pairwise.complete.obs")
cor(nutrition_impute_num_data$ob_bothsexes, nutrition_impute_num_data$ob_female, use = "pairwise.complete.obs")
cor(nutrition_impute_num_data$ob_bothsexes, nutrition_impute_num_data$ob_male, use = "pairwise.complete.obs")
cor(nutrition_impute_num_data$ob_female, nutrition_impute_num_data$ob_male, use = "pairwise.complete.obs")
cor(nutrition_impute_num_data$number_wasting, nutrition_impute_num_data$number_stunting_current, use = "pairwise.complete.obs")

cor(nutrition_impute_num_data$number_sev_wasting, nutrition_impute_num_data$number_stunting_current, use = "pairwise.complete.obs")
cor(nutrition_impute_num_data$gender_inequality_index, nutrition_impute_num_data$gender_inequality_rank, use = "pairwise.complete.obs")
#cor(nutrition_impute_num_data$, nutrition_impute_num_data$ob_male, use = "pairwise.complete.obs")
################################################### perfect_corr_temp<- as.data.frame(perfect_corr_temp)
###################################################### #attach(nutrition_cor_temp)
View(as.data.frame(cor(nutrition_impute_num_data[,c(1,2,5,6,12)], nutrition_impute_num_data[,c(2,40,6,7,13)]))) as.data.frame(cor(nutrition_impute_num_data[,c(13,17,25,33,34)], nutrition_impute_num_data[,c(14,25,26,35,36)])) as.data.frame(cor(nutrition_impute_num_data[,c(35,36,37,42,45)], nutrition_impute_num_data[,c(37,38,43,44,46)])) as.data.frame(cor(nutrition_impute_num_data[,c(48,49,63,65,68)], nutrition_impute_num_data[,c(47,49,50,64,66)])) View(as.data.frame(cor(nutrition_impute_num_data[,c(73,74,75,76,78)], nutrition_impute_num_data[,c(74,75,76,77,82)]))) as.data.frame(cor(nutrition_impute_num_data[,c(80,81,90,92,93)], nutrition_impute_num_data[,c(83,85,94,96,97)])) as.data.frame(cor(nutrition_impute_num_data[,c(94,95,97)], nutrition_impute_num_data[,c(96,97,98,99,101)]))
#detach(nutrition_cor_temp)
##########CORRELATION TESTS###############################
cor.test(nutrition_impute_num_data$totalpop2015 , nutrition_impute_num_data$under5pop, method = "pearson")
cor.test(nutrition_impute_num_data$under5pop , nutrition_impute_num_data$WRAanaemia_NUMBER, method = "pearson") cor.test(nutrition_impute_num_data$u5mr2009 , nutrition_impute_num_data$u5mr2010, method = "pearson")
cor.test(nutrition_impute_num_data$u5mr2009 , nutrition_impute_num_data$u5mr2011, method
         
         = "pearson")
cor.test(nutrition_impute_num_data$u5mr2009 , nutrition_impute_num_data$u5mr2010, method = "pearson")
cor.test(nutrition_impute_num_data$GDP_1 , nutrition_impute_num_data$GDP_4, method = "pearson")
cor.test(nutrition_impute_num_data$GDP_1 , nutrition_impute_num_data$GDP_5, method = "pearson")
cor.test(nutrition_impute_num_data$GDP_4 , nutrition_impute_num_data$GDP_5, method = "pearson")
cor.test(nutrition_impute_num_data$ow_male , nutrition_impute_num_data$ow_female, method = "pearson")
cor.test(nutrition_impute_num_data$ow_bothsexes , nutrition_impute_num_data$ow_female, method = "pearson")
cor.test(nutrition_impute_num_data$ow_bothsexes , nutrition_impute_num_data$ow_male, method = "pearson")
cor.test(nutrition_impute_num_data$ow_bothsexes , nutrition_impute_num_data$ow_female, method = "pearson")
cor.test(nutrition_impute_num_data$ow_bothsexes , nutrition_impute_num_data$ob_female, method = "pearson")
cor.test(nutrition_impute_num_data$ow_bothsexes , nutrition_impute_num_data$ob_male, method = "pearson")
cor.test(nutrition_impute_num_data$ow_bothsexes , nutrition_impute_num_data$ob_bothsexes, method = "pearson")
cor.test(nutrition_impute_num_data$ob_female , nutrition_impute_num_data$ob_male, method = "pearson")
cor.test(nutrition_impute_num_data$BGboth , nutrition_impute_num_data$BGmale, method = "pearson")
cor.test(nutrition_impute_num_data$BGboth , nutrition_impute_num_data$BGfemale, method = "pearson")
cor.test(nutrition_impute_num_data$BGmale , nutrition_impute_num_data$BGfemale, method = "pearson")
cor.test(nutrition_impute_num_data$BGmale , nutrition_impute_num_data$BPfemale, method =
           
           "pearson")
cor.test(nutrition_impute_num_data$BGmale , nutrition_impute_num_data$BPmale, method = "pearson")
cor.test(nutrition_impute_num_data$BGmale , nutrition_impute_num_data$BGboth, method = "pearson")
cor.test(nutrition_impute_num_data$BGmale , nutrition_impute_num_data$BPboth, method = "pearson")
cor.test(nutrition_impute_num_data$BPboth , nutrition_impute_num_data$BGmale, method = "pearson")
cor.test(nutrition_impute_num_data$BPboth , nutrition_impute_num_data$BGmale, method = "pearson")
cor.test(nutrition_impute_num_data$rate_stuntingtrend3 , nutrition_impute_num_data$rate_stuntingtrend4, method = "pearson")
cor.test(nutrition_impute_num_data$poverty125_3, nutrition_impute_num_data$poverty125_4, method = "pearson")
cor.test(nutrition_impute_num_data$poverty2_3, nutrition_impute_num_data$poverty2_4, method = "pearson")
cor.test(nutrition_impute_num_data$undernutritionrank_1to126, nutrition_impute_num_data$overnutritionrank_1to116, method = "pearson")
nutrition_cor_final_1<- nutrition_impute_num_data[, -c(2,5,6,12,13,33,34,36,37,43,44,47,49,50,63,65,73,74,75,76,78,80,90,94)]
nutrition_corr_final_2<- nutrition_cor_final_1[, -c(35,47,48,50,58,59,60,61,62,63,68,69,70,71,72,73,78:84)]
################################################################## final_corr_plot<-cor(nutrition_corr_final_2, use = "pairwise.complete.obs")
final_corr_temp<- as.data.frame(which(final_corr_plot >= .95, arr.ind=T)) final_corr_df<- subset(final_corr_temp, final_corr_temp$row != final_corr_temp$col)

final_corr_df<- final_corr_df[order(final_corr_df$row),]
View(final_corr_df) ################################################################# nutrition_corr_final_3<- nutrition_corr_final_2[,-c(5,6,7,11,12,20,28,49 )] nutrition_corr_final_3<- nutrition_corr_final_3[,-c(37)]
final_corr_plot_1<- cor(nutrition_corr_final_3, use = "pairwise.complete.obs")
final_corr_temp_1<- as.data.frame(which(final_corr_plot_1 >= .95, arr.ind=T))
final_corr_df_1<- subset(final_corr_temp_1, final_corr_temp_1$row != final_corr_temp_1$col)
final_corr_df_1<- final_corr_df_1[order(final_corr_df_1$row),]
#View(final_corr_df_1)
nutrition_corr_final<- nutrition_corr_final_3 row.names(nutrition_corr_final)<- nutrition_diabetes_merge$country
```
```{r}
#create the covariance matrix
covariance_matrix<- as.data.frame(cov(nutrition_corr_final)) covariance_matrix
#check if there are any variables that have a co-variance value of 0 meaning #there exists no linear relationship
sum(which(covariance_matrix == 0))
#look into how the different variables positively or negatively affect the #dependent variable
prev_covariance<- data.frame(covariance_matrix$diabetes_prevelance, row.names = row.names(covariance_matrix))
View(prev_covariance)

#Merge with the Correlation of the independent and the dependent variables
prev_corr<- as.data.frame(cor(nutrition_corr_final, nutrition_corr_final$diabetes_prevelance,use = "pairwise.complete.obs"))
cov_corr_df<- as.data.frame(cbind(prev_covariance, prev_corr)) colnames(cov_corr_df)[1]<- "diabetes_prevalence_covariance" colnames(cov_corr_df)[2]<- "diabetes_prevalence_correlation"
View(cov_corr_df) ```
```{r}
nutrition_impute_char_final<- select_if(nutrition_impute_char_2, is.factor)
temp_5<-cbind(nutrition_corr_final, nutrition_impute_char_2) #head(temp_5)