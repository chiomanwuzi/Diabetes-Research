
# clustering algorithm

set.seed(271) within_ss<- function(x){
  kmeans(nutrition_corr_final[, c(-59)], x, n= 100)$tot.withinss }
num_clusters<- 2:15
wss_values<- map_dbl(num_clusters, within_ss)
plot(num_clusters, wss_values,
     type = "b", pch = 19, frame = FALSE, main= "K Means Elbow Plot",
     xlab = "Number of Clusters",
     ylab = "Total Within Sum of Squares")
kmeans_cluster<- kmeans(nutrition_corr_final[, c(-59)], centers = 7 , nstart = 300) kmeans_cluster
kmeans_cluster_df<- as.data.frame(kmeans_cluster$cluster) colnames(kmeans_cluster_df)[1]<- "cluster"

kmeans_df<-merge(kmeans_cluster_df, nutrition_corr_final,by = 0)
library(factoextra) library(NbClust)
#install.packages("broom") library(broom)
#fviz_nbclust(nutrition_corr_final[, c(-59)], kmeans, method = "gap_stat")
#Compare Clusters Recursively cluster_comparison<- clValid(
nutrition_corr_final[, -59], nClust = 5:20,
clMethods = c("kmeans"), method = "single"
)
cluster_summary<- summary(cluster_comparison)
#create a dataframe of only the cluster and diabetes prevalence kmeans_df<- kmeans_df[,-c(3:60)]
kmeans_df
rownames(kmeans_df) <- kmeans_df$Row.names kmeans_df[1]<- NULL
#find the distribution of countries in each cluster table(kmeans_df$cluster)
#look for the countries in a specific cluster subset(kmeans_df, kmeans_df$cluster == 3)
#AVERGAE DIABETES PREVELANCE
kmeans_grouping_avg<- aggregate(diabetes_prevelance ~ cluster , kmeans_df, mean) kmeans_grouping_avg
kmeans_grouping_min<- aggregate(diabetes_prevelance ~ cluster , kmeans_df, min) kmeans_grouping_min
kmeans_grouping_max<- aggregate(diabetes_prevelance ~ cluster , kmeans_df, max) kmeans_grouping_max
kmeans_grouping_range<- merge(kmeans_grouping_min, kmeans_grouping_max, by = 0)
colnames(kmeans_grouping_range)[3]<- "min_prevalence" colnames(kmeans_grouping_range)[5]<- "max_prevalence"

#[Add how the cluster distributions look with the number of clusters] kmeans_cluster$centers
##############K Means Analysis #merge the country names with the clusters
temp_5<- cbind(nutrition_diabetes_merge$country, temp_5)
kmeans_cluster_df<- cbind(nutrition_diabetes_merge$country, kmeans_cluster_df) temp_5$`nutrition_diabetes_merge$country`<- NULL
row.names(kmeans_cluster_df)<- kmeans_cluster_df$`nutrition_diabetes_merge$country` kmeans_cluster_df$`nutrition_diabetes_merge$country`<- NULL
temp_6<- as.data.frame(cbind(temp_5, kmeans_cluster_df)) regional_columns<- select_if(nutrition_diabetes, is.character) temp_6<- merge(temp_6, regional_columns, by = 0)
temp_6 <- temp_6[, !duplicated(colnames(temp_6))] table(temp_6$cluster, temp_6$continent)
table(temp_6$cluster, temp_6$stunting_progress)
#find quantiles by group for variables
quants <- c(0.25, 0.5, 0.75)
quant_names <- map_chr(quants, ~paste0(.x*100, "%"))
quant_calc <- map(quants, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% set_names(nm = quant_names)
quantiles_pop<- temp_6 %>% group_by(temp_6$cluster) %>% summarize_at(vars(totalpop2015), funs(!!!quant_calc))
quantiles_edu_spend<- temp_6 %>%
  group_by(temp_6$cluster) %>% summarize_at(vars(toteducation_ppp2010), funs(!!!quant_calc))
quantiles_health_spend<- temp_6 %>% group_by(temp_6$cluster) %>% summarize_at(vars(tothealth_ppp2010), funs(!!!quant_calc))

quantiles_sp_spend<- temp_6 %>% group_by(temp_6$cluster) %>% summarize_at(vars(totsp_ppp2010), funs(!!!quant_calc))
quantiles_gdp<- temp_6 %>% group_by(temp_6$cluster) %>% summarize_at(vars(GDP_5), funs(!!!quant_calc))
quantiles_wasting<- temp_6 %>% group_by(temp_6$cluster) %>% summarize_at(vars(number_wasting), funs(!!!quant_calc))
#####ANALYZE THE CLUSTERS######
cluster_summary<- temp_6 %>% split(.$cluster) %>% map(summary)
describeBy(temp_6[, -59], temp_6[,59])
pseudoF = function(X, k, ns = 25){ nk = length(k)
n = nrow(X)
T = sum(scale(X,scale=F)^2)
W = rep(T, nk) for (i in 1:nk){
  cli = kmeans(X, k[i], nstart=ns)
  W[i] = sum(cli$withinss) }
pF = ((T-W)/(k-1))/(W/(n-k)) return(list(k=k, W=W, pF=pF)) }
pseudoF(nutrition_corr_final[, c(-59)], 3:25)
##################Based on the ADM we will be Using clusters
pseudo_f<-pseudoF(nutrition_corr_final[, c(-59)], 3:25)
pseudo_f$pF
for ( i in 5:length(pseudo_f$pF)){
  print(pf(i, 6-1, nrow(nutrition_corr_final)- 6, lower.tail = FALSE))
  
}
###############################################################3
install.packages("clusterSim") library(clusterSim)
``` ```{r}
fviz_cluster(kmeans_cluster, data = nutrition_corr_final[, c(-59)], #palette = c("#2E9FDF", "#00AFBB", "#E7B800"), geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw() )
``` ```{r}
factor_imputed_columns<- select_if(nutrition_impute_char_2, is.factor) regional_columns
final_dataframe<- cbind(nutrition_corr_final, factor_imputed_columns) final_dataframe<- cbind(final_dataframe, regional_columns)
```
```{r}
#create subsets based on the clusters
cluster_1_set<- subset(kmeans_df, kmeans_df$cluster == 1) cluster_2_set<- subset(kmeans_df, kmeans_df$cluster == 2) cluster_3_set<- subset(kmeans_df, kmeans_df$cluster == 3) cluster_4_set<- subset(kmeans_df, kmeans_df$cluster == 4) cluster_5_set<- subset(kmeans_df, kmeans_df$cluster == 5) cluster_6_set<- subset(kmeans_df, kmeans_df$cluster == 6) cluster_7_set<- subset(kmeans_df, kmeans_df$cluster == 7)


#attach the cluster to the full dataframe to stratify the training set set.seed(271)
final_dataframe$continent<- as.factor(final_dataframe$continent) final_dataframe$regionUN<- as.factor(final_dataframe$regionUN) final_dataframe$subregionUN<- as.factor(final_dataframe$subregionUN)
final_dataframe_2<- merge(final_dataframe, kmeans_df, by = 0)
row.names(final_dataframe_2)<- final_dataframe_2$Row.names final_dataframe_2$Row.names<- NULL
final_dataframe_2$diabetes_prevelance.y<- NULL colnames(final_dataframe_2)[59]<-"diabetes_prevelance"
final_df_split<- createDataPartition(final_dataframe_2$cluster, p = .75, list = FALSE)
final_df_split<- final_df_split[-c(27,11)]
#creating training data set by selecting the output row values train_set_full<-final_dataframe[final_df_split,]
train_set<- train_set_full[, c(-59)]
#creating test data set by not selecting the output row values test_set_full<-final_dataframe[-final_df_split,]
test_set<- test_set_full[, c(-59)]
#assign the x, and y of the training set
x_train<- data.matrix(train_set_full[, c(-59)]) y_train<- train_set_full$diabetes_prevelance
x_test<- data.matrix(test_set_full[, c(-59)]) y_test<- test_set_full$diabetes_prevelance


