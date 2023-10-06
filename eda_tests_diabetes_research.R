
#exploratory data analysis and tests

#Load the Initial datasets
diabetes_data_raw<- read_csv("Downloads/diabetes-prevalence.csv")
#Rename the columns
colnames(diabetes_data_raw)<- c("country", "country_code", "diabetes_prevelance")
colnames(nutrition_data_raw)[1]<- "country"
nutrition_diabetes_merge<- merge(nutrition_data_raw, diabetes_data_raw, by = "country") nutrition_diabetes_merge[nutrition_diabetes_merge == "."]<- NA nutrition_diabetes_merge[nutrition_diabetes_merge == "Not available"]<- NA
#remove any columns if all of the observations are missing or NA nutrition_diabetes_merge<- nutrition_diabetes_merge[, colSums(is.na(nutrition_diabetes_merge)) != nrow(nutrition_diabetes_merge)]
#remove columns that are redundant: country and country code tell the same thing nutrition_diabetes_merge$country_code<- NULL
#rescale the variables that contain negative values
nutrition_diabetes_merge$current_aarr_stunting<- rescale(nutrition_diabetes_merge$current_aarr_stunting, to = c(0,1)) nutrition_diabetes_merge$current_aarr_u5overweight<- rescale(nutrition_diabetes_merge$current_aarr_u5overweight, to = c(0,1))
#Ordering of the Factor Variables
nutrition_diabetes_merge$stunting_progress<- ordered(nutrition_diabetes_merge$stunting_progress, levels= c("Off course - No progress", "Off course - Some progress", "On course - Good progress"))

nutrition_diabetes_merge$u5overweight_progress<- ordered(nutrition_diabetes_merge$u5overweight_progress,levels = c("Off course - No progress","Off course - Some progress","On course - At risk","On course - Good progress"))
nutrition_diabetes_merge$wasting_progress<- ordered(nutrition_diabetes_merge$wasting_progress, levels = c("off course","on course"))
nutrition_diabetes_merge$WHAtarget_WRAanaemia1<- ordered(nutrition_diabetes_merge$WHAtarget_WRAanaemia1, levels = c("Currently off course","Currently on course"))
nutrition_diabetes_merge$Class_IodineNutrition<- ordered(nutrition_diabetes_merge$Class_IodineNutrition, levels = c("Mild iodine deficiency","Moderate iodine deficiency","Optimal iodine nutrition","Risk of adverse health consequences (iodine induced hyperthyroidism, auto-immune thyroid diseases)" ,"Risk of iodine-induced hyperthyroidism within 5?10 years following introduction of iodized salt in susceptible groups", "Severe iodine deficiency"))
nutrition_diabetes_merge$Code_breastfeeding<- ordered(nutrition_diabetes_merge$Code_breastfeeding, levels = c("No information","No action","Being studied","Some provisions voluntary", "Voluntary", "Measure drafted awaiting final approval","Action to end Free Supplies only", "Few provisions law", "Many provisions law","Law"))
nutrition_diabetes_merge$RTF_level1<- ordered(nutrition_diabetes_merge$RTF_level1, levels = c("Low", "Medium","Medium low" ,"Medium high", "High"))
nutrition_diabetes_merge$cat_maternityprotection<- ordered(nutrition_diabetes_merge$cat_maternityprotection, 0:2, c("No", "Partial", "Yes"))
nutrition_diabetes_merge$Fortification<- ordered(nutrition_diabetes_merge$Fortification, levels = c("No Fortification","Planning", "Voluntary", "Mandatory"))
nutrition_diabetes_merge$hypertension_policy<- ordered(nutrition_diabetes_merge$hypertension_policy, levels = c("Available, not implemented", "Available, partially implemented", "Available, fully implemented"))
nutrition_diabetes_merge$diabetes_policy<- ordered(nutrition_diabetes_merge$diabetes_policy, levels = c("Available, not implemented", "Available, partially implemented", "Available, fully implemented"))
nutrition_diabetes_merge<- as.data.frame(nutrition_diabetes_merge) ```

###


#Frequency Tables
table(nutrition_diabetes_merge$WHAtarget_WRAanaemia1) plot(table(nutrition_diabetes_merge$WHAtarget_WRAanaemia1)) table(nutrition_diabetes_merge$Class_IodineNutrition) plot(table(nutrition_diabetes_merge$Class_IodineNutrition)) table(nutrition_diabetes_merge$RTF_level1) plot(table(nutrition_diabetes_merge$RTF_level1))
##############################################################3 #Chi-Squared Test and Fisher's Exact of Independence for categorical variables around government policy
table(nutrition_diabetes_merge$hypertension_policy, nutrition_diabetes_merge$diabetes_policy)
#visualize the contingency table ggplot(nutrition_diabetes_merge) +
aes(x = hypertension_policy, fill = diabetes_policy) + geom_bar() +
  scale_fill_hue() +
  theme_minimal() +
  labs(fill="Diabetes Policy") + xlab("Hypertension Policy") + ylab ("Count of Countries")
#Fisher Test fisher.test(table(nutrition_diabetes_merge$hypertension_policy, nutrition_diabetes_merge$diabetes_policy)
)
#significant
table(nutrition_diabetes_merge$stunting_progress, nutrition_diabetes_merge$wasting_progress)
ggplot(nutrition_diabetes_merge) +
  aes(x = stunting_progress, fill = wasting_progress) + geom_bar() +
  scale_fill_hue() +
  theme_minimal() +
  labs(fill = "Wasting Progress") +
  ylab("Count of Countries") +
  xlab("Stunting Progress")
#Chi Squared Test chisq.test(table(nutrition_diabetes_merge$stunting_progress, nutrition_diabetes_merge$wasting_progress)

)

###


#not significant
table(nutrition_diabetes_merge$u5overweight_progress, nutrition_diabetes_merge$stunting_progress)
ggplot(nutrition_diabetes_merge) +
  aes(x = u5overweight_progress, fill = stunting_progress) + geom_bar() +
  scale_fill_hue() +
  theme_minimal() +
  labs( fil = "Stunting Progress") xlab = ""
#Fisher Test fisher.test(table(nutrition_diabetes_merge$u5overweight_progress, nutrition_diabetes_merge$stunting_progress))
#significant
``` ```{r}
###Kernal Density Plot of the Dependent Variable
prev_density<- density(nutrition_diabetes_merge$diabetes_prevelance) plot(prev_density, frame = FALSE, col = "blue") polygon(prev_density, col = "steelblue")
shapiro.test(nutrition_diabetes_merge$diabetes_prevelance)
#Distribution of the Dependent Variable with the Factors
ggplot(data = nutrition_diabetes_merge, aes(continent, diabetes_prevelance)) + xlab("Continent") +
  ylab("Average Diabetes Prevelance") +
  geom_bar(stat = "summary", fun = "mean")+
  geom_text(aes(label= mean(diabetes_prevelance)), position=position_dodge(width=0.9), just=-0.25)
continent_temp<- aggregate(diabetes_prevelance ~ continent, nutrition_diabetes_merge, mean) levels(nutrition_diabetes_merge$continent)

#create db subsets to perform a wilcox.test for the mean diabetes prevalence of the continents
db1<- subset(nutrition_diabetes_merge, nutrition_diabetes_merge$continent == "Asia" | nutrition_diabetes_merge$continent == "Europe")
db2<- subset(nutrition_diabetes_merge, nutrition_diabetes_merge$continent == "Asia" | nutrition_diabetes_merge$continent == "Africa")
db3<- subset(nutrition_diabetes_merge, nutrition_diabetes_merge$continent == "Asia" | nutrition_diabetes_merge$continent == "Americas")
db4<- subset(nutrition_diabetes_merge, nutrition_diabetes_merge$continent == "Asia" | nutrition_diabetes_merge$continent == "Oceania")
db5<- subset(nutrition_diabetes_merge, nutrition_diabetes_merge$continent == "Africa" | nutrition_diabetes_merge$continent == "Europe")
db6<- subset(nutrition_diabetes_merge, nutrition_diabetes_merge$continent == "Americas" | nutrition_diabetes_merge$continent == "Europe")
db7<- subset(nutrition_diabetes_merge, nutrition_diabetes_merge$continent == "Oceania" | nutrition_diabetes_merge$continent == "Europe")
db8<- subset(nutrition_diabetes_merge, nutrition_diabetes_merge$continent == "Africa" | nutrition_diabetes_merge$continent == "Americas")
db9<- subset(nutrition_diabetes_merge, nutrition_diabetes_merge$continent == "Africa" | nutrition_diabetes_merge$continent == "Oceania")
db10<- subset(nutrition_diabetes_merge, nutrition_diabetes_merge$continent == "Americas" | nutrition_diabetes_merge$continent == "Oceania")
#all significant except by continent
wilcox.test(diabetes_prevelance ~ continent, data =
              #significant
              wilcox.test(diabetes_prevelance ~ continent, data =
                            wilcox.test(diabetes_prevelance ~ continent, data =
                                          #not significant
                                          wilcox.test(diabetes_prevelance ~ continent, data =
                                                        wilcox.test(diabetes_prevelance ~ continent, data =
                                                                      wilcox.test(diabetes_prevelance ~ continent, data =
                                                                                    wilcox.test(diabetes_prevelance ~ continent, data =
                                                                                                  wilcox.test(diabetes_prevelance ~ continent, data =
                                                                                                                wilcox.test(diabetes_prevelance ~ continent, data =
                                                                                                                              x_test1<- wilcox.test(diabetes_prevelance ~ continent, data = db10, correct=FALSE)
                                                                                                                            ####################################################################
                                                                                                                            #count the number of observations and the number of factors nrow(nutrition_diabetes_merge) ncol(nutrition_diabetes_merge)
                                                                                                                            db1, correct=FALSE)
                                                                                                              db2, correct=FALSE) db3, correct=FALSE)
                                                                                  db4, correct=FALSE) db5, correct=FALSE) db6, correct=FALSE) db7, correct=FALSE) db8, correct=FALSE) db9, correct=FALSE)

#count the number of numerical and categorical variables ncol(select_if(nutrition_diabetes_merge, is.numeric)) ncol(select_if(nutrition_diabetes_merge, is.character))
#plot the distribution of the dependent variables
x_test2<- nutrition_diabetes_merge$diabetes_prevelance
prev_hist<- hist(nutrition_diabetes_merge$diabetes_prevelance, color = "blue", main = "Distrinution of Diabetes Prevalence")
fit_xline<- seq(min(x_test2), max(x_test2), length = 40)
fit_yline<- dnorm(fit_xline, mean = mean(x_test2), sd= sd(x_test2))
fit_yline<- fit_yline * diff(prev_hist$mids[1:2]*length(x_test2)) lines(fit_xline, fit_yline, col = "red", lwd = 2)
skim(nutrition_diabetes_merge) ######Distribution Plots
#Continent and Diabetes Prevalence
ggplot(data = nutrition_diabetes_merge, aes(continent, diabetes_prevelance)) +
  geom_bar(stat = "identity", aes(fill = "identity"), show.legend = FALSE) + ggtitle("Distribution of Diabetes Prevalence by Country") + xlab("Continent") +
  ylab("Diabetes Prevalence")
#Under 5 Population and Diabetes Prevalence
ggplot(data = nutrition_diabetes_merge, aes(under5pop, diabetes_prevelance)) +
  geom_point(stat = "identity") +
  ggtitle("Distribution of Diabetes Prevelance by Young Population") + xlab("Total Under 5 Population") +
  ylab("Diabetes Prevalence")
#Under 5 Population excuding India, China, and Nigeria
ggplot(data = nutrition_diabetes_merge[-c(35,73,119),], aes(under5pop, diabetes_prevelance)) +
  geom_point(stat = "identity") +
  ggtitle("Distribution of Diabetes Prevelance by Young Population") + xlab("Total Under 5 Population") +
  ylab("Diabetes Prevalence")
#Above 65 Population and Diabetes Prevalence- decreasing trend
ggplot(data = nutrition_diabetes_merge, aes(over65pop, diabetes_prevelance)) +
  
  geom_point(stat = "identity") +
  ggtitle("Distribution of Diabetes Prevelance by Aging Population") + xlab("Total Above 65 Population") +
  ylab("Diabetes Prevalence") +
  geom_smooth(method='lm', formula= y~x)
#Poverty Rates and Diabetes Prevalence- decreasing trend
ggplot(data = nutrition_diabetes_merge, aes(poverty2_4,diabetes_prevelance)) +
  geom_point(stat = "identity") + xlab("Percentage of Poverty Rates") + ylab("Diabetes Prevelance") + geom_smooth(method='lm', formula= y~x) + ggtitle("Poverty Rates by Diabetes Prevelance")
poverty_slope<- lm(log(diabetes_prevelance) ~poverty2_4, nutrition_diabetes_merge) plot(poverty_slope,3)
summary(poverty_slope)
#GDP by Diabetes Prevalence- increasing trend
ggplot(data = nutrition_diabetes_merge, aes(GDP_1,diabetes_prevelance)) +
  geom_point(stat = "identity") +
  xlab("GDP Distribution") +
  ylab("Diabetes Prevelance") + geom_smooth(method='lm', formula= y~x)
explore_temp2<- nutrition_diabetes_merge[-c(35,73)] #explore_temp2$diabetes_prevelance<- (log(explore_temp2$diabetes_prevelance))
# Overweight Population by Diabetes Prevalence-increasing trend
ggplot(data = nutrition_diabetes_merge, aes(prev_u5overweight,diabetes_prevelance)) +
  geom_point(stat = "identity") + xlab("Percentage of Overweight Population") + ylab("Diabetes Prevelance") + geom_smooth(method='lm', formula= y~x)
summary(weight_slope) ```


