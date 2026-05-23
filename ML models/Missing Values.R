#https://youtu.be/An7nPLJ0fsg
#Libraries
install.packages("mice")
install.packages("VIM")
library(mice)
library(VIM)

#data
setwd("G:/R Programming/Datasets/Waste Generation/")
data <- read.csv(
  "G:/R Programming/Datasets/Waste Generation/waste generation dataset.csv"
)


str(data)

summary(data)


#missing variable

p <- function(x){sum(is.na(x))/length(x)*100} 
apply(data, 2,p)

#missing data(md)
missing_data <- md.pattern(data)
missing_data_pattern <- md.pairs(data)
#rm= observed and missing 
#mr=missing vs observed
#mm= missing vs missing 

write.csv(missing_data, file = "missing data.csv")
write.csv(missing_data_pattern, file = "missing data Pattern.csv")


marginplot(data[,c("msw_total_msw_generated_kg_per_cap_per_day","gdp")])

#impute
impute <- mice(data[,], m=3, seed=123)
print(impute)
impute$imp$composition_msw_food_organic_waste_percent
#parameters
#pmm = predictive mean matching
#polyreg = multinomial logisistic regresion

#once impute has done now we can have complete dataset using complete function
new_data <- complete(impute, 1 ) #here 1 is number of imputation i want 


#Distribution of observed/imputed values
stripplot(impute,pch=20, cex=1.2)

xyplot(impute, gdp ~ msw_total_msw_generated_kg_per_cap_per_day | .imp, pch=20, cex=1.4)
