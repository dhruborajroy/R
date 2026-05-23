# https://youtu.be/qkivJzjyHoA?list=PL34t5iLfZddu8M0jd7pjSVUjvjBOBdYZ1&t=216
#Preparre data
data <- read.csv("https://raw.githubusercontent.com/bkrai/Statistical-Modeling-and-Graphs-with-R/main/Cardiotocographic.csv")
data
data$NSP <- as.ordered(data$NSP)
str(data)
# "1"<"2"<"3" 1 is normal and 3 is worst condition
data$Tendancy <- as.factor(data$Tendency)
str(data)
summary(data)
xtabs(~NSP+Tendency, data)

#partion the data
ind <- sample(2, nrow(data), replace = T, prob= c(0.8,0.2))
train <- data[ind==1,]
test <- data[ind==2,]


#Ordinal logistic regrassion or potential odds logistic regression  model
install.packages("MASS")
library(MASS)
model <- polr(NSP~LB+AC+FM, train, Hess= TRUE) #propotional odds logistic regression
summary(model)
# Interpretatüon: For one unit increase ün FM, we expect about 6030483 inncrease in the expected value of NSP in the bg odds scale9 given that all other varüables in the model are held constant

# p value calculation
(ctable <- coef(summary(model)))
p <- pnorm(abs(ctable[,"t value"]), lower.tail = FALSE) *2

           