
#Preparre data
data <- read.csv("G:/R Programming/ML/vehicle.csv")
data

head(data)

ind <- sample(2, nrow(data), replace = T, prob = c(0.8,0.2))

train <- data[ind==1,]
test <- data[ind==2,]

#multiple regrassion model
result <- lm(lc~lh+Mileage, train)
summary(result)

pred <- predict(result, train)
summary(pred)

head(pred)
head(train)
