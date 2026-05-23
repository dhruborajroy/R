#data
data(iris)
str(iris)
library(ggplot2)

qplot(Petal.Length, Petal.Width, data=iris, color=Species)


#support Vector Machine
install.packages("e1071")
library(e1071)

mymodel <- svm(Species~., data=iris)


summary(mymodel)

plot(mymodel, data=iris, 
     Petal.Width~Petal.Length, slice = list(Sepal.Width=3, Sepal.Length=4))


# Tuning
set.seed(123)
tmodel <- tune(svm, Species~., data=iris, ranges=list(epsilon = seq(0,1,0.1), cost=2^(2:9)))

plot(tmodel)
