library(rpart)
library(rpart.plot)

cars2020 <- read.csv("data/cars2020.csv")


my_tree <- rpart(mpg~transmission+disp, data = cars2020)

rpart.plot(my_tree)

my_tree <- rpart(mpg~transmission+disp+aspiration+cyl+gears, data = cars2020)

thisisdaryn@gmail.com