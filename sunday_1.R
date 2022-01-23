library(tidyverse)# include dplyr, ggplot2 and a few other packages
library(rsample) # package for splitting data for testing and training
library(rpart) # for decision tree models
library(rpart.plot) # visualising decision trees that are output from rpart

cars2020 <- read.csv("cars2020.csv")

set.seed(1729) # to make sure entire class has the same splits of data 
split <- initial_split(cars2020, prop = 0.8, strata = mpg) # function in rsample

train <- training(split)
test <- testing(split)

ggplot(data = cars2020,
       aes(x = transmission, 
           y = mpg)) + geom_boxplot() + theme_minimal()

ggplot(data = cars2020, 
       aes(x = disp, y = mpg)) + 
  geom_jitter(alpha = .2) + theme_minimal() + 
  geom_smooth(method = "lm")

### First model: using transmission type alone to predict mpg using lm

model1 <- lm(mpg~transmission, data = train)
summary(model1)

train <- mutate(train, 
                model1_prediction = predict(model1, newdata = train))

ggplot(train, 
       aes(y = model1_prediction, 
           x = mpg)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) + 
  theme_minimal() + ylim(c(0,60)) + xlim(c(0,60))

model2 <- lm(mpg~disp, data = train)

summary(model2)

train <- mutate(train, 
                model2_prediction = predict(model2, newdata = train))

ggplot(train,
       aes(y = model2_prediction, 
           x = mpg)) + 
  geom_point()+
  geom_abline(intercept = 0, slope = 1) + 
  theme_minimal() + ylim(c(0,60)) + xlim(c(0,60))

temp <- train |> select(mpg, model1_prediction, model2_prediction)|> 
  pivot_longer(model1_prediction:model2_prediction,
               names_to = "model",
               values_to = "prediction")

ggplot(temp,
       aes(x = mpg, y = prediction)) + 
  facet_wrap(~model) + geom_point() + 
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1) + 
  theme_minimal() + ylim(c(0,60)) + xlim(c(0,60))


model3 <- lm(mpg~disp+transmission, data = train)
summary(model3)

train <- mutate(train, model3_prediction = predict(model3, newdata = train))


temp <- train |> select(mpg, model1_prediction, model2_prediction, model3_prediction)|> 
  pivot_longer(model1_prediction:model3_prediction,
               names_to = "model",
               values_to = "prediction")

ggplot(temp,
       aes(x = mpg, y = prediction)) + 
  facet_wrap(~model) + geom_point() + 
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1) + 
  theme_minimal() + ylim(c(0,60)) + xlim(c(0,60))

model4 <- lm(mpg~atvType+disp+transmission, data = train)
temp4 <- mutate(train, 
               model4_prediction = predict(model4, newdata = train))|>
  select(mpg, model1_prediction:model4_prediction) |> 
  pivot_longer(model1_prediction:model4_prediction,
               names_to = "model",
               values_to = "prediction")

ggplot(data = temp4,
       aes(x = mpg, y = prediction)) +
  facet_wrap(~model) + geom_point() + 
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1) + 
  theme_minimal() + ylim(c(0,60)) + xlim(c(0,60))


model5 <- rpart(mpg~atvType+disp+transmission, data = train)
rpart.plot(model5)

train <- mutate(train, model5_prediction = predict(model5, newdata = 
                                                     train))

#### Now to apply two of the models to the testing set

test <- mutate(test, model4_prediction = predict(model4, newdata = test),
                 model5_prediction = predict(model5, newdata = 
                                                   test))
temp_test <- test |> select(mpg, model4_prediction, model5_prediction) |> 
  pivot_longer(model4_prediction:model5_prediction,
               names_to = "model",
               values_to = "prediction")

ggplot(data = temp_test,
       aes(x = mpg, y = prediction)) +
  facet_wrap(~model) + geom_point() + 
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1) + 
  theme_minimal() + ylim(c(0,60)) + xlim(c(0,60)) + 
  labs(title = "Model predictions on the test set")
  
