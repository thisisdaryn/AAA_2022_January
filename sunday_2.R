library(dplyr)
library(ggplot2)
library(rsample)

so <- read.csv("data/stackoverflow.csv")

ggplot(data = so,
       aes(x = country)) + 
  geom_bar(fill = NA, color = "brown") + theme_minimal()

count(so, remote)

so <- mutate(so, 
             remote_binary = ifelse(remote == "Not remote", 0, 1)) |>
  relocate(remote_binary, remote)

set.seed(2001)
split <- initial_split(so, prop = 0.8)

train <- training(split)
test <- testing(split)

##### Logistic regression model using glm 

logistic_regression_model <- glm(remote_binary~country+salary+years_coded_job,
                                 data = train, family = binomial(link = "logit"))
summary(logistic_regression_model)

hist(predict(logistic_regression_model, newdata = train, type = "response"))
# type = "response" is necessary to get the output as a probability i.e. 
# after the reversing the logit function

train <- mutate(train, 
                log_m_prediction = predict(logistic_regression_model, newdata = train, type = "response"))

ggplot(data = train,
       aes(x = remote, y = log_m_prediction)) + geom_boxplot()

### To handle imbalance of data 

remote_df <- so |> filter(remote == "Remote")
notremote_df <- so |> filter(remote == "Not remote")

### Now take half of the remote workers for the training set. Then take the 
### same amount from the not remote group 

set.seed(2001)
sample1 <- sample(718, 359)
remote_train <- remote_df[sample1, ]
sample2 <- sample(6273, 359)
notremote_train <- notremote_df[sample2, ]

new_train <- bind_rows(remote_train, notremote_train)

### New logistic regression model trained on balanced training set
logistic_regression_model2 <- glm(remote_binary~country+salary+years_coded_job,
                                  data = new_train, family = binomial(link = "logit"))

new_train <- mutate(new_train, logistic_model_prediction = 
                      predict(logistic_regression_model2, newdata = new_train,
                              type = "response"))

ggplot(data = new_train, 
       aes(x = remote, y = logistic_model_prediction)) + 
  geom_boxplot()


