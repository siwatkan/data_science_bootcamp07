#install.packages("tidyverse")
install.packages("caret")
#install.packages("sqldf")

library("tidyverse")
library("caret")


#train test split
#1. split data
#2. train
#3. score
#4. evaluate
#~

glimpse(mtcars)

train_test_Split <- function(data, trainRatio=0.7) {
set.seed(42)
#split data 70:30
(n <- nrow(data))
(id <- sample(1:n, size=trainRatio*n))
#การใส่วงเล็บจะ print in console
train_data <- data[id,]
test_data <- data[-id,]

preproc1 <- preProcess(train_data, method = "range")
preproc2 <- preProcess(test_data, method = "range")
train_data <- predict(preproc1, train_data)
test_data <- predict(preproc2, test_data)

#train_data <- predict(preproc, data[id,])
#test_data <- predict(preproc, data[-id,])
#train_data <- scale(data[id,])
#test_data <- scale(data[-id,])
return (list(train=train_data, test=test_data))
}

splitData <- train_test_Split(mtcars, 0.8)
train_data <- splitData$train
test_date <- splitData$test


#train model
model <- lm(mpg ~ hp+ wt + am, data = train_data)


#score model
mpg_pred <- predict(model, newdata= test_data)

mpg_pred
test_data$mpg


#evaluate model
#MAE, MSE , RMSE

mae_metric <- function(actual, prediction) {
  #mean absolute error
  abs_error <- abs(actual - prediction)
  mean(abs_error)
}
#

mse_metric <- function(actual, prediction) {
  #mean squared  error
  sq_error <- (actual - prediction)**2
  mean(sq_error)
}
# 
rmse_metric <- function(actual, prediction) {
  #root mean squared error
  sq_error <- (actual - prediction)**2
  sqrt(mean(sq_error)) ## back to normal unit
}

r_squared <- function(actual, prediction) {
  #root mean squared error
  r2 <- 1-(sum((actual - prediction)^2) / sum((actual - mean(prediction))^2))
  return(r2)
}


r_squared <- 1 - sum((actual_values - predicted_values)^2) / sum((actual_values - mean(actual_values))^2)



## CARET = Classification amd regression Tree
## Supervied Learning = Prediction





library(caret)
#1. Split Data
splitData <- train_test_Split(mtcars,0.7)
train_data <= splitData[[1]]
test_data <= splitData[[2]]

#2. Train model
# เข้าแทราแซง 
ctrl <- trainControl(
  method = "cv",# "LOOCV" , "cv" k-fold golden standard  
  number = 5,
  verboseIter = TRUE
)


# mpg = f(hp, wt, am)
model <- train(mpg ~ hp + wt + am, 
               data = train_data,
               method = "lm" ,# algorithm ,
               trControl = ctrl
               )
#defalt resample model caret = bootstrapped 
#resample 25%


rf_model <- train(mpg ~ hp + wt + am, 
               data = train_data,
               method = "rf" ,# algorithm ,
               trControl = ctrl
)

knn_model <- train(mpg ~ hp + wt + am, 
                  data = train_data,
                  method = "knn" ,# algorithm ,
                  trControl = ctrl
)


#3. Score Model

p<- predict(model, newdata = test_data)

#4. Evaluate mode

rmse_metric(test_data$mpg , p)

#5.save model
saveRDS(model, "linear_regression_v1.RDS")


new_cars <- data.frame(
  hp = c(150,200,250),
  wt = c(1.25,2.2,2.5),
  am = c(0,1,1)
)
## read file model
model1 <- readRDS("linear_regression_v1.RDS")

new_car$mpg_pred <- predict(model, newdata = new_cars)
new_cars

#HW Intro ML
# objective : To predict the price of houses in India.
# import library
library("httr")
library("readxl")
library("tidyverse")
library("caret")
GET("https://query.data.world/s/m6jw4q7y2ekkfba5rcse5o6zepaj3k", write_disk(tf <- tempfile(fileext = ".xlsx")))
df_house <- read_excel(tf)

# EDA 
# Data manipulation
df_house$Price <- log(df_house$Price)
names(df_house) <- gsub(" ", "_", names(df_house))
names(df_house) <- gsub("[()]", "_", names(df_house))
#correlation coefficient for feature selection
cor(df_house)
#1. Split Data
splitData <- train_test_Split(df_house,0.8)
#train_data <- splitData[[1]]
#test_data <- splitData[[2]]
#2. Train model
ctrl <- trainControl(
  method = "cv",  
  number = 5,
  verboseIter = TRUE
)

model <- train(Price ~ number_of_bedrooms + 
                 number_of_bathrooms + 
                 living_area + 
                 number_of_floors +
                 waterfront_present +
                 number_of_views +
                 grade_of_the_house + 
                 Area_of_the_house_excluding_basement_ +
                 Area_of_the_basement +
                 living_area_renov +
                 Renovation_Year +
                 Lattitude +
                 Built_Year +
                 lot_area_renov
                 , 
               data = train_data,
               method = "lm" ,
               trControl = ctrl
)
#3. Score Model
p<- predict(model, newdata = test_data)
#4. Evaluate mode
paste('RMSE of Test:',rmse_metric(test_data$Price , p))
paste('Rsquared of Test:',r_squared(test_data$Price , p))
paste('MSE of Test:',mse_metric(test_data$Price , p))

p1 <- ggplot(df_house, aes(Price)) +
  geom_histogram(bins=30)
