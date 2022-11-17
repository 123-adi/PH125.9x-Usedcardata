#### Packages ####

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(matrixStats)
library(e1071)
library(dplyr)
library(ggplot2)
library(scales)
library(caret)
library(rpart)
library(rpart.plot)

#### Importing data ####

data <- read_csv("UsedCarData.csv") # uploaded on github: https://github.com/123-adi/PH125.9x-Usedcardata/blob/main/UsedCarData.csv

#### 1. Data exploration & manipulation ####

### 1.1 Initial exploration and manipulation

glimpse(data) # 7906 records across 18 columns; 1 id, 1 outcome, 16 potential predictors

# check any NA's

cbind(
  lapply(
    lapply(data, is.na)
    , sum)
) 

# drop torque as not needed

data_2 <- select(data, -torque) 

# factorizing

data_3 <- mutate(data_2,
                      Sales_ID = factor(Sales_ID),
                      name = factor(name),
                      Region = factor(Region),
                      `State or Province` = factor(`State or Province`),
                      City = factor(City),
                      fuel = factor(fuel),
                      seller_type = factor(seller_type),
                      transmission = factor(transmission),
                      owner = factor(owner),
                      seats = factor(seats))

# renaming

data_4 <- rename(data_3, 
                      id = Sales_ID,
                      brand = name,
                      region = Region,
                      state_province = `State or Province`,
                      city = City)

# converting sold N,Y to 0,1

data_5 <- data_4

data_5$sold <- ifelse(data_4$sold == "Y", 1, 0) %>% factor() 


### 1.2 Summary statistics and visualization

## 1.2.1 Summary stats

dim(data_5) # 1 id, 1 outcome, 15 potential predictors

summary(data_5)[,-1] # don't include id

# brands
n_distinct(data_5$brand) # 31 brands

# states/provinces
n_distinct(data_5$state_province) # 49 states/provinces

# cities
n_distinct(data_5$city) # 1310 cities

## 1.2.2 Visualizing the data

# 1.2.2.1 Non-categorical data histograms


hist(data_5$selling_price, main = "", xlab = "Selling price") # right/positive skew
hist(data_5$km_driven, main = "", xlab = "Km driven") # right/positive skew
hist(data_5$mileage, main = "", xlab = "Mileage") # normal, mean=median
hist(data_5$engine, main = "", xlab = "Engine") # right/positive
skewness(data_5$engine) # 1.13
hist(data_5$max_power, main = "", xlab = "Max power") # right/positive?
skewness((data_5$max_power)) # 1.63

# 1.2.2.2 Categorical data barplots

plot(data_5$region, main = "", xlab = "Region") # not evenly distributed but not a large variance either
plot(data_5$state_province, main = "", xlab = "State or Province") # large variances/peaks
plot(data_5$city, main = "", xlab = "City") # very large spikes, presumably for large cities

plot(data_5$brand, main = "", xlab = "Brand") # not evenly distributed across brands
plot(data_5$fuel, main = "", xlab = "Fuel") # mostly diesel or petrol
plot(data_5$seller_type, main = "", xlab = "Seller type") # majority individual
plot(data_5$transmission, main = "", xlab = "Transmission") # mostly manual
plot(data_5$owner, main = "", xlab = "Owner") # majority first or second owner
plot(data_5$seats, main = "", xlab = "Seats") # majority 5 seaters
hist(data_5$year,main = "", xlab = "Year") # mostly from mid-2000's 


### 1.3 Further exploration & manipulation

# Ratio of sold:unsold cars
sold_prop <- mean(data_5$sold == 1) # only 25% cars sold; N:Y = 3:1
unsold_prop <- mean(data_5$sold == 0)

# Cars with 0 mileage
data_5 %>%
  filter(mileage == 0) # 17 cars with 0 mileage

skewness(data_5$mileage) # -0.14; fairly symmetrical 

mileage_median <- data_5 %>%
  filter(mileage != 0) %>%
  select(mileage) %>%
  as.matrix() %>%
  colMedians(.) # median mileage excluding 0's

usedcardata <- data_5

usedcardata$mileage <- replace(data_5$mileage, data_5$mileage==0, mileage_median) # replace 0's with column median

summary(usedcardata$mileage) # check again

## 1.3.1 Exploring ratio of sold:unsold across the predictors

## Categorical ##

# 1.3.1.1 Brand

# all

ggplot(usedcardata, aes(y = brand, fill = sold)) + geom_bar(position = "fill") # variation of sold:unsold ratio across brands

# top 6

top_brand_names <- c("Maruti", "Hyundai", "Mahindra", "Tata", "Honda", "Toyota") # top brands by count

top_brands <- filter(usedcardata, brand %in% top_brand_names) # keeping only top brands

ggplot(top_brands, aes(y = brand, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") # for top 6 brands sold:unsold ratio similar to overall ratio

# 1.3.1.2 Region

ggplot(usedcardata, aes(y = region, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") # East seems to have slightly larger proportion of sold cars to unsold

# 1.3.1.3 State/Province

# all
ggplot(usedcardata, aes(y = state_province, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") # variation of sold:unsold across states/provinces

# top 6
top_sp_names <- c("California", "Texas", "New York", "Illinois", "Florida", "Ohio")

top_sp <- filter(usedcardata, state_province %in% top_sp_names)

ggplot(top_sp, aes(y = state_province, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") # ohio unsold and new york sold proportions larger than overall ratio

# 1.3.1.4 City

# all

ggplot(usedcardata, aes(y = city, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") # difficult to visually determine proportions for individual cities but based on color there seems to be a lot of variation across cities

# top 6

top_city_names <- c("New York City", "Los Angeles", "Seattle", "Chicago", "Boston", "Washington")

top_city <- filter(usedcardata, city %in% top_city_names)

ggplot(top_city, aes(y = city, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") # top 6 cities most or all cars are sold

# 1.3.1.5 Fuel type

ggplot(usedcardata, aes(y = fuel, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") # sold:unsold for petrol and diesel (two most popular fuels) almost identical to overall ratio

# 1.3.1.6 Transmission type

ggplot(usedcardata, aes(y = transmission, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") # sold ratio across the two types almost identical to overall ratio


# 1.3.1.7 Seller type

ggplot(usedcardata, aes(y = seller_type, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") # trustmark dealer has higher proportion of sold compared to overall ratio; individual and dealer ratios almost identical to overall


# 1.3.1.8 Owner type 

ggplot(usedcardata, aes(y = owner, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") # sold ratio for first and second owners (two most prevalent types) almost identical to overall ratio

# 1.3.1.9 No. of seats

ggplot(usedcardata, aes(y = seats, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") # sold ratio for 5 seaters (most prevalent) almost identical to overall ratio


# 1.3.1.10 Year

ggplot(usedcardata, aes(y = year, fill = sold)) + 
  geom_bar(position = "fill") +
  geom_vline(xintercept = sold_prop, color="black", linetype="dashed") # sold ratio for 2005 or later hovers around overall ratio

## Non-categorical ##

# 1.3.1.11 Selling price

ggplot(usedcardata, 
       aes(x = selling_price, y = sold)) +
  geom_violin() +
  scale_x_continuous(labels = comma) +
  coord_flip() +
  geom_boxplot(width=0.2)

# not much variation in distribution of selling price between sold and unsold cars

# 1.3.1.12 Km driven

ggplot(usedcardata, 
       aes(x = km_driven, y = sold)) +
  geom_violin() +
  scale_x_continuous(labels = comma) +
  coord_flip() +
  geom_boxplot(width=0.2)

# not much variation in distribution of km driven between sold and unsold cars


# 1.3.1.13 Car mileage

ggplot(usedcardata, 
       aes(x = mileage, y = sold)) +
  geom_violin() +
  coord_flip() +
  geom_boxplot(width=0.2)

# not much variation in distribution of mileage between sold and unsold cars


# 1.3.1.14 Engine size

ggplot(usedcardata, 
       aes(x = engine, y = sold)) +
  geom_violin() +
  coord_flip() +
  geom_boxplot(width=0.2)

# not much variation in distribution of engine size between sold and unsold cars

# 1.3.1.15 Engine max power

ggplot(usedcardata, 
       aes(x = max_power, y = sold)) +
  geom_violin() +
  coord_flip() +
  geom_boxplot(width=0.2)

# not much variation in distribution of max power between sold and unsold cars

#### 2 Modeling ####

### 2.1 Creating training and test sets

set.seed(1, sample.kind = "Rounding") 

test_index <- createDataPartition(usedcardata$sold, times = 1, p = 0.2, list = FALSE) # 20% index
test_set <- usedcardata[test_index,] # 20% test
train_set <- usedcardata[-test_index,] # 80% train

nrow(train_set)/nrow(usedcardata) # check if 80%

mean(train_set$sold == 1) # see sold % in training set, matches ratio for whole dataset

### 2.2 Basic model guessing

set.seed(2, sample.kind = "Rounding")

base <- sample(c(0,1), nrow(test_set), replace = TRUE)

mean(base == test_set$sold) # .482933

model_accuracy <- tibble(model = "Basic", accuracy = mean(base == test_set$sold))

### 2.3 Logistic regression

## 2.3.1 Region

set.seed(1, sample.kind = "Rounding") 

# model
train_glm_region <- train(sold ~ region, 
                          method = "glm", 
                          data = train_set)

# accuracy
pred_glm_region <- predict(train_glm_region, test_set)

cm_region_glm <- confusionMatrix(data = pred_glm_region, 
                                 reference = test_set$sold)

cm_region_glm$table
cm_region_glm$overall["Accuracy"] # 0.7471555
cm_region_glm$byClass["F1"] # 0.8552822

# add to table
model_accuracy <- model_accuracy %>% add_row(model = "Region GLM", accuracy = cm_region_glm$overall["Accuracy"])
model_f1 <- tibble(model = "Region GLM", f1 = cm_region_glm$byClass["F1"])


## 2.3.2 State or Province

set.seed(1, sample.kind = "Rounding") 

# model
train_glm_state <- train(sold ~ state_province, 
                         method = "glm", 
                         data = train_set)

# accuracy
pred_glm_state <- predict(train_glm_state, test_set)

cm_state_glm <- confusionMatrix(pred_glm_state, test_set$sold)

cm_state_glm$table
cm_state_glm$overall["Accuracy"] # 0.7541087
cm_state_glm$byClass["F1"] # 0.853484 

# add to table
model_accuracy <- model_accuracy %>% add_row(model = "State/Province GLM", accuracy = cm_state_glm$overall["Accuracy"])
model_f1 <- model_f1 %>% add_row(model = "State/Province GLM", f1 = cm_state_glm$byClass["F1"])

## 2.3.3 City

set.seed(1, sample.kind = "Rounding") 

# model
train_glm_city <- train(sold ~ city, 
                        method = "glm", 
                        data = train_set)

# accuracy
pred_glm_city <- predict(train_glm_city, test_set)

cm_city_glm <- confusionMatrix(pred_glm_city, test_set$sold)

cm_city_glm$table
cm_city_glm$overall["Accuracy"] # 0.9241466 
cm_city_glm$byClass["F1"] # 0.9494949 

# add to table
model_accuracy <- model_accuracy %>% add_row(model = "City GLM", accuracy = cm_city_glm$overall["Accuracy"])
model_f1 <- model_f1 %>% add_row(model = "City GLM", f1 = cm_city_glm$byClass["F1"])

## 2.3.4 Location (Region + State/Province + City)

set.seed(1, sample.kind = "Rounding") 

# model
train_glm_location <- train(sold ~ region + state_province + city, 
                            method = "glm", 
                            data = train_set)

# accuracy
pred_glm_location <- predict(train_glm_location, test_set)

cm_location_glm <- confusionMatrix(pred_glm_location, test_set$sold)

cm_location_glm$table
cm_location_glm$overall["Accuracy"] # 0.9405815
cm_location_glm$byClass["F1"] # 0.9603041 

# add to table
model_accuracy <- model_accuracy %>% add_row(model = "Location GLM", accuracy = cm_location_glm$overall["Accuracy"])
model_f1 <- model_f1 %>% add_row(model = "Location GLM", f1 = cm_location_glm$byClass["F1"])

### 2.4 Decision tree

## 2.4.1 Location (Region + State/Province +  City) 

set.seed(2, sample.kind = "Rounding") 

rpart_tunegrid <- data.frame(cp = seq(0, 0.02, 0.001))

# model
train_rpart_location <- train(sold ~ region + state_province + city, 
                              method = "rpart",
                              tuneGrid = rpart_tunegrid,
                              data = train_set)

# best cp
train_rpart_location$results %>%
  select(cp, Accuracy) %>%
  plot()

train_rpart_location$bestTune # 0

# accuracy
pred_rpart_location <- predict(train_rpart_location, test_set)

cm_location_rpart <- confusionMatrix(pred_rpart_location, test_set$sold)

cm_location_rpart$table
cm_location_rpart$overall["Accuracy"] # 0.869153 
cm_location_rpart$byClass["F1"] # 0.9182787 

# add to table
model_accuracy <- model_accuracy %>% add_row(model = "Location RPART", accuracy = cm_location_rpart$overall["Accuracy"])
model_f1 <- model_f1 %>% add_row(model = "Location RPART", f1 = cm_location_rpart$byClass["F1"])

# plot
rpart.plot(train_rpart_location$finalModel, 
           type = 2, 
           fallen.leaves = FALSE, 
           box.palette="RdGn")

## 2.4.2 Location 2 - less complex model

# model
set.seed(2, sample.kind = "Rounding") 

train_rpart_location_2 <- train(sold ~ region + state_province + city, 
                              method = "rpart",
                              tuneGrid = data.frame(cp = seq(0.03, 0.05, 0.001)),
                              data = train_set)

# best cp
train_rpart_location_2$results %>%
  select(cp, Accuracy) %>%
  plot()

train_rpart_location_2$bestTune # 0.03

# accuracy
pred_rpart_location_2 <- predict(train_rpart_location_2, test_set)

cm_location_rpart_2 <- confusionMatrix(pred_rpart_location_2, test_set$sold)

cm_location_rpart_2$table
cm_location_rpart_2$overall["Accuracy"] # 0.8185841  
cm_location_rpart_2$byClass["F1"] #  0.8916572 

# add to table
model_accuracy <- model_accuracy %>% add_row(model = "Location RPART 2", accuracy = cm_location_rpart_2$overall["Accuracy"])
model_f1 <- model_f1 %>% add_row(model = "Location RPART 2", f1 = cm_location_rpart_2$byClass["F1"])

# plot
rpart.plot(train_rpart_location_2$finalModel, 
           type = 2, 
           fallen.leaves = FALSE, 
           box.palette="RdGn")



