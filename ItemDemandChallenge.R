library(tidyverse)
library(tidymodels)
library(embed)
library(vroom)
library(ggplot)
library(patchwork)

# Read in the data
setwd("/Users/student/Desktop/STAT348/ItemDemandChallenge")
Item_training <-vroom("/Users/student/Desktop/STAT348/ItemDemandChallenge/train.csv")
Item_test <- vroom("/Users/student/Desktop/STAT348/ItemDemandChallenge/test.csv")

view(Item_training)

nStores <- max(Item_training$store)
nItems <- max(Item_training$item)
  for(s in 1:nStores){
    for(i in 1:nItems){
    storeItemTrain <- Item_training %>%
    filter(store==s, item==i)
    storeItemTest <- Item_test %>%
    filter(store==s, item==i)
    
# Fit storeItem models here

# Predict storeItem sales
    
# Save storeItem predictions
    if(s==1 & i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
    
  }
  }

library(timetk)
storeItemTrain %>%
plot_time_series(dateVar, responseVar, .interactive=FALSE)

store1 <- filter(Item_training, store == "1")
view(store1)

## 4 Panel Plot of ACFs
plot1 <- Item_training %>%
  filter(store==sample(1:10,1), item==sample(1:50,1)) %>%
  pull(sales) %>%
  forecast::ggAcf(.)

plot2 <- Item_training %>%
  filter(store==sample(1:10,1), item==sample(1:50,1)) %>%
  pull(sales) %>%
  forecast::ggAcf(.)

plot3 <- Item_training %>%
  filter(store==sample(1:10,1), item==sample(1:50,1)) %>%
  pull(sales) %>%
  forecast::ggAcf(.)

plot4 <- Item_training %>%
  filter(store==sample(1:10,1), item==sample(1:50,1)) %>%
  pull(sales) %>%
  forecast::ggAcf(.)

plot <- (plot1 + plot2) / (plot3 + plot4)
plot


# Feature Engineering -----------------------------------------------------

library(tidyverse)
library(tidymodels)
library(embed)
library(vroom)
library(patchwork)

# Read in the data
setwd("/Users/student/Desktop/STAT348/ItemDemandChallenge")
Item_training <-vroom("/Users/student/Desktop/STAT348/ItemDemandChallenge/train.csv")
Item_test <- vroom("/Users/student/Desktop/STAT348/ItemDemandChallenge/test.csv")

view(Item_training)

storeItem <- Item_training %>%
  filter(store==5, item==7)

# Bake the recipe
Item_recipe <- recipe(sales~., data=storeItem) %>%
  step_date(date, features="month") %>%
  step_date(date, features="dow") %>%
  step_date(date, features="doy") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))

prep <- prep(Item_recipe)
bake(prep, new_data=storeItem)

# KNN setup(model and workflow)
knn_model <- nearest_neighbor(neighbors=tune()) %>% # set or tune
  set_mode("regression") %>%
  set_engine("kknn")

knn_wf <- workflow() %>%
  add_recipe(Item_recipe) %>%
  add_model(knn_model)

# Set up tuning grid and folds
folds <- vfold_cv(storeItem, v = 5, repeats=1)

knn_tuning_grid <- grid_regular(neighbors(),
                                levels = 5)

# Tune neighbors here
knn_cv <- knn_wf %>%
  tune_grid(resamples=folds,
            grid=knn_tuning_grid,
            metrics=metric_set(smape))

# Find the best Fit
bestTune <- knn_cv %>%
  select_best("smape")

# Collect the best tune and find the average
collect_metrics(knn_cv) %>% 
  filter(neighbors == bestTune$neighbors, .config == bestTune$.config) %>%
  pull(mean)


# Exponential Smoothing ---------------------------------------------------
library(tidyverse)
library(tidymodels)
library(embed)
library(vroom)
library(patchwork)
library(modeltime) #Extensions of tidymodels to time series
library(timetk) #Some nice time series functions
library(ggplot2)

# Read in the data
setwd("/Users/student/Desktop/STAT348/ItemDemandChallenge")
Item_training <-vroom("/Users/student/Desktop/STAT348/ItemDemandChallenge/train.csv")
Item_test <- vroom("/Users/student/Desktop/STAT348/ItemDemandChallenge/test.csv")

# Cross-Validation Set-up
train1 <- Item_training %>% filter(store==3, item==17)

cv_split1 <- time_series_split(train1, assess="3 months", cumulative = TRUE)

cv_split1 %>%
tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

es_model1 <- exp_smoothing() %>%
set_engine("ets") %>%
fit(sales~date, data=training(cv_split1))

## Cross-validate to tune model
cv_results1 <- modeltime_calibrate(es_model1,new_data = testing(cv_split1))

## Visualize CV results
CVResult1 <- cv_results1 %>%
modeltime_forecast(new_data = testing(cv_split1),actual_data = train1) %>%
plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results1 %>%
modeltime_accuracy() %>%
table_modeltime_accuracy(.interactive = FALSE)

## Refit to all data then forecast
es_fullfit1 <- cv_results1 %>%
modeltime_refit(data = train1)

es_preds1 <- es_fullfit1 %>%
modeltime_forecast(h = "3 months") %>%
rename(date=.index, sales=.value) %>%
select(date, sales) %>%
full_join(., y=Item_test, by="date") %>%
select(id, sales)

actual_prediction1 <- es_fullfit1 %>%
modeltime_forecast(h = "3 months", actual_data = train1) %>%
plot_modeltime_forecast(.interactive=TRUE)

# Cross-Validation Set-up
train2 <- Item_training %>% filter(store==4, item==25)

cv_split2 <- time_series_split(train2, assess="3 months", cumulative = TRUE)

cv_split2 %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

es_model2 <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data=training(cv_split2))

## Cross-validate to tune model
cv_results2 <- modeltime_calibrate(es_model2,new_data = testing(cv_split2))

## Visualize CV results
CVresult2 <- cv_results2 %>%
  modeltime_forecast(new_data = testing(cv_split2),actual_data = train2) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results2 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

## Refit to all data then forecast
es_fullfit2 <- cv_results2 %>%
  modeltime_refit(data = train2)

es_preds2 <- es_fullfit2 %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=Item_test, by="date") %>%
  select(id, sales)

actual_prediction2 <- es_fullfit2 %>%
  modeltime_forecast(h = "3 months", actual_data = train2) %>%
  plot_modeltime_forecast(.interactive=TRUE)


plot <- plotly::subplot(CVResult1, CVresult2, actual_prediction1, actual_prediction2, nrows=2)
plot


# ARIMA model -------------------------------------------------------------
# library
library(tidyverse)
library(tidymodels)
library(embed)
library(vroom)
library(patchwork)
library(forecast)
library(timetk)
library(modeltime)

# Read in the data and filter to store
setwd("/Users/student/Desktop/STAT348/ItemDemandChallenge")
Item_training <-vroom("/Users/student/Desktop/STAT348/ItemDemandChallenge/train.csv")
Item_test <- vroom("/Users/student/Desktop/STAT348/ItemDemandChallenge/test.csv")

storeItem <- Item_training %>%
  filter(store==5, item==7)
storeItemtest <- Item_test %>%
  filter(store==5, item==7)

storeItem2 <- Item_training %>%
  filter(store==8, item==35)
storeItemtest2 <- Item_test %>%
  filter(store==8, item==35)

# Create the CV split for time series
cv_split1 <- time_series_split(storeItem, assess="3 months", cumulative = TRUE)
cv_split2 <- time_series_split(storeItem2, assess="3 months", cumulative = TRUE)

cv_split1 %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

cv_split2 %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

# Bake the recipe
arima_recipe <- recipe(sales~., data=storeItem) %>%
  step_date(date, features="month") %>%
  step_date(date, features="dow") %>%
  step_date(date, features="doy") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))

prep <- prep(arima_recipe)
bake(prep, new_data=storeItem)
bake(prep, new_data=storeItemtest)
bake(prep, new_data=storeItem2)
bake(prep, new_data=storeItemtest2)

# Setting up an arima model and the workflow
## 1) model
arima_model <- arima_reg(seasonal_period=365,
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2 #default max D to tune
                         ) %>%
  set_engine("auto_arima")

# model <- arima_model %>%
#   set_engine("auto_arima") %>%
#   fit(sales~date, data=training(cv_split))

## 3) workflow
arima_wf1 <- workflow() %>%
add_recipe(arima_recipe) %>%
add_model(arima_model) %>%
fit(data=training(cv_split1))

arima_wf2 <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split2))

# Cross-validate to tune model
cv_results1 <- modeltime_calibrate(arima_wf1,new_data = testing(cv_split1))
cv_results2 <- modeltime_calibrate(arima_wf2,new_data = testing(cv_split2))

# Visualize & Evaluate CV accuracy
## Visualize CV results
sarima_CVResult_plot1 <- cv_results1 %>%
  modeltime_forecast(new_data = testing(cv_split1),actual_data = storeItem) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results1 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

## Refit to all data then forecast
arima_fullfit1 <- cv_results1 %>%
  modeltime_refit(data = storeItem)

arima_preds1 <- arima_fullfit1 %>%
  modeltime_forecast(new_data = storeItemtest) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=storeItemtest, by="date") %>%
  select(id, sales)

sarima_actual_prediction_plot1 <- arima_fullfit1 %>%
  modeltime_forecast(new_data = storeItemtest, actual_data = storeItem) %>%
  plot_modeltime_forecast(.interactive=TRUE)

# For the second model ----------------------------------------------------

# Cross-validate to tune model
cv_results2 <- modeltime_calibrate(arima_wf2,new_data = testing(cv_split2))

# Visualize & Evaluate CV accuracy
## Visualize CV results
sarima_CVResult2_plot2 <- cv_results2 %>%
  modeltime_forecast(new_data = testing(cv_split2),actual_data = storeItem2) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results2 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

## Refit to all data then forecast
arima_fullfit2 <- cv_results2 %>%
  modeltime_refit(data = storeItem2)

arima_preds2 <- arima_fullfit2 %>%
  modeltime_forecast(new_data = storeItemtest2) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=storeItemtest2, by="date") %>%
  select(id, sales)

sarima_actual_prediction_plot2 <- arima_fullfit2 %>%
  modeltime_forecast(new_data = storeItemtest2, actual_data = storeItem2) %>%
  plot_modeltime_forecast(.interactive=TRUE)

sarima_plot <- plotly::subplot(sarima_CVResult_plot1, sarima_CVResult2_plot2, sarima_actual_prediction_plot1, sarima_actual_prediction_plot2, nrows=2)
sarima_plot

# Facebook Prophet model --------------------------------------------------
# library
library(tidyverse)
library(tidymodels)
library(embed)
library(vroom)
library(patchwork)
library(forecast)
library(timetk)
library(modeltime)

# Read in the data and filter to store
Item_training <-vroom("/Users/student/Desktop/STAT348/ItemDemandChallenge/train.csv")
Item_test <- vroom("/Users/student/Desktop/STAT348/ItemDemandChallenge/test.csv")

storeItem <- Item_training %>%
  filter(store==5, item==7)
storeItemtest <- Item_test %>%
  filter(store==5, item==7)

storeItem2 <- Item_training %>%
  filter(store==8, item==35)
storeItemtest2 <- Item_test %>%
  filter(store==8, item==35)

# CV setup
cv_split1 <- time_series_split(storeItem, assess="3 months", cumulative = TRUE)
cv_split2 <- time_series_split(storeItem2, assess="3 months", cumulative = TRUE)

cv_split1 %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

cv_split2 %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

# Bake the recipe
recipe <- recipe(sales~., data=storeItem) %>%
  step_date(date, features="month") %>%
  step_date(date, features="dow") %>%
  step_date(date, features="doy") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))

prep <- prep(recipe)
bake(prep, new_data=storeItem)
bake(prep, new_data=storeItemtest)
bake(prep, new_data=storeItem2)
bake(prep, new_data=storeItemtest2)

# Prophet model and setting up the workflow
prophet_model1 <- prophet_reg() %>%
set_engine(engine = "prophet") %>%
fit(sales ~ date, data = training(cv_split))

prophet_model2 <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split2))

# prophet_wf <- workflow() %>%
#   add_recipe(recipe) %>%
#   add_model(prophet_model) %>%
#   fit(data=training(cv_split))

# Cross-validate to tune model
cv_results1 <- modeltime_calibrate(prophet_model1,new_data = testing(cv_split1))

# Visualize & Evaluate CV accuracy
## Visualize CV results
CVResult_plot1_pro <- cv_results1 %>%
  modeltime_forecast(new_data = testing(cv_split1),actual_data = storeItem) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results1 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

## Refit to all data then forecast
arima_fullfit1 <- cv_results1 %>%
  modeltime_refit(data = storeItem)

arima_preds1 <- arima_fullfit1 %>%
  modeltime_forecast(new_data = storeItemtest) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=storeItemtest, by="date") %>%
  select(id, sales)

actual_prediction_plot1_pro <- arima_fullfit1 %>%
  modeltime_forecast(new_data = storeItemtest, actual_data = storeItem) %>%
  plot_modeltime_forecast(.interactive=TRUE)


# For the second  ----------------------------------------------------
## Visualize CV results
cv_results2 <- modeltime_calibrate(prophet_model2,new_data = testing(cv_split2))

CVResult_plot2_pro <- cv_results2 %>%
  modeltime_forecast(new_data = testing(cv_split2),actual_data = storeItem2) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results2 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

## Refit to all data then forecast
arima_fullfit2 <- cv_results2 %>%
  modeltime_refit(data = storeItem2)

arima_preds2 <- arima_fullfit2 %>%
  modeltime_forecast(new_data = storeItemtest2) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=storeItemtest2, by="date") %>%
  select(id, sales)

actual_prediction_plot2_pro <- arima_fullfit2 %>%
  modeltime_forecast(new_data = storeItemtest2, actual_data = storeItem2) %>%
  plot_modeltime_forecast(.interactive=TRUE)

prophet_plot <- plotly::subplot(CVResult_plot1_pro, CVResult_plot2_pro, actual_prediction_plot1_pro, actual_prediction_plot2_pro, nrows=2)
prophet_plot


