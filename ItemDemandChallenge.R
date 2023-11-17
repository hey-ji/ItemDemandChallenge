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

