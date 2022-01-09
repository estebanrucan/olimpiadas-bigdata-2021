require(tidyverse)
require(caret)
require(rpart)
require(rpart.plot)
require(rsample)
require(recipes)
require(themis)
require(yardstick)

data_raw <- read_csv("data/fraude.csv")
data <- data_raw %>% 
    select(-isFlaggedFraud, -nameOrig, -nameDest) %>% 
    mutate(isFraud = factor(isFraud))

GGally::ggpairs(data, aes(color = isFraud))

# Train/Test Split
set.seed(2021)
initialsplit <- initial_split(data, strata = isFraud)
data_train   <- training(initialsplit)
data_testing <- testing(initialsplit)

receta <- recipe(isFraud ~ ., data = data_train) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_smote(isFraud)

modelo <- train(
    receta,
    data       = data_train,
    method     = "rpart",
    tuneLength = 10,
    control    = rpart.control(maxdepth = 5, minsplit = 10),
    trControl  = trainControl(method = "cv")
)

ggplot(modelo)

resumen <- tibble(
    y_pred = predict(modelo, newdata = data_testing),
    y_obs  = data_testing$isFraud
) %>% 
    bind_cols(
        predict(modelo, newdata = data_testing, type = "probs")
    )

resumen %>% conf_mat(y_obs, y_pred) 
resumen %>% conf_mat(y_obs, y_pred) %>% summary() 
resumen %>% roc_curve(y_obs, No) %>% autoplot()
modelo$finalModel %>% 
    rpart.plot(box.palette = "GnYlRd", cex = 0.5)

roc_curve()