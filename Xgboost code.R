
install.packages('xgboost')
library(xgboost) 
library(tidyverse)
diseaseInfo <- read_csv("C:/Users/USER/Documents/Liyana/S4/Data Science/Outbreak_240817.csv")
set.seed(1234)
diseaseInfo <- diseaseInfo[sample(1:nrow(diseaseInfo)), ]
head(diseaseInfo)
diseaseInfo_humansRemoved <- diseaseInfo %>%
  select(-starts_with("human"))
diseaseLabels <- diseaseInfo %>%
  select(humansAffected) %>%
  is.na() %>%
  magrittr::not()
head(diseaseLabels)
head(diseaseInfo$humansAffected)
diseaseInfo_numeric <- diseaseInfo_humansRemoved %>%
  select(-Id) %>% 
  select(-c(longitude, latitude)) %>% 
  select_if(is.numeric)
str(diseaseInfo_numeric)
head(diseaseInfo$country)
model.matrix(~country-1,head(diseaseInfo))
region <- model.matrix(~country-1,diseaseInfo)
head(diseaseInfo$speciesDescription)
diseaseInfo_numeric$is_domestic <- str_detect(diseaseInfo$speciesDescription, "domestic")
speciesList <- diseaseInfo$speciesDescription %>%
  str_replace("[[:punct:]]", "") %>% 
  str_extract("[a-z]*$")
speciesList <- tibble(species = speciesList)
options(na.action='na.pass') 
species <- model.matrix(~species-1,speciesList)
diseaseInfo_numeric <- cbind(diseaseInfo_numeric, region, species)
diseaseInfo_matrix <- data.matrix(diseaseInfo_numeric)
numberOfTrainingSamples <- round(length(diseaseLabels) * .7)
train_data <- diseaseInfo_matrix[1:numberOfTrainingSamples,]
train_labels <- diseaseLabels[1:numberOfTrainingSamples]
test_data <- diseaseInfo_matrix[-(1:numberOfTrainingSamples),]
test_labels <- diseaseLabels[-(1:numberOfTrainingSamples)]
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
model <- xgboost(data = dtrain, 
                 nround = 2, 
                 objective = "binary:logistic")  
pred <- predict(model, dtest)
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))
model_tuned <- xgboost(data = dtrain,          
                       max.depth = 3, 
                       nround = 2, 
                       objective = "binary:logistic") 
pred <- predict(model_tuned, dtest)
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))
negative_cases <- sum(train_labels == FALSE)
postive_cases <- sum(train_labels == TRUE)
model_tuned <- xgboost(data = dtrain,       
                       max.depth = 3, 
                       nround = 10,
                       early_stopping_rounds = 3, 
                       objective = "binary:logistic", 
                       scale_pos_weight = negative_cases/postive_cases)

pred <- predict(model_tuned, dtest)
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))
model_tuned <- xgboost(data = dtrain,       
                       max.depth = 3, 
                       nround = 10,
                       early_stopping_rounds = 3, 
                       objective = "binary:logistic",
                       scale_pos_weight = negative_cases/postive_cases, 
                       gamma = 1)
pred <- predict(model_tuned, dtest)
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))
xgb.plot.multi.trees(feature_names = names(diseaseInfo_matrix), 
                     model = model)
odds_to_probs <- function(odds){
  return(exp(odds)/ (1 + exp(odds)))
}
odds_to_probs(-0.599)
importance_matrix <- xgb.importance(names(diseaseInfo_matrix), model = model)
xgb.plot.importance(importance_matrix)













