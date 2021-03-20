
INPUT:

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


OUTPUT:

> library(tidyverse)
-- Attaching packages ------------------------------------------------- tidyverse 1.3.0 --
  v ggplot2 3.3.2     v purrr   0.3.4
v tibble  3.0.4     v dplyr   1.0.2
v tidyr   1.1.2     v stringr 1.4.0
v readr   1.4.0     v forcats 0.5.0
-- Conflicts ---------------------------------------------------- tidyverse_conflicts() --
  x dplyr::filter() masks stats::filter()
x dplyr::lag()    masks stats::lag()
x dplyr::slice()  masks xgboost::slice()
> diseaseInfo <- read_csv("C:/Users/USER/Documents/Liyana/S4/Data Science/Outbreak_240817.csv")

-- Column specification ------------------------------------------------------------------
  cols(
    .default = col_character(),
    Id = col_double(),
    latitude = col_double(),
    longitude = col_double(),
    sumAtRisk = col_double(),
    sumCases = col_double(),
    sumDeaths = col_double(),
    sumDestroyed = col_double(),
    sumSlaughtered = col_double(),
    humansAge = col_double(),
    humansAffected = col_double(),
    humansDeaths = col_double()
  )
i Use `spec()` for the full column specifications.

> set.seed(1234)
> diseaseInfo <- diseaseInfo[sample(1:nrow(diseaseInfo)), ]
> head(diseaseInfo)
# A tibble: 6 x 24
Id source latitude longitude region country admin1 localityName localityQuality
<dbl> <chr>     <dbl>     <dbl> <chr>  <chr>   <chr>  <chr>        <chr>          
  1 219318 Natio~     28.4     46.0  Asia   Saudi ~ Easte~ Hafr-Elbatin Unknown        
2 219097 OIE        45.6     11.4  Europe Italy   Veneto CASTELGOMBE~ Exact          
3 219828 OIE        52.4     23.2  Europe Poland  Podla~ Adamowo      Unknown        
4 221042 OIE        36.6     10.7  Africa Tunisia Nabeul Beni khalled Exact          
5 217753 OIE        46.1      4.43 Europe France  Rhone~ POULE LES E~ Unknown        
6 228469 OIE        35.3    129.   Asia   Republ~ Kyong~ Gijang-gun   Exact          
# ... with 15 more variables: observationDate <chr>, reportingDate <chr>, status <chr>,
#   disease <chr>, serotypes <chr>, speciesDescription <chr>, sumAtRisk <dbl>,
#   sumCases <dbl>, sumDeaths <dbl>, sumDestroyed <dbl>, sumSlaughtered <dbl>,
#   humansGenderDesc <chr>, humansAge <dbl>, humansAffected <dbl>, humansDeaths <dbl>
> diseaseInfo_humansRemoved <- diseaseInfo %>%
  +   select(-starts_with("human"))
> diseaseLabels <- diseaseInfo %>%
  +   select(humansAffected) %>%
  +   is.na() %>%
  +   magrittr::not()
> head(diseaseLabels)
humansAffected
[1,]           TRUE
[2,]          FALSE
[3,]          FALSE
[4,]          FALSE
[5,]          FALSE
[6,]          FALSE
> head(diseaseInfo$humansAffected)
[1]  1 NA NA NA NA NA
> diseaseInfo_numeric <- diseaseInfo_humansRemoved %>%
  +   select(-Id) %>% 
  +   select(-c(longitude, latitude)) %>% 
  +   select_if(is.numeric)
> str(diseaseInfo_numeric)
tibble [17,008 x 5] (S3: tbl_df/tbl/data.frame)
$ sumAtRisk     : num [1:17008] NA 53 NA 61 93 12 103 49 13 NA ...
$ sumCases      : num [1:17008] NA 4 1 1 1 NA 1 9 10 1 ...
$ sumDeaths     : num [1:17008] NA 0 1 0 0 6 NA 0 10 0 ...
$ sumDestroyed  : num [1:17008] NA 0 0 0 0 6 NA 0 3 1 ...
$ sumSlaughtered: num [1:17008] NA 0 0 0 0 NA NA 0 0 0 ...
> head(diseaseInfo$country)
[1] "Saudi Arabia"      "Italy"             "Poland"            "Tunisia"          
[5] "France"            "Republic of Korea"
> model.matrix(~country-1,head(diseaseInfo))
countryFrance countryItaly countryPoland countryRepublic of Korea countrySaudi Arabia
1             0            0             0                        0                   1
2             0            1             0                        0                   0
3             0            0             1                        0                   0
4             0            0             0                        0                   0
5             1            0             0                        0                   0
6             0            0             0                        1                   0
countryTunisia
1              0
2              0
3              0
4              1
5              0
6              0
attr(,"assign")
[1] 1 1 1 1 1 1
attr(,"contrasts")
attr(,"contrasts")$country
[1] "contr.treatment"

> region <- model.matrix(~country-1,diseaseInfo)
> head(diseaseInfo$speciesDescription)
[1] NA                                                 
[2] "domestic, cattle"                                 
[3] "wild, wild boar"                                  
[4] "domestic, cattle, domestic, goat, domestic, sheep"
[5] "domestic, cattle"                                 
[6] "domestic, unspecified bird"                       
> diseaseInfo_numeric$is_domestic <- str_detect(diseaseInfo$speciesDescription, "domestic")
> speciesList <- diseaseInfo$speciesDescription %>%
  +   str_replace("[[:punct:]]", "") %>% 
  +   str_extract("[a-z]*$")
> speciesList <- tibble(species = speciesList)
> options(na.action='na.pass') 
> species <- model.matrix(~species-1,speciesList)
> diseaseInfo_numeric <- cbind(diseaseInfo_numeric, region, species)
> diseaseInfo_matrix <- data.matrix(diseaseInfo_numeric)
> numberOfTrainingSamples <- round(length(diseaseLabels) * .7)
> train_data <- diseaseInfo_matrix[1:numberOfTrainingSamples,]
> train_labels <- diseaseLabels[1:numberOfTrainingSamples]
> test_data <- diseaseInfo_matrix[-(1:numberOfTrainingSamples),]
> test_labels <- diseaseLabels[-(1:numberOfTrainingSamples)]
> dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
> dtest <- xgb.DMatrix(data = test_data, label= test_labels)
> model <- xgboost(data = dtrain, 
                   +                  nround = 2, 
                   +                  objective = "binary:logistic")  
[19:18:05] WARNING: amalgamation/../src/learner.cc:1061: Starting in XGBoost 1.3.0, the default evaluation metric used with the objective 'binary:logistic' was changed from 'error' to 'logloss'. Explicitly set eval_metric if you'd like to restore the old behavior.
[1]	train-logloss:0.448445 
[2]	train-logloss:0.313408 
> pred <- predict(model, dtest)
> err <- mean(as.numeric(pred > 0.5) != test_labels)
> print(paste("test-error=", err))
[1] "test-error= 0.0139161113288906"
> model_tuned <- xgboost(data = dtrain,          
+                        max.depth = 3, 
+                        nround = 2, 
+                        objective = "binary:logistic") 
[19:18:19] WARNING: amalgamation/../src/learner.cc:1061: Starting in XGBoost 1.3.0, the default evaluation metric used with the objective 'binary:logistic' was changed from 'error' to 'logloss'. Explicitly set eval_metric if you'd like to restore the old behavior.
[1]	train-logloss:0.448445 
[2]	train-logloss:0.313408 
> pred <- predict(model_tuned, dtest)
> err <- mean(as.numeric(pred > 0.5) != test_labels)
> print(paste("test-error=", err))
[1] "test-error= 0.0139161113288906"
> negative_cases <- sum(train_labels == FALSE)
> postive_cases <- sum(train_labels == TRUE)
> model_tuned <- xgboost(data = dtrain,       
                         +                        max.depth = 3, 
                         +                        nround = 10,
                         +                        early_stopping_rounds = 3, 
                         +                        objective = "binary:logistic", 
                         +                        scale_pos_weight = negative_cases/postive_cases)
[19:18:19] WARNING: amalgamation/../src/learner.cc:1061: Starting in XGBoost 1.3.0, the default evaluation metric used with the objective 'binary:logistic' was changed from 'error' to 'logloss'. Explicitly set eval_metric if you'd like to restore the old behavior.
[1]	train-logloss:0.446741 
Will train until train_logloss hasn't improved in 3 rounds.

[2]	train-logloss:0.311951 
[3]	train-logloss:0.228264 
[4]	train-logloss:0.173352 
[5]	train-logloss:0.136227 
[6]	train-logloss:0.110756 
[7]	train-logloss:0.093110 
[8]	train-logloss:0.080865 
[9]	train-logloss:0.072262 
[10]	train-logloss:0.066267 
> 
  > pred <- predict(model_tuned, dtest)
> err <- mean(as.numeric(pred > 0.5) != test_labels)
> print(paste("test-error=", err))
[1] "test-error= 0.0139161113288906"
> model_tuned <- xgboost(data = dtrain,       
                         +                        max.depth = 3, 
                         +                        nround = 10,
                         +                        early_stopping_rounds = 3, 
                         +                        objective = "binary:logistic",
                         +                        scale_pos_weight = negative_cases/postive_cases, 
                         +                        gamma = 1)
[19:18:19] WARNING: amalgamation/../src/learner.cc:1061: Starting in XGBoost 1.3.0, the default evaluation metric used with the objective 'binary:logistic' was changed from 'error' to 'logloss'. Explicitly set eval_metric if you'd like to restore the old behavior.
[1]	train-logloss:0.446741 
Will train until train_logloss hasn't improved in 3 rounds.

[2]	train-logloss:0.311927 
[3]	train-logloss:0.228243 
[4]	train-logloss:0.173355 
[5]	train-logloss:0.136232 
[6]	train-logloss:0.110765 
[7]	train-logloss:0.093121 
[8]	train-logloss:0.080876 
[9]	train-logloss:0.072275 
[10]	train-logloss:0.066280 
> pred <- predict(model_tuned, dtest)
> err <- mean(as.numeric(pred > 0.5) != test_labels)
> print(paste("test-error=", err))
[1] "test-error= 0.0139161113288906"
> xgb.plot.multi.trees(feature_names = names(diseaseInfo_matrix), 
                       +                      model = model)
Column 2 ['No'] of item 2 is missing in item 1. Use fill=TRUE to fill with NA (NULL for list columns), or use.names=FALSE to ignore column names. use.names='check' (default from v1.12.2) emits this message and proceeds as if use.names=FALSE for  backwards compatibility. See news item 5 in v1.12.2 for options to control this message.
> odds_to_probs <- function(odds){
  +   return(exp(odds)/ (1 + exp(odds)))
  + }
> odds_to_probs(-0.599)
[1] 0.3545725
> importance_matrix <- xgb.importance(names(diseaseInfo_matrix), model = model)
> xgb.plot.importance(importance_matrix)










