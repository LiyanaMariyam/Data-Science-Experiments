# XGBoost 
XGBoost is an implementation of the Gradient Boosted Decision Trees algorithm. 
XGBoost stands for eXtreme Gradient Boosting. 
It is an especialy efficent implimentation of gradient boosting.

Gradient Boosted Decision Trees use an ensemble of models. 
Cycles that repeatedly builds new models are combined into an ensemble model.
Starting from an existing model which could be pretty naive, 
errors for each observation in the dataset will be calculated.
Then a new model is built to predict these errors and
these predictions are then added to the ensemble of models.
