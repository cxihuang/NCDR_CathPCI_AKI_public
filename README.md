# NCDR_CathPCI_AKI

## Data extraction and feature engineering 
ncdrAKIextract_develop_cohort.R: for main analysis, development cohort  
ncdrAKIextract_update_cohort.R: for temporal validation, contemporary cohort  

## Develop and validate models  
ncdrAKImodel_xgboost_permutation.R: permutation selection for xgboost  
prediction_performance.R: function to calculate performance measures  
ncdrAKImodel_prediction_perfm_develop.R: develop in training and validate in test set for all models    
ncdrAKImodel_perfm_results_develop.R: summary of results  

## Temporal validation  
ncdrAKImodel_prediction_perfm_update.R: update models via 4 strategies on contemporary cohort  
ncdrAKImodel_perfm_results_update.R: summary of results  

## Miscellaneous  
xgboost_autogrid.R: non-exhaustive grid search for tuning xgboost  
