# Natural computing final
This is the final project repository of Bauke Brenninkmeijer and Ties Robroek. The project is about using natural computing related classifiers for anomaly detection.
This project is originally a GECCO challenge, found [http://www.spotseven.de/gecco/gecco-challenge/gecco-challenge-2018](http://www.spotseven.de/gecco/gecco-challenge/gecco-challenge-2018)

# How to run
To get some sense of the project is quite easy. The different detectors we created are in the detectors directory. To get immediate results, the EvaluationMain.R file can be run.
This runs all the detectors on the train data with a 2/3 train 1/3 test split. The results will be given in true positive, false positive, true negative and false negative, as well as the corresponding F1-score.
Additionally, the boostingEnsemble.R file can be run for a 10-fold cross validation score of Adaboost, XGBoost and an ensemble of them. 
