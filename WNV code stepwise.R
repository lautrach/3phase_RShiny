##--- Load the R file which does Phase 1 of the model and outputs sensitivity 
##--- and specificity scores
##--- Phase1Results.csv is the saved file from the below R code. 
##--- The Phase 1 code takes time to run. 

#load("WNV Phase1.R")

##--- Load the R file which does Phase 2 of the model and makes the final 
##--- data frame with the scores
##--- trapsensAug2022.rds is the saved file which contains the dataframe with 
##--- the scores for the T* traps (326 traps with sens scores)
##--- trapAug2022.rds is the saved file which contains the dataframe with 
##--- specificity for all traps (1062 traps)

load("WNV Phase2.R")

##--- Load the R file which plots the results of Phase 1 and 2 of the model 

load("Plots from Phase2.R")

##--- Load the R file which creates the data for Phase 3 with all the features
##--- and does an exploratory data analysis via plots
##--- "Data for score regression.csv" is the saved file which contains the 
##--- dataframe with all the features and "Processed X for Score regression.csv"
##--- is the saved file which contains the final preprocessed variables 
##--- (only X, no reponse)

load("Data and EDA for Phase3.R")

##--- Load the R file which build models to predict the score using the features 
##--- and plots feature importance for all the variables
##--- Tool to build R-Shiny app on 
load("Model to predict scores Phase3 tool.R")

##--- Load the R file which plots ADRF plots to analyse causal effect
load("WNV Phase3.R")
