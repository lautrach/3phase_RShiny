#Note for Anwesha: Refer to  Regression of scores.R

library(data.table)
library(randomForest)
library(tidyverse)
library(spaMM)
library(caret)
library(rpart)
library(rpart.plot)

actual_dat = fread("Data for score regression.csv")
actual_dat.sens = na.omit(actual_dat)

# Defining the response
y = actual_dat.sens$score

# Loading in the pre preprocessed covariates
X = fread("Processed X for Score regression.csv")
X$MajorLC = as.factor(X$MajorLC)
X_dat = model.matrix(y~.,data = X)

#----------------- Variables in X --------------------------------------------------------------

# 1) n: Number of pools collected
# 2) num_positive: Number of positive pools
# 3) MajorLC: Major Land Coverage 
# 4) AVG_IMP: Average impervious land
# 5) AVG_CANPY: Average canopy coverage
# 6) TPOP2020: Total population
# 7) HSgradPct: Percent high school graduates 
# 8) FamPovPct: Percent of families in poverty
# 9) Pct_HUOcc: Percentage of occupied housing units
# 10) ClassBest_Pct: Percentage coverage of best type (xx) of landscape
# 11) Class_xxPct: Percentage coverage of each type (xx) of landscape
# 12) Black_Perc: Percentage Black population
# 13) White_Perc: Percentage White population
# 14) HispOrTwo_Perc: Percentage Hispanic or population belonging to two or more races
# 15) perc_positive: Percentage of positive pools


# ------------------------- Feature Selection for Random Forests Modelling -----------------------------
# Finding features of importance using 5-fold CV
set.seed(2910)
cv_mod <- replicate(5, rfcv(X_dat, y, cv.fold=5, step=1.5), simplify=FALSE)
error.cv <- sapply(cv_mod, "[[", "error.cv")
error.cv = apply(error.cv,1,mean)
plot(as.numeric(names(error.cv)),error.cv, type="o", lwd=2)
names(error.cv)[which.min(error.cv)]

# Results show top 15 features to be considered. 
tree_mod = randomForest(y~.,data = data.frame("y" = y,X_dat))
varImpPlot(tree_mod)

#-------------------------- Function to calculate CV error for different models --------
# Building a function for computing cv error for different methods
cv_error = function(X,y){
  cv_set = sample(1:5,325,replace=T)
  cv_error_rf = numeric(5)
  cv_error_lm = numeric(5)
  cv_error_spat = numeric(5)
  cv_error_base = numeric(5)
  cv_error_ens = numeric(5)
  
  for(cv in 1:5){
    X_train = X[cv_set != cv,]
    y_train = y[cv_set != cv]
    X_test = X[cv_set == cv,]
    y_test = y[cv_set == cv]
    
    ## Base Model where predictions are random ################################
    pred_base = runif(length(y_test),0,1)
    cv_error_base[cv]= mean((pred_base - y_test)^2)
    
    ### Linear Model ##########################################################
    X_lm_train = model.matrix(y_train~.,data = X_train)
    X_lm_test = model.matrix(y_test~.,data = X_test)
    mod_lm = lm(y_train~.-1,data =  data.frame("y_train" = y_train,X_lm_train))
    pred = predict(mod_lm, newdata = data.frame(X_lm_test))
    cv_error_lm[cv]= mean((pred - y_test)^2)
    
    ### Spatial Linear Model ###################################################
    coords = actual_dat.sens %>% select(c(long,lat))
    coords_train = coords[cv_set != cv,]
    coords_test = coords[cv_set == cv,]
    X_spatial_train = cbind.data.frame(X_train,coords_train)
    X_spatial_test = cbind.data.frame(X_test,coords_test)
    
    spat_mod = fitme(score~n+perc_positive+MajorLC+AVG_IMP+AVG_CANPY+TPOP2020+White_Perc+
                       Black_Perc+HSgradPct+
                       FamPovPct+HispOrTwo_Perc+ClassBest_Pct+Class_21Pct+Class_22Pct+
                       Class_23Pct+Class_24Pct+Pct_HUocc+
                       Matern(1|lat+long),
                     data=data.frame("score" = y_train,X_spatial_train),method="REML")
    pred_spat = predict(spat_mod,newdata = X_spatial_test)
    cv_error_spat[cv]= mean((pred_spat - y_test)^2)
    
    ### Random Forests #########################################################
    X_train_rf = X_train %>% select(-c("MajorLC"))
    X_test_rf = X_test %>% select(-c("MajorLC"))
    
    # Tuning
    tuning = list()
    ntree = c(10,50,100,500,1000)
    for(i in 1:5){
      tuning[[i]] = tuneRF(X_train_rf, y_train, ntreeTry=ntree[i],
                           stepFactor=2, improve=0.05,trace = F,plot = F)
    }
    
    tuning = do.call(rbind,lapply(1:5,function(t){
      cbind.data.frame(tuning[[t]],"ntree" = rep(ntree[t],dim(tuning[[t]])[1]))
    }))
    mtry_tune = tuning$mtry[which.min(tuning$OOBError)]
    ntree_tune = tuning$ntree[which.min(tuning$OOBError)]
    
    # Fitting the tree
    tree_mod = randomForest(y~.,xtest = X_test_rf,ytest = y_test,
                            data = data.frame("y" = y_train,X_train_rf),
                            mtry = mtry_tune,ntree=ntree_tune)
    cv_error_rf[cv] = mean((tree_mod$test$predicted - y_test)^2)
    
    # Ensembling through weighted mean of results
    ensemble_prediction = (0.8*tree_mod$test$predicted + 0.2*pred_spat)
    cv_error_ens[cv] = mean((ensemble_prediction - y_test)^2)
  }
  return(list("CV Error RF" = mean(cv_error_rf),"CV Error LM" = mean(cv_error_lm),
              "CV Error Spatial" = mean(cv_error_spat),
              "CV Error Ensemble" = mean(cv_error_ens),
              "CV Error Base" = mean(cv_error_base)))
}

set.seed(2609)
error_models = cv_error(X,y)


#################### Making the actual model ###################################

#-------------------- Random Forest -------------------------------------------
tuning = list()
ntree = c(10,50,100,500,1000)
for(i in 1:5){
  tuning[[i]] = tuneRF(X, y, ntreeTry=ntree[i],
                       stepFactor=2, improve=0.05,plot = F,trace = F)
}

tuning = do.call(rbind,lapply(1:5,function(t){
  cbind.data.frame(tuning[[t]],"ntree" = rep(ntree[t],dim(tuning[[t]])[1]))
}))
mtry_tune = tuning$mtry[which.min(tuning$OOBError)]
ntree_tune = tuning$ntree[which.min(tuning$OOBError)]
tree_mod = randomForest(y~.,data = data.frame("y" = y,X),
                        mtry = mtry_tune,ntree=ntree_tune)
varImpPlot(tree_mod)

#-------------------- Linear Modelling -----------------------------------------
mod_lm = lm(y~.-1,data =  data.frame("y" = y,X_dat))
summary(mod_lm)

varimp_lm = varImp(mod_lm, scale = FALSE)
importanceOrder=order(varimp_lm$Overall)
dotchart(varimp_lm$Overal[importanceOrder],
         labels = rownames(varimp_lm)[importanceOrder],ann = T,
         main = "Linear Model",xlab = "abs(t-value)")

# -------------------- Spatial Modeliing ---------------------------------------
coords = actual_dat.sens %>% select(c(long,lat))
X_spatial = cbind.data.frame(X,coords)

spat_mod = fitme(score~n+num_positive+MajorLC+AVG_IMP+AVG_CANPY+TPOP2020+HSgradPct+
        FamPovPct+Pct_HUocc+ClassBest_Pct+Class_21Pct+Class_22Pct+
        Class_23Pct+Class_24Pct+Black_Perc+White_Perc+HispOrTwo_Perc+perc_positive+
        Matern(1|lat+long),
      data=data.frame("score" = y,X_spatial),method="REML")
varimp_spat = abs(summary(spat_mod)$beta_table[,3])
importanceOrder=order(varimp_spat)
dotchart(varimp_spat[importanceOrder],
         labels = rownames(varimp_spat)[importanceOrder],ann = T,
         main = "Spatial Linear Model",xlab = "abs(t-value)")




