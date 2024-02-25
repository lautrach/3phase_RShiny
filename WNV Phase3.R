################################################################################
##################### Causal Inference  ########################################
################################################################################

# Section 1: GAM estimates for ADRF of variables causal effect on score
# Section 2: GAM estimates for ADRF with score = sensitivity
# Section 3: GAM estimates for ADRF with score = specificity
# Section 4: Different ways of estimating the ADRF
# Section 5: GAM estimates for ADRF with original score but only traps with population>10000

library(causaldrf)
library(data.table)
library(ggplot2)
library(tidyverse)

actual_dat = fread("Data for score regression.csv")
actual_dat.sens = na.omit(actual_dat)

# Defining the response
y = actual_dat.sens$score

# Loading in the pre preprocessed covariates
X = fread("Processed X for Score regression.csv")
X$MajorLC = as.factor(X$MajorLC)

variables = colnames(X)

# ------------------------------------------------------------------------------
# -----------Using Generalised Additive Models to estimate ADRF-----------------
# ------------------------------------------------------------------------------
# Experiments for the choice of estimates and results from other estimates can 
# be found below 


# a) Total Population
gam_list = gam_est(Y = y,treat = TPOP2020,
                   treat_formula = TPOP2020~1,
                   data.frame('y' = y,"TPOP2020" = X$TPOP2020),
                   grid_val = quantile(X$TPOP2020,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

#Empirical Bootstrap to find 95% CI:
params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y_boot,treat = TPOP2020,
                                treat_formula = TPOP2020~1,
                                data.frame('y_boot' = y_boot,"TPOP2020" = X_boot$TPOP2020),
                                grid_val = quantile(X$TPOP2020,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$TPOP2020,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("TPOP2020" = X$TPOP2020,y),
                                   aes(x = TPOP2020,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Total Population")+ylab("Score")+ggtitle("Estimate of ADRF")

# b) Avg_Imp
gam_list = gam_est(Y = y,treat = AVG_IMP,
                   treat_formula = AVG_IMP~TPOP2020,
                   data = data.frame('y' = y,"TPOP2020" = X$TPOP2020,"AVG_IMP" = X$AVG_IMP),
                   grid_val = quantile(X$AVG_IMP,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = AVG_IMP,
                                treat_formula = AVG_IMP~TPOP2020,
                                data = data.frame('y' = y_boot,
                                                  "TPOP2020" = X_boot$TPOP2020,
                                                  "AVG_IMP" = X_boot$AVG_IMP),
                                grid_val = quantile(X$AVG_IMP,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$AVG_IMP,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("AVG_IMP" = X$AVG_IMP,y),
                                   aes(x = AVG_IMP,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Impervious Land")+ylab("Score")+ggtitle("Estimate of ADRF")

# c) AVG_CANPY

gam_list = gam_est(Y = y,treat = AVG_CANPY,
                   treat_formula = AVG_CANPY~Perc_minority+ClassBest_Pct+Class_21Pct+
                     Class_22Pct+Class_23Pct+Class_24Pct,
                   data = data.frame('y' = y,
                                     "Perc_minority" = 100 - X$White_Perc,
                                     "AVG_CANPY" = X$AVG_CANPY,
                                     "ClassBest_Pct"=X$ClassBest_Pct,
                                     "Class_21Pct"=X$Class_21Pct,
                                     "Class_22Pct" = X$Class_22Pct,
                                     "Class_23Pct" = X$Class_23Pct,
                                     "Class_24Pct" = X$Class_24Pct),
                   grid_val = quantile(X$AVG_CANPY,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = AVG_CANPY,
                                treat_formula = AVG_CANPY~Perc_minority+ClassBest_Pct+Class_21Pct+
                                  Class_22Pct+Class_23Pct+Class_24Pct,
                                data = data.frame('y' = y_boot,
                                                  "Perc_minority" = 100 - X_boot$White_Perc,
                                                  "AVG_CANPY" = X_boot$AVG_CANPY,
                                                  "ClassBest_Pct"=X_boot$ClassBest_Pct,
                                                  "Class_21Pct"=X_boot$Class_21Pct,
                                                  "Class_22Pct" = X_boot$Class_22Pct,
                                                  "Class_23Pct" = X_boot$Class_23Pct,
                                                  "Class_24Pct" = X_boot$Class_24Pct),
                                grid_val = quantile(X$AVG_CANPY,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$AVG_CANPY,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("AVG_CANPY" = X$AVG_CANPY,y),
                                   aes(x = AVG_CANPY,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Canopy")+ylab("Score")+ggtitle("Estimate of ADRF")

# d) HSgradPct
gam_list = gam_est(Y = y,treat = HSgradPct,
                   treat_formula = HSgradPct~Perc_minority,
                   data = data.frame('y' = y,"Perc_minority" = 100 - X$White_Perc,
                                     "HSgradPct" = X$HSgradPct),
                   grid_val = quantile(X$HSgradPct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = HSgradPct,
                                treat_formula = HSgradPct~Perc_minority,
                                data = data.frame('y' = y_boot,"Perc_minority" = 100 - X_boot$White_Perc,
                                                  "HSgradPct" = X_boot$HSgradPct),
                                grid_val = quantile(X$HSgradPct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$HSgradPct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("HSgradPct" = X$HSgradPct,y),
                                   aes(x = HSgradPct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage High School graduates")+ylab("Score")+ggtitle("Estimate of ADRF")

# e) FamPovPct
gam_list = gam_est(Y = y,treat = FamPovPct,
                   treat_formula = FamPovPct~Perc_minority,
                   data = data.frame('y' = y,"Perc_minority" = 100 - X$White_Perc,
                                     "FamPovPct" = X$FamPovPct),
                   grid_val = quantile(X$FamPovPct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = FamPovPct,
                                treat_formula = FamPovPct~Perc_minority,
                                data = data.frame('y' = y_boot,"Perc_minority" = 100-X_boot$White_Perc,
                                                  "FamPovPct" = X_boot$FamPovPct),
                                grid_val = quantile(X$FamPovPct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$FamPovPct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("FamPovPct" = X$FamPovPct,y),
                                   aes(x = FamPovPct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Family Poverty")+ylab("Score")+ggtitle("Estimate of ADRF")

# f) Pct_HUocc
gam_list = gam_est(Y = y,treat = Pct_HUocc,
                   treat_formula = Pct_HUocc~Perc_minority+TPOP2020,
                   data = data.frame('y' = y,"Perc_minority" =100 - X$White_Perc,
                                     "Pct_HUocc" = X$Pct_HUocc,"TPOP2020" = X$TPOP2020),
                   grid_val = quantile(X$Pct_HUocc,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Pct_HUocc,
                                treat_formula = Pct_HUocc~Perc_minority+TPOP2020,
                                data = data.frame('y' = y_boot,"Perc_minority" = 100 - X_boot$White_Perc,
                                                  "Pct_HUocc" = X_boot$Pct_HUocc,"TPOP2020" = X_boot$TPOP2020),
                                grid_val = quantile(X$Pct_HUocc,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$Pct_HUocc,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Pct_HUocc" = X$Pct_HUocc,y),
                                   aes(x = Pct_HUocc,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Occupied Housing")+ylab("Score")+ggtitle("Estimate of ADRF")

# g) ClassBest_Pct
gam_list = gam_est(Y = y,treat = ClassBest_Pct,
                   treat_formula = ClassBest_Pct~AVG_IMP,
                   data = data.frame('y' = y,"AVG_IMP" = X$AVG_IMP,
                                     "ClassBest_Pct" = X$ClassBest_Pct),
                   grid_val = quantile(X$ClassBest_Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = ClassBest_Pct,
                                treat_formula = ClassBest_Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "ClassBest_Pct" = X_boot$ClassBest_Pct),
                                grid_val = quantile(X$ClassBest_Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$ClassBest_Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))


ggplot(data = plot_dat)+geom_point(data = data.frame("ClassBest_Pct" = X$ClassBest_Pct,y),
                                   aes(x = ClassBest_Pct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Developed Land")+ylab("Score")+ggtitle("Estimate of ADRF")


# h) Class_21Pct
gam_list = gam_est(Y = y,treat = Class_21Pct,
                   treat_formula = Class_21Pct~AVG_IMP,
                   data = data.frame('y' = y,"AVG_IMP" = X$AVG_IMP,
                                     "Class_21Pct" = X$Class_21Pct),
                   grid_val = quantile(X$Class_21Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Class_21Pct,
                                treat_formula = Class_21Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Class_21Pct" = X_boot$Class_21Pct),
                                grid_val = quantile(X$Class_21Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$Class_21Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Class_21Pct" = X$Class_21Pct,y),
                                   aes(x = Class_21Pct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Open Space")+ylab("Score")+ggtitle("Estimate of ADRF")


# i) Class_22Pct
gam_list = gam_est(Y = y,treat = Class_22Pct,
                   treat_formula = Class_22Pct~AVG_IMP,
                   data = data.frame('y' = y,"AVG_IMP" = X$AVG_IMP,
                                     "Class_22Pct" = X$Class_22Pct),
                   grid_val = quantile(X$Class_22Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Class_22Pct,
                                treat_formula = Class_22Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Class_22Pct" = X_boot$Class_22Pct),
                                grid_val = quantile(X$Class_22Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$Class_22Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Class_22Pct" = X$Class_22Pct,y),
                                   aes(x = Class_22Pct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Developed Low Density")+ylab("Score")+ggtitle("Estimate of ADRF")


# j) Class_23Pct
gam_list = gam_est(Y = y,treat = Class_23Pct,
                   treat_formula = Class_23Pct~AVG_IMP,
                   data = data.frame('y' = y,"AVG_IMP" = X$AVG_IMP,
                                     "Class_23Pct" = X$Class_23Pct),
                   grid_val = quantile(X$Class_23Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Class_23Pct,
                                treat_formula = Class_23Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Class_23Pct" = X_boot$Class_23Pct),
                                grid_val = quantile(X$Class_23Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$Class_23Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Class_23Pct" = X$Class_23Pct,y),
                                   aes(x = Class_23Pct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Developed Medium Density")+ylab("Score")+ggtitle("Estimate of ADRF")


# k) Class_24Pct
gam_list = gam_est(Y = y,treat = Class_24Pct,
                   treat_formula = Class_24Pct~AVG_IMP,
                   data = data.frame('y' = y,"AVG_IMP" = X$AVG_IMP,
                                     "Class_24Pct" = X$Class_24Pct),
                   grid_val = quantile(X$Class_24Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Class_24Pct,
                                treat_formula = Class_24Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Class_24Pct" = X_boot$Class_24Pct),
                                grid_val = quantile(X$Class_24Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$Class_24Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Class_24Pct" = X$Class_24Pct,y),
                                   aes(x = Class_24Pct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Developed High Density")+ylab("Score")+ggtitle("Estimate of ADRF")


# l) Perc_Minority
gam_list = gam_est(Y = y,treat = Perc_minority,
                   treat_formula = Perc_minority~TPOP2020,
                   data = data.frame('y' = y,"TPOP2020" = X$TPOP2020,
                                     "Perc_minority" = 100 - X$White_Perc),
                   grid_val = quantile(100 - X$White_Perc,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Perc_minority,
                                treat_formula = Perc_minority~TPOP2020,
                                data = data.frame('y' = y_boot,
                                                  "TPOP2020" = X_boot$TPOP2020,
                                                  "Perc_minority" = 100 - X_boot$White_Perc),
                                grid_val = quantile(100 - X$White_Perc,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(100 - X$White_Perc,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Perc_minority" = 100 - X$White_Perc,y),
                                   aes(x = Perc_minority,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Minority")+ylab("Score")+ggtitle("Estimate of ADRF")


# m) Number of pools
gam_list = gam_est(Y = y,treat = n,
                   treat_formula = n~perc_positive,
                   data = data.frame('y' = y,"n" = X$n,
                                     "perc_positive" = X$perc_positive),
                   grid_val = quantile(X$n,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = n,
                                treat_formula = n~perc_positive,
                                data = data.frame('y' = y_boot,"n" = X_boot$n,
                                                  "perc_positive" = X_boot$perc_positive),
                                grid_val = quantile(X$n,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$n,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("n" = X$n,y),
                                   aes(x = n,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Number of Pools")+ylab("Score")+ggtitle("Estimate of ADRF")


# m) Percentage of positive pools
gam_list = gam_est(Y = y,treat = perc_positive,
                   treat_formula = perc_positive~AVG_CANPY+ClassBest_Pct+
                     Class_21Pct+Class_22Pct+Class_23Pct+Class_24Pct+AVG_IMP+
                     Pct_HUocc,
                   data = data.frame('y' = y,
                                     "perc_positive" = X$perc_positive,"AVG_CANPY" = X$AVG_CANPY,
                                     "ClassBest_Pct"= X$ClassBest_Pct,"Class_21Pct" = X$Class_21Pct,
                                     "Class_22Pct"=X$Class_22Pct,"Class_23Pct" = X$Class_23Pct,
                                     "Class_24Pct" = X$Class_24Pct,"AVG_IMP" = X$AVG_IMP,
                                     "Pct_HUocc"=X$Pct_HUocc),
                   grid_val = quantile(X$perc_positive,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = perc_positive,
                                treat_formula = perc_positive~AVG_CANPY+ClassBest_Pct+
                                  Class_21Pct+Class_22Pct+Class_23Pct+Class_24Pct+AVG_IMP+
                                  Pct_HUocc,
                                data = data.frame('y' = y_boot,
                                                  "perc_positive" = X_boot$perc_positive,"AVG_CANPY" = X_boot$AVG_CANPY,
                                                  "ClassBest_Pct"= X_boot$ClassBest_Pct,"Class_21Pct" = X_boot$Class_21Pct,
                                                  "Class_22Pct"=X_boot$Class_22Pct,"Class_23Pct" = X_boot$Class_23Pct,
                                                  "Class_24Pct" = X_boot$Class_24Pct,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Pct_HUocc"=X_boot$Pct_HUocc),
                                grid_val = quantile(X$perc_positive,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$perc_positive,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("perc_positive" = X$perc_positive,y),
                                   aes(x = perc_positive,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage of Postive Pools")+ylab("Score")+ggtitle("Estimate of ADRF")


# n) Number of positive pools
gam_list = gam_est(Y = y,treat = num_positive,
                   treat_formula = num_positive~AVG_CANPY+ClassBest_Pct+
                     Class_21Pct+Class_22Pct+Class_23Pct+Class_24Pct+AVG_IMP+
                     Pct_HUocc,
                   data = data.frame('y' = y,
                                     "num_positive" = log(X$num_positive),"AVG_CANPY" = X$AVG_CANPY,
                                     "ClassBest_Pct"= X$ClassBest_Pct,"Class_21Pct" = X$Class_21Pct,
                                     "Class_22Pct"=X$Class_22Pct,"Class_23Pct" = X$Class_23Pct,
                                     "Class_24Pct" = X$Class_24Pct,"AVG_IMP" = X$AVG_IMP,
                                     "Pct_HUocc"=X$Pct_HUocc),
                   grid_val = quantile(log(X$num_positive),probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = num_positive,
                                treat_formula = num_positive~AVG_CANPY+ClassBest_Pct+
                                  Class_21Pct+Class_22Pct+Class_23Pct+Class_24Pct+AVG_IMP+
                                  Pct_HUocc,
                                data = data.frame('y' = y_boot,
                                                  "num_positive" = log(X_boot$num_positive),"AVG_CANPY" = X_boot$AVG_CANPY,
                                                  "ClassBest_Pct"= X_boot$ClassBest_Pct,"Class_21Pct" = X_boot$Class_21Pct,
                                                  "Class_22Pct"=X_boot$Class_22Pct,"Class_23Pct" = X_boot$Class_23Pct,
                                                  "Class_24Pct" = X_boot$Class_24Pct,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Pct_HUocc"=X_boot$Pct_HUocc),
                                grid_val = quantile(log(X$num_positive),probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(log(X$num_positive),probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("num_positive" = log(X$num_positive),y),
                                   aes(x = num_positive,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Log Number of Postive Pools")+ylab("Score")+ggtitle("Estimate of ADRF")



# ------------------------------------------------------------------------------
######## Running all the above with score as sensitivity# ----------------------
# ------------------------------------------------------------------------------
# Defining the response
y = actual_dat.sens$avg_sensitivity

# a) Total Population
gam_list = gam_est(Y = y,treat = TPOP2020,
                   treat_formula = TPOP2020~1,
                   data.frame('y' = y,"TPOP2020" = X$TPOP2020),
                   grid_val = quantile(X$TPOP2020,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

#Empirical Bootstrap to find 95% CI:
params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y_boot,treat = TPOP2020,
                                treat_formula = TPOP2020~1,
                                data.frame('y_boot' = y_boot,"TPOP2020" = X_boot$TPOP2020),
                                grid_val = quantile(X$TPOP2020,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$TPOP2020,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("TPOP2020" = X$TPOP2020,y),
                                   aes(x = TPOP2020,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Total Population")+ylab("Score")+ggtitle("Estimate of ADRF")


# b) Avg_Imp
gam_list = gam_est(Y = y,treat = AVG_IMP,
                   treat_formula = AVG_IMP~TPOP2020,
                   data = data.frame('y' = y,"TPOP2020" = X$TPOP2020,"AVG_IMP" = X$AVG_IMP),
                   grid_val = quantile(X$AVG_IMP,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = AVG_IMP,
                                treat_formula = AVG_IMP~TPOP2020,
                                data = data.frame('y' = y_boot,
                                                  "TPOP2020" = X_boot$TPOP2020,
                                                  "AVG_IMP" = X_boot$AVG_IMP),
                                grid_val = quantile(X$AVG_IMP,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$AVG_IMP,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("AVG_IMP" = X$AVG_IMP,y),
                                   aes(x = AVG_IMP,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Impervious Land")+ylab("Score")+ggtitle("Estimate of ADRF")

# c) AVG_CANPY

gam_list = gam_est(Y = y,treat = AVG_CANPY,
                   treat_formula = AVG_CANPY~Perc_minority+ClassBest_Pct+Class_21Pct+
                     Class_22Pct+Class_23Pct+Class_24Pct,
                   data = data.frame('y' = y,
                                     "Perc_minority" = 100 - X$White_Perc,
                                     "AVG_CANPY" = X$AVG_CANPY,
                                     "ClassBest_Pct"=X$ClassBest_Pct,
                                     "Class_21Pct"=X$Class_21Pct,
                                     "Class_22Pct" = X$Class_22Pct,
                                     "Class_23Pct" = X$Class_23Pct,
                                     "Class_24Pct" = X$Class_24Pct),
                   grid_val = quantile(X$AVG_CANPY,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = AVG_CANPY,
                                treat_formula = AVG_CANPY~Perc_minority+ClassBest_Pct+Class_21Pct+
                                  Class_22Pct+Class_23Pct+Class_24Pct,
                                data = data.frame('y' = y_boot,
                                                  "Perc_minority" = 100 - X_boot$White_Perc,
                                                  "AVG_CANPY" = X_boot$AVG_CANPY,
                                                  "ClassBest_Pct"=X_boot$ClassBest_Pct,
                                                  "Class_21Pct"=X_boot$Class_21Pct,
                                                  "Class_22Pct" = X_boot$Class_22Pct,
                                                  "Class_23Pct" = X_boot$Class_23Pct,
                                                  "Class_24Pct" = X_boot$Class_24Pct),
                                grid_val = quantile(X$AVG_CANPY,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$AVG_CANPY,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("AVG_CANPY" = X$AVG_CANPY,y),
                                   aes(x = AVG_CANPY,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Canopy")+ylab("Score")+ggtitle("Estimate of ADRF")

# d) HSgradPct
gam_list = gam_est(Y = y,treat = HSgradPct,
                   treat_formula = HSgradPct~Perc_minority,
                   data = data.frame('y' = y,"Perc_minority" = 100 - X$White_Perc,
                                     "HSgradPct" = X$HSgradPct),
                   grid_val = quantile(X$HSgradPct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = HSgradPct,
                                treat_formula = HSgradPct~Perc_minority,
                                data = data.frame('y' = y_boot,"Perc_minority" = 100 - X_boot$White_Perc,
                                                  "HSgradPct" = X_boot$HSgradPct),
                                grid_val = quantile(X$HSgradPct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$HSgradPct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("HSgradPct" = X$HSgradPct,y),
                                   aes(x = HSgradPct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage High School graduates")+ylab("Score")+ggtitle("Estimate of ADRF")
# e) FamPovPct
gam_list = gam_est(Y = y,treat = FamPovPct,
                   treat_formula = FamPovPct~Perc_minority,
                   data = data.frame('y' = y,"Perc_minority" = 100 - X$White_Perc,
                                     "FamPovPct" = X$FamPovPct),
                   grid_val = quantile(X$FamPovPct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = FamPovPct,
                                treat_formula = FamPovPct~Perc_minority,
                                data = data.frame('y' = y_boot,"Perc_minority" = 100-X_boot$White_Perc,
                                                  "FamPovPct" = X_boot$FamPovPct),
                                grid_val = quantile(X$FamPovPct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$FamPovPct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("FamPovPct" = X$FamPovPct,y),
                                   aes(x = FamPovPct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Family Poverty")+ylab("Score")+ggtitle("Estimate of ADRF")

# f) Pct_HUocc
gam_list = gam_est(Y = y,treat = Pct_HUocc,
                   treat_formula = Pct_HUocc~Perc_minority+TPOP2020,
                   data = data.frame('y' = y,"Perc_minority" =100 - X$White_Perc,
                                     "Pct_HUocc" = X$Pct_HUocc,"TPOP2020" = X$TPOP2020),
                   grid_val = quantile(X$Pct_HUocc,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Pct_HUocc,
                                treat_formula = Pct_HUocc~Perc_minority+TPOP2020,
                                data = data.frame('y' = y_boot,"Perc_minority" = 100 - X_boot$White_Perc,
                                                  "Pct_HUocc" = X_boot$Pct_HUocc,"TPOP2020" = X_boot$TPOP2020),
                                grid_val = quantile(X$Pct_HUocc,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$Pct_HUocc,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Pct_HUocc" = X$Pct_HUocc,y),
                                   aes(x = Pct_HUocc,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Occupied Housing")+ylab("Score")+ggtitle("Estimate of ADRF")

# g) ClassBest_Pct
gam_list = gam_est(Y = y,treat = ClassBest_Pct,
                   treat_formula = ClassBest_Pct~AVG_IMP,
                   data = data.frame('y' = y,"AVG_IMP" = X$AVG_IMP,
                                     "ClassBest_Pct" = X$ClassBest_Pct),
                   grid_val = quantile(X$ClassBest_Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = ClassBest_Pct,
                                treat_formula = ClassBest_Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "ClassBest_Pct" = X_boot$ClassBest_Pct),
                                grid_val = quantile(X$ClassBest_Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$ClassBest_Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))


ggplot(data = plot_dat)+geom_point(data = data.frame("ClassBest_Pct" = X$ClassBest_Pct,y),
                                   aes(x = ClassBest_Pct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Developed Land")+ylab("Score")+ggtitle("Estimate of ADRF")



# h) Class_21Pct
gam_list = gam_est(Y = y,treat = Class_21Pct,
                   treat_formula = Class_21Pct~AVG_IMP,
                   data = data.frame('y' = y,"AVG_IMP" = X$AVG_IMP,
                                     "Class_21Pct" = X$Class_21Pct),
                   grid_val = quantile(X$Class_21Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Class_21Pct,
                                treat_formula = Class_21Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Class_21Pct" = X_boot$Class_21Pct),
                                grid_val = quantile(X$Class_21Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$Class_21Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Class_21Pct" = X$Class_21Pct,y),
                                   aes(x = Class_21Pct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Open Space")+ylab("Score")+ggtitle("Estimate of ADRF")
# i) Class_22Pct
gam_list = gam_est(Y = y,treat = Class_22Pct,
                   treat_formula = Class_22Pct~AVG_IMP,
                   data = data.frame('y' = y,"AVG_IMP" = X$AVG_IMP,
                                     "Class_22Pct" = X$Class_22Pct),
                   grid_val = quantile(X$Class_22Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Class_22Pct,
                                treat_formula = Class_22Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Class_22Pct" = X_boot$Class_22Pct),
                                grid_val = quantile(X$Class_22Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$Class_22Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Class_22Pct" = X$Class_22Pct,y),
                                   aes(x = Class_22Pct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Developed Low Density")+ylab("Score")+ggtitle("Estimate of ADRF")


# j) Class_23Pct
gam_list = gam_est(Y = y,treat = Class_23Pct,
                   treat_formula = Class_23Pct~AVG_IMP,
                   data = data.frame('y' = y,"AVG_IMP" = X$AVG_IMP,
                                     "Class_23Pct" = X$Class_23Pct),
                   grid_val = quantile(X$Class_23Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Class_23Pct,
                                treat_formula = Class_23Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Class_23Pct" = X_boot$Class_23Pct),
                                grid_val = quantile(X$Class_23Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$Class_23Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Class_23Pct" = X$Class_23Pct,y),
                                   aes(x = Class_23Pct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Developed Medium Density")+ylab("Score")+ggtitle("Estimate of ADRF")


# k) Class_24Pct
gam_list = gam_est(Y = y,treat = Class_24Pct,
                   treat_formula = Class_24Pct~AVG_IMP,
                   data = data.frame('y' = y,"AVG_IMP" = X$AVG_IMP,
                                     "Class_24Pct" = X$Class_24Pct),
                   grid_val = quantile(X$Class_24Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Class_24Pct,
                                treat_formula = Class_24Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Class_24Pct" = X_boot$Class_24Pct),
                                grid_val = quantile(X$Class_24Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$Class_24Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Class_24Pct" = X$Class_24Pct,y),
                                   aes(x = Class_24Pct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Developed High Density")+ylab("Score")+ggtitle("Estimate of ADRF")


# l) Perc_Minority
gam_list = gam_est(Y = y,treat = Perc_minority,
                   treat_formula = Perc_minority~TPOP2020,
                   data = data.frame('y' = y,"TPOP2020" = X$TPOP2020,
                                     "Perc_minority" = 100 - X$White_Perc),
                   grid_val = quantile(100 - X$White_Perc,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Perc_minority,
                                treat_formula = Perc_minority~TPOP2020,
                                data = data.frame('y' = y_boot,
                                                  "TPOP2020" = X_boot$TPOP2020,
                                                  "Perc_minority" = 100 - X_boot$White_Perc),
                                grid_val = quantile(100 - X$White_Perc,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(100 - X$White_Perc,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Perc_minority" = 100 - X$White_Perc,y),
                                   aes(x = Perc_minority,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Minority")+ylab("Score")+ggtitle("Estimate of ADRF")



# m) Number of pools
gam_list = gam_est(Y = y,treat = n,
                   treat_formula = n~perc_positive,
                   data = data.frame('y' = y,"n" = X$n,
                                     "perc_positive" = X$perc_positive),
                   grid_val = quantile(X$n,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = n,
                                treat_formula = n~perc_positive,
                                data = data.frame('y' = y_boot,"n" = X_boot$n,
                                                  "perc_positive" = X_boot$perc_positive),
                                grid_val = quantile(X$n,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$n,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("n" = X$n,y),
                                   aes(x = n,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Number of Pools")+ylab("Score")+ggtitle("Estimate of ADRF")


# m) Percentage of positive pools
gam_list = gam_est(Y = y,treat = perc_positive,
                   treat_formula = perc_positive~AVG_CANPY+ClassBest_Pct+
                     Class_21Pct+Class_22Pct+Class_23Pct+Class_24Pct+AVG_IMP+
                     Pct_HUocc,
                   data = data.frame('y' = y,
                                     "perc_positive" = X$perc_positive,"AVG_CANPY" = X$AVG_CANPY,
                                     "ClassBest_Pct"= X$ClassBest_Pct,"Class_21Pct" = X$Class_21Pct,
                                     "Class_22Pct"=X$Class_22Pct,"Class_23Pct" = X$Class_23Pct,
                                     "Class_24Pct" = X$Class_24Pct,"AVG_IMP" = X$AVG_IMP,
                                     "Pct_HUocc"=X$Pct_HUocc),
                   grid_val = quantile(X$perc_positive,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = perc_positive,
                                treat_formula = perc_positive~AVG_CANPY+ClassBest_Pct+
                                  Class_21Pct+Class_22Pct+Class_23Pct+Class_24Pct+AVG_IMP+
                                  Pct_HUocc,
                                data = data.frame('y' = y_boot,
                                                  "perc_positive" = X_boot$perc_positive,"AVG_CANPY" = X_boot$AVG_CANPY,
                                                  "ClassBest_Pct"= X_boot$ClassBest_Pct,"Class_21Pct" = X_boot$Class_21Pct,
                                                  "Class_22Pct"=X_boot$Class_22Pct,"Class_23Pct" = X_boot$Class_23Pct,
                                                  "Class_24Pct" = X_boot$Class_24Pct,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Pct_HUocc"=X_boot$Pct_HUocc),
                                grid_val = quantile(X$perc_positive,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$perc_positive,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("perc_positive" = X$perc_positive,y),
                                   aes(x = perc_positive,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage of Postive Pools")+ylab("Score")+ggtitle("Estimate of ADRF")

# n) Number of positive pools
gam_list = gam_est(Y = y,treat = num_positive,
                   treat_formula = num_positive~AVG_CANPY+ClassBest_Pct+
                     Class_21Pct+Class_22Pct+Class_23Pct+Class_24Pct+AVG_IMP+
                     Pct_HUocc,
                   data = data.frame('y' = y,
                                     "num_positive" = log(X$num_positive),"AVG_CANPY" = X$AVG_CANPY,
                                     "ClassBest_Pct"= X$ClassBest_Pct,"Class_21Pct" = X$Class_21Pct,
                                     "Class_22Pct"=X$Class_22Pct,"Class_23Pct" = X$Class_23Pct,
                                     "Class_24Pct" = X$Class_24Pct,"AVG_IMP" = X$AVG_IMP,
                                     "Pct_HUocc"=X$Pct_HUocc),
                   grid_val = quantile(log(X$num_positive),probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = num_positive,
                                treat_formula = num_positive~AVG_CANPY+ClassBest_Pct+
                                  Class_21Pct+Class_22Pct+Class_23Pct+Class_24Pct+AVG_IMP+
                                  Pct_HUocc,
                                data = data.frame('y' = y_boot,
                                                  "num_positive" = log(X_boot$num_positive),"AVG_CANPY" = X_boot$AVG_CANPY,
                                                  "ClassBest_Pct"= X_boot$ClassBest_Pct,"Class_21Pct" = X_boot$Class_21Pct,
                                                  "Class_22Pct"=X_boot$Class_22Pct,"Class_23Pct" = X_boot$Class_23Pct,
                                                  "Class_24Pct" = X_boot$Class_24Pct,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Pct_HUocc"=X_boot$Pct_HUocc),
                                grid_val = quantile(log(X$num_positive),probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(log(X$num_positive),probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("num_positive" = log(X$num_positive),y),
                                   aes(x = num_positive,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Log Number of Postive Pools")+ylab("Score")+ggtitle("Estimate of ADRF")


# ------------------------------------------------------------------------------
######## Running all the above with score as specificity
# ------------------------------------------------------------------------------

X = actual_dat %>% select(-c("ID","n_max", "num_positive_max","n_min",
                             "num_positive_min","zip","TID","SortID","BUFF_DIST",
                             "HUVac","score"))
X = apply(X,2,as.numeric)
X  = as.data.frame(X[,-c(3,4,5,6)])

# Defining the response
y = actual_dat$avg_specificity

X$MajorLC = as.factor(X$MajorLC)
table(X$MajorLC)
levels(X$MajorLC) = list("21" = c(21),
                         "22" = c(22),
                         "23" = c(23),
                         "24" = c(24),
                         "90" = c(90),
                         "0" = c(11,41,71,81,31,82,95))
X$ClassBest_Pct = apply(X %>% select(starts_with("Class_")),1,max) 
X = X %>% select(-starts_with("Class_"))
extra_cols = startsWith(colnames(actual_dat),"Class_2")
X = cbind.data.frame(X,actual_dat[,..extra_cols])
X = X %>% mutate("Pct_HUocc" = HU_Occ/TotHU*100 )
X = X %>% mutate("HispTwoOrMore" = Hispanic+TwoOrMore)
X = X %>% select(-c("HU_Occ","TotHU","TOT18andUp","Hispanic","TwoOrMore"))

X = X %>% mutate(Black_Perc = (Black/TPOP2020)*100,
                 White_Perc = (White/TPOP2020)*100,
                 HispOrTwo_Perc = (HispTwoOrMore/TPOP2020)*100,
                 perc_positive = (num_positive/n)*100) %>% 
  select(-c(Black,White,HispTwoOrMore))

y = y[rowSums(is.na(X)) == 0]
X = X[rowSums(is.na(X)) == 0,]


# a) Total Population
gam_list = gam_est(Y = y,treat = TPOP2020,
                   treat_formula = TPOP2020~1,
                   data.frame('y' = y,"TPOP2020" = X$TPOP2020),
                   grid_val = quantile(X$TPOP2020,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

#Empirical Bootstrap to find 95% CI:
params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y_boot,treat = TPOP2020,
                                treat_formula = TPOP2020~1,
                                data.frame('y_boot' = y_boot,"TPOP2020" = X_boot$TPOP2020),
                                grid_val = quantile(X$TPOP2020,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$TPOP2020,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("TPOP2020" = X$TPOP2020,y),
                                   aes(x = TPOP2020,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Total Population")+ylab("Score")+ggtitle("Estimate of ADRF")


# b) Avg_Imp
gam_list = gam_est(Y = y,treat = AVG_IMP,
                   treat_formula = AVG_IMP~TPOP2020,
                   data = data.frame('y' = y,"TPOP2020" = X$TPOP2020,"AVG_IMP" = X$AVG_IMP),
                   grid_val = quantile(X$AVG_IMP,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = AVG_IMP,
                                treat_formula = AVG_IMP~TPOP2020,
                                data = data.frame('y' = y_boot,
                                                  "TPOP2020" = X_boot$TPOP2020,
                                                  "AVG_IMP" = X_boot$AVG_IMP),
                                grid_val = quantile(X$AVG_IMP,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$AVG_IMP,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("AVG_IMP" = X$AVG_IMP,y),
                                   aes(x = AVG_IMP,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Impervious Land")+ylab("Score")+ggtitle("Estimate of ADRF")

# c) AVG_CANPY

gam_list = gam_est(Y = y,treat = AVG_CANPY,
                   treat_formula = AVG_CANPY~Perc_minority+ClassBest_Pct+Class_21Pct+
                     Class_22Pct+Class_23Pct+Class_24Pct,
                   data = data.frame('y' = y,
                                     "Perc_minority" = 100 - X$White_Perc,
                                     "AVG_CANPY" = X$AVG_CANPY,
                                     "ClassBest_Pct"=X$ClassBest_Pct,
                                     "Class_21Pct"=X$Class_21Pct,
                                     "Class_22Pct" = X$Class_22Pct,
                                     "Class_23Pct" = X$Class_23Pct,
                                     "Class_24Pct" = X$Class_24Pct),
                   grid_val = quantile(X$AVG_CANPY,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = AVG_CANPY,
                                treat_formula = AVG_CANPY~Perc_minority+ClassBest_Pct+Class_21Pct+
                                  Class_22Pct+Class_23Pct+Class_24Pct,
                                data = data.frame('y' = y_boot,
                                                  "Perc_minority" = 100 - X_boot$White_Perc,
                                                  "AVG_CANPY" = X_boot$AVG_CANPY,
                                                  "ClassBest_Pct"=X_boot$ClassBest_Pct,
                                                  "Class_21Pct"=X_boot$Class_21Pct,
                                                  "Class_22Pct" = X_boot$Class_22Pct,
                                                  "Class_23Pct" = X_boot$Class_23Pct,
                                                  "Class_24Pct" = X_boot$Class_24Pct),
                                grid_val = quantile(X$AVG_CANPY,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$AVG_CANPY,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("AVG_CANPY" = X$AVG_CANPY,y),
                                   aes(x = AVG_CANPY,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Canopy")+ylab("Score")+ggtitle("Estimate of ADRF")

# d) HSgradPct
gam_list = gam_est(Y = y,treat = HSgradPct,
                   treat_formula = HSgradPct~Perc_minority,
                   data = data.frame('y' = y,"Perc_minority" = 100 - X$White_Perc,
                                     "HSgradPct" = X$HSgradPct),
                   grid_val = quantile(X$HSgradPct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = HSgradPct,
                                treat_formula = HSgradPct~Perc_minority,
                                data = data.frame('y' = y_boot,"Perc_minority" = 100 - X_boot$White_Perc,
                                                  "HSgradPct" = X_boot$HSgradPct),
                                grid_val = quantile(X$HSgradPct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$HSgradPct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("HSgradPct" = X$HSgradPct,y),
                                   aes(x = HSgradPct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage High School graduates")+ylab("Score")+ggtitle("Estimate of ADRF")
# e) FamPovPct
gam_list = gam_est(Y = y,treat = FamPovPct,
                   treat_formula = FamPovPct~Perc_minority,
                   data = data.frame('y' = y,"Perc_minority" = 100 - X$White_Perc,
                                     "FamPovPct" = X$FamPovPct),
                   grid_val = quantile(X$FamPovPct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = FamPovPct,
                                treat_formula = FamPovPct~Perc_minority,
                                data = data.frame('y' = y_boot,"Perc_minority" = 100-X_boot$White_Perc,
                                                  "FamPovPct" = X_boot$FamPovPct),
                                grid_val = quantile(X$FamPovPct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$FamPovPct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("FamPovPct" = X$FamPovPct,y),
                                   aes(x = FamPovPct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Family Poverty")+ylab("Score")+ggtitle("Estimate of ADRF")

# f) Pct_HUocc
gam_list = gam_est(Y = y,treat = Pct_HUocc,
                   treat_formula = Pct_HUocc~Perc_minority+TPOP2020,
                   data = data.frame('y' = y,"Perc_minority" =100 - X$White_Perc,
                                     "Pct_HUocc" = X$Pct_HUocc,"TPOP2020" = X$TPOP2020),
                   grid_val = quantile(X$Pct_HUocc,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Pct_HUocc,
                                treat_formula = Pct_HUocc~Perc_minority+TPOP2020,
                                data = data.frame('y' = y_boot,"Perc_minority" = 100 - X_boot$White_Perc,
                                                  "Pct_HUocc" = X_boot$Pct_HUocc,"TPOP2020" = X_boot$TPOP2020),
                                grid_val = quantile(X$Pct_HUocc,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$Pct_HUocc,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Pct_HUocc" = X$Pct_HUocc,y),
                                   aes(x = Pct_HUocc,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Occupied Housing")+ylab("Score")+ggtitle("Estimate of ADRF")

# g) ClassBest_Pct
gam_list = gam_est(Y = y,treat = ClassBest_Pct,
                   treat_formula = ClassBest_Pct~AVG_IMP,
                   data = data.frame('y' = y,"AVG_IMP" = X$AVG_IMP,
                                     "ClassBest_Pct" = X$ClassBest_Pct),
                   grid_val = quantile(X$ClassBest_Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = ClassBest_Pct,
                                treat_formula = ClassBest_Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "ClassBest_Pct" = X_boot$ClassBest_Pct),
                                grid_val = quantile(X$ClassBest_Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$ClassBest_Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))


ggplot(data = plot_dat)+geom_point(data = data.frame("ClassBest_Pct" = X$ClassBest_Pct,y),
                                   aes(x = ClassBest_Pct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Developed Land")+ylab("Score")+ggtitle("Estimate of ADRF")



# h) Class_21Pct
gam_list = gam_est(Y = y,treat = Class_21Pct,
                   treat_formula = Class_21Pct~AVG_IMP,
                   data = data.frame('y' = y,"AVG_IMP" = X$AVG_IMP,
                                     "Class_21Pct" = X$Class_21Pct),
                   grid_val = quantile(X$Class_21Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Class_21Pct,
                                treat_formula = Class_21Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Class_21Pct" = X_boot$Class_21Pct),
                                grid_val = quantile(X$Class_21Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$Class_21Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Class_21Pct" = X$Class_21Pct,y),
                                   aes(x = Class_21Pct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Open Space")+ylab("Score")+ggtitle("Estimate of ADRF")
# i) Class_22Pct
gam_list = gam_est(Y = y,treat = Class_22Pct,
                   treat_formula = Class_22Pct~AVG_IMP,
                   data = data.frame('y' = y,"AVG_IMP" = X$AVG_IMP,
                                     "Class_22Pct" = X$Class_22Pct),
                   grid_val = quantile(X$Class_22Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Class_22Pct,
                                treat_formula = Class_22Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Class_22Pct" = X_boot$Class_22Pct),
                                grid_val = quantile(X$Class_22Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$Class_22Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Class_22Pct" = X$Class_22Pct,y),
                                   aes(x = Class_22Pct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Developed Low Density")+ylab("Score")+ggtitle("Estimate of ADRF")


# j) Class_23Pct
gam_list = gam_est(Y = y,treat = Class_23Pct,
                   treat_formula = Class_23Pct~AVG_IMP,
                   data = data.frame('y' = y,"AVG_IMP" = X$AVG_IMP,
                                     "Class_23Pct" = X$Class_23Pct),
                   grid_val = quantile(X$Class_23Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Class_23Pct,
                                treat_formula = Class_23Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Class_23Pct" = X_boot$Class_23Pct),
                                grid_val = quantile(X$Class_23Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$Class_23Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Class_23Pct" = X$Class_23Pct,y),
                                   aes(x = Class_23Pct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Developed Medium Density")+ylab("Score")+ggtitle("Estimate of ADRF")


# k) Class_24Pct
gam_list = gam_est(Y = y,treat = Class_24Pct,
                   treat_formula = Class_24Pct~AVG_IMP,
                   data = data.frame('y' = y,"AVG_IMP" = X$AVG_IMP,
                                     "Class_24Pct" = X$Class_24Pct),
                   grid_val = quantile(X$Class_24Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Class_24Pct,
                                treat_formula = Class_24Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Class_24Pct" = X_boot$Class_24Pct),
                                grid_val = quantile(X$Class_24Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$Class_24Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Class_24Pct" = X$Class_24Pct,y),
                                   aes(x = Class_24Pct,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Developed High Density")+ylab("Score")+ggtitle("Estimate of ADRF")


# l) Perc_Minority
gam_list = gam_est(Y = y,treat = Perc_minority,
                   treat_formula = Perc_minority~TPOP2020,
                   data = data.frame('y' = y,"TPOP2020" = X$TPOP2020,
                                     "Perc_minority" = 100 - X$White_Perc),
                   grid_val = quantile(100 - X$White_Perc,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Perc_minority,
                                treat_formula = Perc_minority~TPOP2020,
                                data = data.frame('y' = y_boot,
                                                  "TPOP2020" = X_boot$TPOP2020,
                                                  "Perc_minority" = 100 - X_boot$White_Perc),
                                grid_val = quantile(100 - X$White_Perc,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(100 - X$White_Perc,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Perc_minority" = 100 - X$White_Perc,y),
                                   aes(x = Perc_minority,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Minority")+ylab("Score")+ggtitle("Estimate of ADRF")



# m) Number of pools
gam_list = gam_est(Y = y,treat = n,
                   treat_formula = n~perc_positive,
                   data = data.frame('y' = y,"n" = X$n,
                                     "perc_positive" = X$perc_positive),
                   grid_val = quantile(X$n,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = n,
                                treat_formula = n~perc_positive,
                                data = data.frame('y' = y_boot,"n" = X_boot$n,
                                                  "perc_positive" = X_boot$perc_positive),
                                grid_val = quantile(X$n,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$n,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("n" = X$n,y),
                                   aes(x = n,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Number of Pools")+ylab("Score")+ggtitle("Estimate of ADRF")


# m) Percentage of positive pools
gam_list = gam_est(Y = y,treat = perc_positive,
                   treat_formula = perc_positive~AVG_CANPY+ClassBest_Pct+
                     Class_21Pct+Class_22Pct+Class_23Pct+Class_24Pct+AVG_IMP+
                     Pct_HUocc,
                   data = data.frame('y' = y,
                                     "perc_positive" = X$perc_positive,"AVG_CANPY" = X$AVG_CANPY,
                                     "ClassBest_Pct"= X$ClassBest_Pct,"Class_21Pct" = X$Class_21Pct,
                                     "Class_22Pct"=X$Class_22Pct,"Class_23Pct" = X$Class_23Pct,
                                     "Class_24Pct" = X$Class_24Pct,"AVG_IMP" = X$AVG_IMP,
                                     "Pct_HUocc"=X$Pct_HUocc),
                   grid_val = quantile(X$perc_positive,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = perc_positive,
                                treat_formula = perc_positive~AVG_CANPY+ClassBest_Pct+
                                  Class_21Pct+Class_22Pct+Class_23Pct+Class_24Pct+AVG_IMP+
                                  Pct_HUocc,
                                data = data.frame('y' = y_boot,
                                                  "perc_positive" = X_boot$perc_positive,"AVG_CANPY" = X_boot$AVG_CANPY,
                                                  "ClassBest_Pct"= X_boot$ClassBest_Pct,"Class_21Pct" = X_boot$Class_21Pct,
                                                  "Class_22Pct"=X_boot$Class_22Pct,"Class_23Pct" = X_boot$Class_23Pct,
                                                  "Class_24Pct" = X_boot$Class_24Pct,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Pct_HUocc"=X_boot$Pct_HUocc),
                                grid_val = quantile(X$perc_positive,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X$perc_positive,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("perc_positive" = X$perc_positive,y),
                                   aes(x = perc_positive,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage of Postive Pools")+ylab("Score")+ggtitle("Estimate of ADRF")

# n) Number of positive pools
gam_list = gam_est(Y = y,treat = num_positive,
                   treat_formula = num_positive~AVG_CANPY+ClassBest_Pct+
                     Class_21Pct+Class_22Pct+Class_23Pct+Class_24Pct+AVG_IMP+
                     Pct_HUocc,
                   data = data.frame('y' = y,
                                     "num_positive" = log(X$num_positive),"AVG_CANPY" = X$AVG_CANPY,
                                     "ClassBest_Pct"= X$ClassBest_Pct,"Class_21Pct" = X$Class_21Pct,
                                     "Class_22Pct"=X$Class_22Pct,"Class_23Pct" = X$Class_23Pct,
                                     "Class_24Pct" = X$Class_24Pct,"AVG_IMP" = X$AVG_IMP,
                                     "Pct_HUocc"=X$Pct_HUocc),
                   grid_val = quantile(log(X$num_positive),probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X)[1],dim(X)[1],replace = T)
  X_boot = X[boot_samp,]
  y_boot = y[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = num_positive,
                                treat_formula = num_positive~AVG_CANPY+ClassBest_Pct+
                                  Class_21Pct+Class_22Pct+Class_23Pct+Class_24Pct+AVG_IMP+
                                  Pct_HUocc,
                                data = data.frame('y' = y_boot,
                                                  "num_positive" = log(X_boot$num_positive),"AVG_CANPY" = X_boot$AVG_CANPY,
                                                  "ClassBest_Pct"= X_boot$ClassBest_Pct,"Class_21Pct" = X_boot$Class_21Pct,
                                                  "Class_22Pct"=X_boot$Class_22Pct,"Class_23Pct" = X_boot$Class_23Pct,
                                                  "Class_24Pct" = X_boot$Class_24Pct,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Pct_HUocc"=X_boot$Pct_HUocc),
                                grid_val = quantile(log(X$num_positive),probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(log(X$num_positive),probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("num_positive" = log(X$num_positive),y),
                                   aes(x = num_positive,y = y))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Log Number of Postive Pools")+ylab("Score")+ggtitle("Estimate of ADRF")

# ------------------------------------------------------------------------------
# ---------------- Different estimates for TOTPOP2020 --------------------------
# ------------------------------------------------------------------------------
# Trying to see the estimate which works best 

plot(data.frame(X$TPOP2020,y),
     xlab = "Total Population",
     ylab = "Y",
     main = "Estimate of ADRF",cex = 0.5,pch = 19)

#1) BART estimate
bart_list = bart_est(Y = y,
                     treat = TPOP2020,
                     outcome_formula = y ~ .,
                     data = data.frame('y' = y,"TPOP2020" = X$TPOP2020),
                     grid_val = seq(min(X$TPOP2020), max(X$TPOP2020),
                                    length.out = 50))

lines(seq(min(X$TPOP2020), max(X$TPOP2020), length.out = 50),
      bart_list$param,
      lty = 1,
      lwd = 2,
      col = "blue")

#2) Additive splines estimate
add_spl_list = add_spl_est(Y = y,
                           treat = TPOP2020,
                           treat_formula = TPOP2020~1 ,
                           data = data.frame('y' = y,"TPOP2020" = X$TPOP2020),
                           grid_val = seq(min(X$TPOP2020), max(X$TPOP2020), length.out = 50),
                           knot_num = 4,
                           treat_mod = "Normal")

lines(seq(min(X$TPOP2020), max(X$TPOP2020), length.out = 50),
      add_spl_list$param,
      lty = 1,
      lwd = 2,
      col = "green")

#3) Generalized Additive Models estimate
gam_list = gam_est(Y = y,treat = TPOP2020,
                   treat_formula = TPOP2020~1,
                   data = data.frame('y' = y,"TPOP2020" = X$TPOP2020),
                   grid_val = seq(min(X$TPOP2020), max(X$TPOP2020), length.out = 50),
                   treat_mod = "Normal")

lines(seq(min(X$TPOP2020), max(X$TPOP2020), length.out = 50),
      gam_list$param,
      lty = 1,
      lwd = 2,
      col = "red")

# 4) Nadaraya Watson Estimate:
nw_list = nw_est(Y = y,treat = TPOP2020,
                 treat_formula = TPOP2020~1,
                 data = data.frame('y' = y,"TPOP2020" = X$TPOP2020),
                 grid_val = seq(min(X$TPOP2020), max(X$TPOP2020), length.out = 50),
                 treat_mod = "Normal",
                 bandw = bw.SJ(X$TPOP2020))

lines(seq(min(X$TPOP2020), max(X$TPOP2020), length.out = 50),
      nw_list$param,
      lty = 1,
      lwd = 2,
      col = "purple")

# 5) Hirano-Imbens Estimate:
hi_list = hi_est(Y = y,treat = TPOP2020,
                 treat_formula = TPOP2020~1,
                 outcome_formula = y~.,
                 data = data.frame('y' = y,"TPOP2020" = X$TPOP2020),
                 grid_val = seq(min(X$TPOP2020), max(X$TPOP2020), length.out = 50),
                 treat_mod = "Normal")

lines(seq(min(X$TPOP2020), max(X$TPOP2020), length.out = 50),
      hi_list$param,
      lty = 1,
      lwd = 2,
      col = "darkorange")

# 6) The inverse weighting Estimate:
iw_list = iw_est(Y = y,treat = TPOP2020,
                 treat_formula = TPOP2020~1,
                 data = data.frame('y' = y,"TPOP2020" = X$TPOP2020),
                 grid_val = seq(min(X$TPOP2020), max(X$TPOP2020), length.out = 50),
                 treat_mod = "Normal",bandw = bw.SJ(X$TPOP2020))

lines(seq(min(X$TPOP2020), max(X$TPOP2020), length.out = 50),
      iw_list$param,
      lty = 1,
      lwd = 2,
      col = "deepskyblue")


grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)

legend('bottomright',
       c("BART estimate","Additive splines estimate", "Generalized Additive Models estimate",
         "Nadaraya Watson Estimate","Hirano-Imbens Estimate","Inverse weighting Estimate"),
       lty=1,
       lwd = 2,
       col = c("blue","green","red","purple","darkorange","deepskyblue"),
       bty='Y',
       cex=1)

# ------------------------------------------------------------------------------
#### ---------- Redoing analysis for places with population over 10000. -------
# ------------------------------------------------------------------------------

X_subset = X[X$TPOP2020>10000]
y_subset = y[X$TPOP2020>10000]
# a) Total Population
gam_list = gam_est(Y = y_subset,treat = TPOP2020,
                   treat_formula = TPOP2020~1,
                   data.frame('y_subset' = y_subset,"TPOP2020" = X_subset$TPOP2020),
                   grid_val = quantile(X_subset$TPOP2020,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

#Empirical Bootstrap to find 95% CI:
params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X_subset)[1],dim(X_subset)[1],replace = T)
  X_boot = X_subset[boot_samp,]
  y_boot = y_subset[boot_samp]
  params = rbind(params,gam_est(Y = y_boot,treat = TPOP2020,
                                treat_formula = TPOP2020~1,
                                data.frame('y_boot' = y_boot,"TPOP2020" = X_boot$TPOP2020),
                                grid_val = quantile(X_subset$TPOP2020,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X_subset$TPOP2020,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("TPOP2020" = X_subset$TPOP2020,y_subset),
                                   aes(x = TPOP2020,y = y_subset))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Total Population")+ylab("Score")+ggtitle("Estimate of ADRF")


# b) Avg_Imp
gam_list = gam_est(Y = y,treat = AVG_IMP,
                   treat_formula = AVG_IMP~TPOP2020,
                   data = data.frame('y' = y_subset,"TPOP2020" = X_subset$TPOP2020,"AVG_IMP" = X_subset$AVG_IMP),
                   grid_val = quantile(X_subset$AVG_IMP,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X_subset)[1],dim(X_subset)[1],replace = T)
  X_boot = X_subset[boot_samp,]
  y_boot = y_subset[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = AVG_IMP,
                                treat_formula = AVG_IMP~TPOP2020,
                                data = data.frame('y' = y_boot,
                                                  "TPOP2020" = X_boot$TPOP2020,
                                                  "AVG_IMP" = X_boot$AVG_IMP),
                                grid_val = quantile(X_subset$AVG_IMP,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X_subset$AVG_IMP,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("AVG_IMP" = X_subset$AVG_IMP,y_subset),
                                   aes(x = AVG_IMP,y = y_subset))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Impervious Land")+ylab("Score")+ggtitle("Estimate of ADRF")

# c) AVG_CANPY

gam_list = gam_est(Y = y,treat = AVG_CANPY,
                   treat_formula = AVG_CANPY~Perc_minority+ClassBest_Pct+Class_21Pct+
                     Class_22Pct+Class_23Pct+Class_24Pct,
                   data = data.frame('y' = y_subset,
                                     "Perc_minority" = 100 - X_subset$White_Perc,
                                     "AVG_CANPY" = X_subset$AVG_CANPY,
                                     "ClassBest_Pct"=X_subset$ClassBest_Pct,
                                     "Class_21Pct"=X_subset$Class_21Pct,
                                     "Class_22Pct" = X_subset$Class_22Pct,
                                     "Class_23Pct" = X_subset$Class_23Pct,
                                     "Class_24Pct" = X_subset$Class_24Pct),
                   grid_val = quantile(X_subset$AVG_CANPY,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X_subset)[1],dim(X_subset)[1],replace = T)
  X_boot = X_subset[boot_samp,]
  y_boot = y_subset[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = AVG_CANPY,
                                treat_formula = AVG_CANPY~Perc_minority+ClassBest_Pct+Class_21Pct+
                                  Class_22Pct+Class_23Pct+Class_24Pct,
                                data = data.frame('y' = y_boot,
                                                  "Perc_minority" = 100 - X_boot$White_Perc,
                                                  "AVG_CANPY" = X_boot$AVG_CANPY,
                                                  "ClassBest_Pct"=X_boot$ClassBest_Pct,
                                                  "Class_21Pct"=X_boot$Class_21Pct,
                                                  "Class_22Pct" = X_boot$Class_22Pct,
                                                  "Class_23Pct" = X_boot$Class_23Pct,
                                                  "Class_24Pct" = X_boot$Class_24Pct),
                                grid_val = quantile(X_subset$AVG_CANPY,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X_subset$AVG_CANPY,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("AVG_CANPY" = X_subset$AVG_CANPY,y_subset),
                                   aes(x = AVG_CANPY,y = y_subset))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Canopy")+ylab("Score")+ggtitle("Estimate of ADRF")

# d) HSgradPct
gam_list = gam_est(Y = y,treat = HSgradPct,
                   treat_formula = HSgradPct~Perc_minority,
                   data = data.frame('y' = y_subset,"Perc_minority" = 100 - X_subset$White_Perc,
                                     "HSgradPct" = X_subset$HSgradPct),
                   grid_val = quantile(X_subset$HSgradPct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X_subset)[1],dim(X_subset)[1],replace = T)
  X_boot = X_subset[boot_samp,]
  y_boot = y_subset[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = HSgradPct,
                                treat_formula = HSgradPct~Perc_minority,
                                data = data.frame('y' = y_boot,"Perc_minority" = 100 - X_boot$White_Perc,
                                                  "HSgradPct" = X_boot$HSgradPct),
                                grid_val = quantile(X_subset$HSgradPct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X_subset$HSgradPct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("HSgradPct" = X_subset$HSgradPct,y_subset),
                                   aes(x = HSgradPct,y = y_subset))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage High School graduates")+ylab("Score")+ggtitle("Estimate of ADRF")
# e) FamPovPct
gam_list = gam_est(Y = y,treat = FamPovPct,
                   treat_formula = FamPovPct~Perc_minority,
                   data = data.frame('y' = y_subset,"Perc_minority" = 100 - X_subset$White_Perc,
                                     "FamPovPct" = X_subset$FamPovPct),
                   grid_val = quantile(X_subset$FamPovPct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X_subset)[1],dim(X_subset)[1],replace = T)
  X_boot = X_subset[boot_samp,]
  y_boot = y_subset[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = FamPovPct,
                                treat_formula = FamPovPct~Perc_minority,
                                data = data.frame('y' = y_boot,"Perc_minority" = 100-X_boot$White_Perc,
                                                  "FamPovPct" = X_boot$FamPovPct),
                                grid_val = quantile(X_subset$FamPovPct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X_subset$FamPovPct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("FamPovPct" = X_subset$FamPovPct,y_subset),
                                   aes(x = FamPovPct,y = y_subset))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Family Poverty")+ylab("Score")+ggtitle("Estimate of ADRF")

# f) Pct_HUocc
gam_list = gam_est(Y = y,treat = Pct_HUocc,
                   treat_formula = Pct_HUocc~Perc_minority+TPOP2020,
                   data = data.frame('y' = y_subset,"Perc_minority" =100 - X_subset$White_Perc,
                                     "Pct_HUocc" = X_subset$Pct_HUocc,"TPOP2020" = X_subset$TPOP2020),
                   grid_val = quantile(X_subset$Pct_HUocc,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X_subset)[1],dim(X_subset)[1],replace = T)
  X_boot = X_subset[boot_samp,]
  y_boot = y_subset[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Pct_HUocc,
                                treat_formula = Pct_HUocc~Perc_minority+TPOP2020,
                                data = data.frame('y' = y_boot,"Perc_minority" = 100 - X_boot$White_Perc,
                                                  "Pct_HUocc" = X_boot$Pct_HUocc,"TPOP2020" = X_boot$TPOP2020),
                                grid_val = quantile(X_subset$Pct_HUocc,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X_subset$Pct_HUocc,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Pct_HUocc" = X_subset$Pct_HUocc,y_subset),
                                   aes(x = Pct_HUocc,y = y_subset))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Occupied Housing")+ylab("Score")+ggtitle("Estimate of ADRF")

# g) ClassBest_Pct
gam_list = gam_est(Y = y,treat = ClassBest_Pct,
                   treat_formula = ClassBest_Pct~AVG_IMP,
                   data = data.frame('y' = y_subset,"AVG_IMP" = X_subset$AVG_IMP,
                                     "ClassBest_Pct" = X_subset$ClassBest_Pct),
                   grid_val = quantile(X_subset$ClassBest_Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X_subset)[1],dim(X_subset)[1],replace = T)
  X_boot = X_subset[boot_samp,]
  y_boot = y_subset[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = ClassBest_Pct,
                                treat_formula = ClassBest_Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "ClassBest_Pct" = X_boot$ClassBest_Pct),
                                grid_val = quantile(X_subset$ClassBest_Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X_subset$ClassBest_Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))


ggplot(data = plot_dat)+geom_point(data = data.frame("ClassBest_Pct" = X_subset$ClassBest_Pct,y_subset),
                                   aes(x = ClassBest_Pct,y = y_subset))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Developed Land")+ylab("Score")+ggtitle("Estimate of ADRF")



# h) Class_21Pct
gam_list = gam_est(Y = y,treat = Class_21Pct,
                   treat_formula = Class_21Pct~AVG_IMP,
                   data = data.frame('y' = y_subset,"AVG_IMP" = X_subset$AVG_IMP,
                                     "Class_21Pct" = X_subset$Class_21Pct),
                   grid_val = quantile(X_subset$Class_21Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X_subset)[1],dim(X_subset)[1],replace = T)
  X_boot = X_subset[boot_samp,]
  y_boot = y_subset[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Class_21Pct,
                                treat_formula = Class_21Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Class_21Pct" = X_boot$Class_21Pct),
                                grid_val = quantile(X_subset$Class_21Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X_subset$Class_21Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Class_21Pct" = X_subset$Class_21Pct,y_subset),
                                   aes(x = Class_21Pct,y = y_subset))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Open Space")+ylab("Score")+ggtitle("Estimate of ADRF")
# i) Class_22Pct
gam_list = gam_est(Y = y,treat = Class_22Pct,
                   treat_formula = Class_22Pct~AVG_IMP,
                   data = data.frame('y' = y_subset,"AVG_IMP" = X_subset$AVG_IMP,
                                     "Class_22Pct" = X_subset$Class_22Pct),
                   grid_val = quantile(X_subset$Class_22Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X_subset)[1],dim(X_subset)[1],replace = T)
  X_boot = X_subset[boot_samp,]
  y_boot = y_subset[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Class_22Pct,
                                treat_formula = Class_22Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Class_22Pct" = X_boot$Class_22Pct),
                                grid_val = quantile(X_subset$Class_22Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X_subset$Class_22Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Class_22Pct" = X_subset$Class_22Pct,y_subset),
                                   aes(x = Class_22Pct,y = y_subset))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Developed Low Density")+ylab("Score")+ggtitle("Estimate of ADRF")


# j) Class_23Pct
gam_list = gam_est(Y = y,treat = Class_23Pct,
                   treat_formula = Class_23Pct~AVG_IMP,
                   data = data.frame('y' = y_subset,"AVG_IMP" = X_subset$AVG_IMP,
                                     "Class_23Pct" = X_subset$Class_23Pct),
                   grid_val = quantile(X_subset$Class_23Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X_subset)[1],dim(X_subset)[1],replace = T)
  X_boot = X_subset[boot_samp,]
  y_boot = y_subset[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Class_23Pct,
                                treat_formula = Class_23Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Class_23Pct" = X_boot$Class_23Pct),
                                grid_val = quantile(X_subset$Class_23Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X_subset$Class_23Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Class_23Pct" = X_subset$Class_23Pct,y_subset),
                                   aes(x = Class_23Pct,y = y_subset))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Developed Medium Density")+ylab("Score")+ggtitle("Estimate of ADRF")


# k) Class_24Pct
gam_list = gam_est(Y = y,treat = Class_24Pct,
                   treat_formula = Class_24Pct~AVG_IMP,
                   data = data.frame('y' = y_subset,"AVG_IMP" = X_subset$AVG_IMP,
                                     "Class_24Pct" = X_subset$Class_24Pct),
                   grid_val = quantile(X_subset$Class_24Pct,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X_subset)[1],dim(X_subset)[1],replace = T)
  X_boot = X_subset[boot_samp,]
  y_boot = y_subset[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Class_24Pct,
                                treat_formula = Class_24Pct~AVG_IMP,
                                data = data.frame('y' = y_boot,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Class_24Pct" = X_boot$Class_24Pct),
                                grid_val = quantile(X_subset$Class_24Pct,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X_subset$Class_24Pct,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Class_24Pct" = X_subset$Class_24Pct,y_subset),
                                   aes(x = Class_24Pct,y = y_subset))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Developed High Density")+ylab("Score")+ggtitle("Estimate of ADRF")


# l) Perc_Minority
gam_list = gam_est(Y = y,treat = Perc_minority,
                   treat_formula = Perc_minority~TPOP2020,
                   data = data.frame('y' = y_subset,"TPOP2020" = X_subset$TPOP2020,
                                     "Perc_minority" = 100 - X_subset$White_Perc),
                   grid_val = quantile(100 - X_subset$White_Perc,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X_subset)[1],dim(X_subset)[1],replace = T)
  X_boot = X_subset[boot_samp,]
  y_boot = y_subset[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = Perc_minority,
                                treat_formula = Perc_minority~TPOP2020,
                                data = data.frame('y' = y_boot,
                                                  "TPOP2020" = X_boot$TPOP2020,
                                                  "Perc_minority" = 100 - X_boot$White_Perc),
                                grid_val = quantile(100 - X_subset$White_Perc,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(100 - X_subset$White_Perc,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("Perc_minority" = 100 - X_subset$White_Perc,y_subset),
                                   aes(x = Perc_minority,y = y_subset))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage Minority")+ylab("Score")+ggtitle("Estimate of ADRF")



# m) Number of pools
gam_list = gam_est(Y = y,treat = n,
                   treat_formula = n~perc_positive,
                   data = data.frame('y' = y_subset,"n" = X_subset$n,
                                     "perc_positive" = X_subset$perc_positive),
                   grid_val = quantile(X_subset$n,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X_subset)[1],dim(X_subset)[1],replace = T)
  X_boot = X_subset[boot_samp,]
  y_boot = y_subset[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = n,
                                treat_formula = n~perc_positive,
                                data = data.frame('y' = y_boot,"n" = X_boot$n,
                                                  "perc_positive" = X_boot$perc_positive),
                                grid_val = quantile(X_subset$n,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X_subset$n,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("n" = X_subset$n,y_subset),
                                   aes(x = n,y = y_subset))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Number of Pools")+ylab("Score")+ggtitle("Estimate of ADRF")


# m) Percentage of positive pools
gam_list = gam_est(Y = y,treat = perc_positive,
                   treat_formula = perc_positive~AVG_CANPY+ClassBest_Pct+
                     Class_21Pct+Class_22Pct+Class_23Pct+Class_24Pct+AVG_IMP+
                     Pct_HUocc,
                   data = data.frame('y' = y_subset,
                                     "perc_positive" = X_subset$perc_positive,"AVG_CANPY" = X_subset$AVG_CANPY,
                                     "ClassBest_Pct"= X_subset$ClassBest_Pct,"Class_21Pct" = X_subset$Class_21Pct,
                                     "Class_22Pct"=X_subset$Class_22Pct,"Class_23Pct" = X_subset$Class_23Pct,
                                     "Class_24Pct" = X_subset$Class_24Pct,"AVG_IMP" = X_subset$AVG_IMP,
                                     "Pct_HUocc"=X_subset$Pct_HUocc),
                   grid_val = quantile(X_subset$perc_positive,probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X_subset)[1],dim(X_subset)[1],replace = T)
  X_boot = X_subset[boot_samp,]
  y_boot = y_subset[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = perc_positive,
                                treat_formula = perc_positive~AVG_CANPY+ClassBest_Pct+
                                  Class_21Pct+Class_22Pct+Class_23Pct+Class_24Pct+AVG_IMP+
                                  Pct_HUocc,
                                data = data.frame('y' = y_boot,
                                                  "perc_positive" = X_boot$perc_positive,"AVG_CANPY" = X_boot$AVG_CANPY,
                                                  "ClassBest_Pct"= X_boot$ClassBest_Pct,"Class_21Pct" = X_boot$Class_21Pct,
                                                  "Class_22Pct"=X_boot$Class_22Pct,"Class_23Pct" = X_boot$Class_23Pct,
                                                  "Class_24Pct" = X_boot$Class_24Pct,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Pct_HUocc"=X_boot$Pct_HUocc),
                                grid_val = quantile(X_subset$perc_positive,probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(X_subset$perc_positive,probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("perc_positive" = X_subset$perc_positive,y_subset),
                                   aes(x = perc_positive,y = y_subset))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Percentage of Postive Pools")+ylab("Score")+ggtitle("Estimate of ADRF")

# n) Number of positive pools
gam_list = gam_est(Y = y,treat = num_positive,
                   treat_formula = num_positive~AVG_CANPY+ClassBest_Pct+
                     Class_21Pct+Class_22Pct+Class_23Pct+Class_24Pct+AVG_IMP+
                     Pct_HUocc,
                   data = data.frame('y' = y_subset,
                                     "num_positive" = log(X_subset$num_positive),"AVG_CANPY" = X_subset$AVG_CANPY,
                                     "ClassBest_Pct"= X_subset$ClassBest_Pct,"Class_21Pct" = X_subset$Class_21Pct,
                                     "Class_22Pct"=X_subset$Class_22Pct,"Class_23Pct" = X_subset$Class_23Pct,
                                     "Class_24Pct" = X_subset$Class_24Pct,"AVG_IMP" = X_subset$AVG_IMP,
                                     "Pct_HUocc"=X_subset$Pct_HUocc),
                   grid_val = quantile(log(X_subset$num_positive),probs = seq(0, 1, by = 0.01)),
                   treat_mod = "Normal")

params = NULL
for(t in 1:1000){
  boot_samp = sample(1:dim(X_subset)[1],dim(X_subset)[1],replace = T)
  X_boot = X_subset[boot_samp,]
  y_boot = y_subset[boot_samp]
  params = rbind(params,gam_est(Y = y,treat = num_positive,
                                treat_formula = num_positive~AVG_CANPY+ClassBest_Pct+
                                  Class_21Pct+Class_22Pct+Class_23Pct+Class_24Pct+AVG_IMP+
                                  Pct_HUocc,
                                data = data.frame('y' = y_boot,
                                                  "num_positive" = log(X_boot$num_positive),"AVG_CANPY" = X_boot$AVG_CANPY,
                                                  "ClassBest_Pct"= X_boot$ClassBest_Pct,"Class_21Pct" = X_boot$Class_21Pct,
                                                  "Class_22Pct"=X_boot$Class_22Pct,"Class_23Pct" = X_boot$Class_23Pct,
                                                  "Class_24Pct" = X_boot$Class_24Pct,"AVG_IMP" = X_boot$AVG_IMP,
                                                  "Pct_HUocc"=X_boot$Pct_HUocc),
                                grid_val = quantile(log(X_subset$num_positive),probs = seq(0, 1, by = 0.01)),
                                treat_mod = "Normal")$param)
}

plot_dat = data.frame(
  "X" = quantile(log(X_subset$num_positive),probs = seq(0, 1, by = 0.01)),
  "Estimate" = gam_list$param,
  "Lower_bd" = 2*gam_list$param - apply(params,2,quantile,0.975),
  "Upper_bd" = 2*gam_list$param - apply(params,2,quantile,0.025))

ggplot(data = plot_dat)+geom_point(data = data.frame("num_positive" = log(X_subset$num_positive),y_subset),
                                   aes(x = num_positive,y = y_subset))+
  geom_ribbon(aes(x = X,ymin = Lower_bd,ymax = Upper_bd),fill = "green",alpha = 0.2)+
  geom_line(aes(x = X,y = Estimate),color = "deepskyblue",lwd = 1.25)+
  xlab("Log Number of Postive Pools")+ylab("Score")+ggtitle("Estimate of ADRF")

