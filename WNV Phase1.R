# Note for Anwesha: Refer to Code for WNV Chicago.R (in folder Code old)

#remotes::install_github("cran/DMwR") - to install package since it has been removed from CRAN
#install_github('mcooper/moranfast')

library(data.table)
library(tidyverse)
library(caret)
library(DMwR)
library(binGroup)
library(spaMM)
library(usa)
library(splines)
library(pbmcapply)
library(ggmap)

preprocess_data = function(year){
  #Reading the data
  path = paste("WNV data by year - Anwesha/",year,".csv",sep = "")
  data = fread(path)
  # Converting the data into a data frame. 
  data = data.frame(apply(data,2,unlist))
  
  # Adding the missing columns associated to other weeks in
  # which no human cases were noted
  # These are columns containing only 0
  
  all_colnames = c(colnames(data[,1:17]),
                   paste("H","1500","W",23:42,sep = ""),
                   paste("H","3000","W",23:42,sep = ""))
  
  missing_colnames = setdiff(all_colnames,colnames(data))
  data_extra = data.frame(matrix(0,nrow = dim(data)[1],
                                 ncol = length(missing_colnames)))
  colnames(data_extra) = missing_colnames
  data = cbind(data,data_extra)
  rm(data_extra)
  data = data[,match(all_colnames,colnames(data))]
  data[,18:dim(data)[2]]= apply(data[,18:dim(data)[2]],2,as.numeric)
  data = transform(data,POOLSIZE = as.numeric(POOLSIZE),
                   WEEK_NEW = as.numeric(WEEK_NEW),
                   POSITIVE = as.factor(POSITIVE),
                   COLLECDATE = as.Date(COLLECDATE,"%m/%d/%y"),
                   LATITUDE = signif(as.numeric(LATITUDE),4),
                   LOGITUDE = signif(as.numeric(LOGITUDE),4),
                   COLLECZIP = as.factor(COLLECZIP))
  rand = sample(nrow(data),nrow(data))
  data$set[rand] = c(rep(1,round(nrow(data)/5)),rep(2,round(nrow(data)/5)),
                     rep(3,round(nrow(data)/5)),rep(4,round(nrow(data)/5)),
                     rep(5,nrow(data) - round(nrow(data)/5)*4))
  data = cbind("YEAR" = year,data)
  return(data)
}

# Creating the full data

data = NULL
for(i in 2004:2018){
  data = rbind(data,preprocess_data(i))
}

## Subset the area
data = data %>%
  filter((LATITUDE>41.4 & LATITUDE < 43 & LOGITUDE > -89 & LOGITUDE < -87.5))

# data = data %>%
#   filter((LATITUDE>41.8 & LATITUDE < 42 & LOGITUDE > -88 & LOGITUDE < -87.8))

# Plotting the subsetted area
map_bounds <- c(left = -89.5, bottom = 41, right = -87, top = 43.2)
coords.map <- get_stadiamap(map_bounds, zoom = 9, maptype = "stamen_toner_lite")
coords.map <- ggmap(coords.map, extent="device", legend="none")

(map = coords.map+
    geom_rect(aes(ymin=41.4, ymax=43, xmin=-89, 
                  xmax=-87.5),alpha = 0.1,fill = "red")+
    geom_rect(aes(ymin=41.8, ymax=42, xmin=-88, 
                  xmax=-87.8),alpha = 0.2,fill = "green")+
    geom_point(data=data,  
               aes(x=LOGITUDE, y=LATITUDE), 
               fill="blue", shape=23,
               alpha=0.8,size = 0.25))

# Subsetting the data by year-week
data = cbind("WEEK_YEAR" = paste(data$WEEK_NEW,data$YEAR),data)

# Getting all the unique elements in week_year
a = unique(data$WEEK_YEAR)

# Calculating the MLE of vector index by week-year - Method : Firth

#Defining a function to calculate the vector index for a week-year
mle_vectorindex = function(week_year){
  y = data %>% filter(data$WEEK_YEAR == week_year) %>% dplyr::select(c("POOLSIZE","POSITIVE"))
  mle_vector_index = pooledBin(x = as.numeric(y$POSITIVE[1])-1,m = y$POOLSIZE[1])
  return(mle_vector_index)
}

#Applying the function to every unique week-year combination
mle = data.frame("WEEK_YEAR" = a,
                 "MLE" = sapply(1:length(a),function(t){mle_vectorindex(a[t])$p}))
mle$MLE = as.numeric(mle$MLE)

# Calculating average and daily abundance
avg_abundance = data %>% group_by(WEEK_YEAR) %>% summarise_at(vars(POOLSIZE),funs(mean))
colnames(avg_abundance)[2] = "avg_abundance"
numpools_bydate = data %>% group_by(COLLECDATE) %>% summarise(number = n())
colnames(numpools_bydate)[2] = "daily_abundance"

#Creating the columns related to abundance and MLE
data = left_join(data,avg_abundance)
data= left_join(data,numpools_bydate)
data = left_join(data,mle)

# Calculating relative risk
data$RELATIVE_RISK = data$daily_abundance*data$MLE/1000
data = data[,c(61:64,1:59,60)]

# Defining the response variable (Y) as the indicator of whether a  
# human case has occurred within 2 weeks after the testing of the pool.  
# First column of Y gives in a range of 1500m from location of trap
# Second column of Y gives between 1500 to 3000

human_case_weeks = as.numeric(sapply(24:(dim(data)[2]-1), function(t){
  paste(unlist(strsplit(colnames(data)[t],
                        split = ""))[7:8],
        collapse  = "")
}))

# Find indicator if case has happened 1 week after pool tested
week_1_ind = t(sapply(1:dim(data)[1],function(t){
  data[t,which(human_case_weeks == data$WEEK_NEW[t]+1)+23]}))
week_1_ind = apply(week_1_ind,2,as.logical)

#Find indicator if case happened 2 weeks after pool tested
week_2_ind = t(sapply(1:dim(data)[1],function(t){
  data[t,which(human_case_weeks == data$WEEK_NEW[t]+2)+23]}))
week_2_ind = apply(week_2_ind,2,as.logical)

Y = week_1_ind+ week_2_ind
rm(week_1_ind,week_2_ind)
Y = apply(Y,2,as.logical)
Y = apply(Y,2,as.numeric)

##########################
#Defining a time variable 
###########################

time_data = data %>%  dplyr::select(c("WEEK_NEW","YEAR")) %>% unique
time_data = time_data[with(time_data,order(YEAR,WEEK_NEW)),]
time_data = cbind.data.frame(time_data,"TIME" = 1:dim(time_data)[1])

data = left_join(data,time_data)
rm(time_data)

####################################
### Dealing with spatial correlation
####################################

#Creating the X (model Matrix)
x = data %>% dplyr::select(c("POOLSIZE","POSITIVE","RELATIVE_RISK",
                             "WEEK_NEW","YEAR","long" = "LOGITUDE",
                             "lat" = "LATITUDE","zip" = "COLLECZIP","set"))

#Creating unique names for the different traps
unique_locs = x %>% group_by(lat,long,zip) %>%  count
unique_locs$ID = paste("Trap",1:nrow(unique_locs))
x = left_join(x,unique_locs,by = c("lat","long","zip"))

# Adding year as a factor variable 
x$YEAR = as.factor(x$YEAR)

#Creating the Y variable
Y_1500 = as.factor(Y[,1])

#Creating the final data frame for the regression 
dat = cbind.data.frame("y"= Y_1500, x)

########################################################################
####### Making a function to do the cross Validation ###################
########################################################################


CV = function(fold,m=0.9){
  # Get the train data
  dat_train = dat %>% filter(set != fold)
  # Get the test data
  dat_test = dat %>% filter(set == fold)
  # Fit the spatial logistic regression model
  model = fitme(y~POOLSIZE+POSITIVE+RELATIVE_RISK+WEEK_NEW+YEAR+Matern(1|lat+long),
                data=dat_train,method="REML",family = "binomial")
  # Predict for the test
  pred_prob = predict(model,newdata = dat_test,type = "response")
  # AUC calculation
  roc = roc.curve(dat_test$y,pred_prob,plotit = F)
  auc = roc$auc
  # Best thresholding using m
  J.stat= roc$true.positive.rate - m*roc$false.positive.rate
  best_threshold = roc$thresholds[which.max(J.stat)]
  pred = pred_prob
  
  # Finding trap wise sensitivity and specificity
  n = num_positive = sensitivity = specificity = ID = numeric(length(unique(dat_test$ID)))
  for(i in 1:length(unique(dat_test$ID))){
    dat_trap = dat_test %>% filter(dat_test$ID == unique(dat_test$ID)[i])
    ID[i] = unique(dat_test$ID)[i]
    n[i] = dim(dat_trap)[1]
    num_positive[i] = table(dat_trap$y)[2]
    pred_trap = as.data.frame(pred) %>% filter(dat_test$ID == unique(dat_test$ID)[i])
    pred_trap = as.factor(ifelse(pred_trap<=best_threshold,0,1))
    
    conf_Mat = confusionMatrix(pred_trap,dat_trap$y,positive = "1")
    sensitivity[i] = conf_Mat$byClass[1]
    specificity[i] = conf_Mat$byClass[2]
  }
  trap_results = data.frame(cbind(ID,n,num_positive,
                                  sensitivity,specificity,auc))
  return(trap_results)
}

result_cv = pbmclapply(1:5,CV,mc.cores = 5)

result_cv = c(list(data.frame("ID" = unique(dat$ID))),result_cv)
result_cv = result_cv %>% reduce(left_join, by='ID')
write.csv(result_cv,"Phase1Results.csv")
