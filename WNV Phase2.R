# Note for Anwesha: Refer to WNV After Cluster Sep 22.R

library(data.table)
library(tidyverse)

result_cv = fread("Phase1Results.csv") %>% 
  as.data.frame
result_cv = result_cv[,-1]
colnames(result_cv)

makeNA20 = function(x){
  x[is.na(x)] = 0
  return(x)}

# Making NA to 0 in n
result_cv[,c(2,7,12,17,22)] = apply(result_cv[,c(2,7,12,17,22)], 2, makeNA20)
#Making NA to 0 in num_positive
result_cv[,c(3,8,13,18,23)] = apply(result_cv[,c(3,8,13,18,23)], 2, makeNA20)

# Putting tag "No Trap chosen" for cases where trap wasn't chosen in sensitivity
# and specificity 

for(i in c(6,11,16,21,26)){
  result_cv[is.na(result_cv[,i]),(i-2)] = "No Trap chosen"
}

# Making the n and num_positive to numeric
result_cv[,-c(1,4,9,14,19,24)] = apply(
  result_cv[,-c(1,4,9,14,19,24)],2,as.numeric)

# n - total number of pools
# n_max/min = max/min number of pools out of CV sets
# num_positive - total number of positive pools
# num_positive_max/min = max/min number of positive pools out of CV sets
# sensitivity - avg sensitivity over cv
# specificity - avg specificity over cv
# auc - avg auc of the models

result_cv$n_max = apply(result_cv %>% dplyr::select(c(starts_with("n."),n)),1,max)
result_cv$num_positive_max = apply(result_cv %>% 
                                     dplyr::select(c(starts_with("num_positive"))),
                                   1,max)
result_cv$n_min = apply(result_cv %>% dplyr::select(c(starts_with("n."),n)),1,min)
result_cv$num_positive_min = apply(result_cv %>% 
                                     dplyr::select(c(starts_with("num_positive"))),
                                   1,min)

result_cv$num_positive = rowSums(result_cv %>% 
                                   dplyr::select(c(starts_with("num_positive"))))
result_cv$n = rowSums(result_cv %>% dplyr::select(c(starts_with("n."),n)))


auc_avg = max(rowSums(result_cv %>% 
                        dplyr::select(c(starts_with("auc"))))/5,
              na.rm = T)
result_cv$avg_specificity = 
  apply(result_cv[,startsWith(colnames(result_cv),"specificity")],1,mean,na.rm = T)

sens = result_cv[,startsWith(colnames(result_cv),"sensitivity")]
result_cv$avg_sensitivity = NULL
for(i in 1:nrow(result_cv)){
  row = sens[i,]
  if("No Trap chosen" %in% row){
    row = row[-which(row == "No Trap chosen")]
  }
  result_cv$avg_sensitivity[i] = mean(as.numeric(row),na.rm = T)
}

rm(sens,row)
# Removing the unnecessary columns
result_cv[,startsWith(colnames(result_cv),"n.")] = NULL
result_cv[,startsWith(colnames(result_cv),"num_positive.")] = NULL
result_cv[,startsWith(colnames(result_cv),"auc")] = NULL
result_cv[,startsWith(colnames(result_cv),"specificity")] = NULL
result_cv[,startsWith(colnames(result_cv),"sensitivity")] = NULL

###########################################################################################
# Attach it with the locations 
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
unique_locs = data %>% dplyr::select("lat" = LATITUDE,"long" = LOGITUDE,"zip" = "COLLECZIP") %>%
  group_by(lat,long,zip) %>%  count
unique_locs$ID = paste("Trap",1:nrow(unique_locs))

############# Joining the Phase 1 results with the locations ###################

trap_results = left_join(result_cv,unique_locs,by = "ID")
trap_results$n.y = NULL
colnames(trap_results)[colnames(trap_results) == 'n.x'] <- "n"

# Getting the traps with atleast one human case in their vicinity
trap_results_sens = na.omit(trap_results)

#Calculating the score
trap_results_sens$score = (0.9*trap_results_sens$avg_specificity + 
                             trap_results_sens$avg_sensitivity)/1.9

while(sum(duplicated(trap_results_sens[,11:10]))!=0){
  trap_results_sens[duplicated(trap_results_sens[,11:10]),11:10] = 
    trap_results_sens[duplicated(trap_results_sens[,11:10]),11:10]+c(0.009,0.009)
}

#Saving the results
saveRDS(trap_results, "trapAug2022.rds")
saveRDS(trap_results_sens, "trapsensAug2022.rds")


