#Note for Anwesha: Refer to  Regression of scores.R

# Load in the results from Phase 2
trap_results = readRDS("trapAug2022.rds")

library(data.table)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(rgdal)
library(sf) #Alternative option might work...?
library(ggmap)

#-----------------Making the dataframe --------------------------------------------------------------

# Dependent Variable : Score of the trap

# Independent Variables:
# 1) n: Number of pools collected
# 2) num_positive: Number of positive pools
# 3) avg_specificity: Specificity over 5 CV sets
# 4) avg_sensitivity: Sensitivity over 5 CV sets
# 5) lat: latitude
# 6) long: longitude
# 7) zip: zipcode
# 8) Class_xxPct: Percentage coverage of each type (xx) of landscape
# 9) MajorLC: Major Land Coverage 
# 10) AVG_IMP: Average impervious land
# 11) AVG_CANPY: Average canopy coverage
# 12) TPOP2020: Total population
# 13) White: Total White population
# 14) Black: Total Black population
# 15) TwoOrMore: Total population belonging to two or more races
# 16) Hispanic: Total Hispanic population
# 17) TOT18andUp: Total population 18 and up
# 18) TotHU: Number of Housing units
# 19) HU_Occ: Number of occupied housing units
# 20) HUVac: Number of vacant housing units (sum of the two above)
# 21) HSgradPct: Percent high school graduates 
# 22) FamPovPct: Percent of families in poverty

Landscape_data = fread("Trap_Landscape.csv")
Census_data = fread("Anwesha_CensusResults.csv")


cov = inner_join(Landscape_data,Census_data[,-c(1,3,4,5,6)],by = "ID")
actual_dat = inner_join(trap_results,cov,"ID")

# Matching the IDs
weird = actual_dat[which(actual_dat$zip.x != actual_dat$zip.y),]
weird = weird[order(weird$ID),]
weird[1:3,13:ncol(weird)] = weird[c(2,3,1),13:ncol(weird)] 
weird[4:5,13:ncol(weird)] = weird[c(5,4),13:ncol(weird)] 
weird[6:7,13:ncol(weird)] = weird[c(7,6),13:ncol(weird)] 
weird[8:9,13:ncol(weird)] = weird[c(9,8),13:ncol(weird)] 
weird[10:11,13:ncol(weird)] = weird[c(11,10),13:ncol(weird)] 
weird[12:13,13:ncol(weird)] = weird[c(13,12),13:ncol(weird)] 
actual_dat[which(actual_dat$zip.x != actual_dat$zip.y),] = weird


colnames(actual_dat)
actual_dat[,endsWith(colnames(actual_dat),".y")] = NULL

colnames(actual_dat)[endsWith(colnames(actual_dat),".x")]  = gsub(".x", '',colnames(actual_dat)[endsWith(colnames(actual_dat),".x")])

# Calculated score again since we used the full data with 1062 traps to merge 
actual_dat$score = (0.9*actual_dat$avg_specificity + 
                      actual_dat$avg_sensitivity)/1.9

# Saving the dataframe 
write.csv(actual_dat,"Data for score regression.csv",row.names = F)

# #################################################################################
#--------------- Some EDA (Exploratory Data Analysis) -------------------------
# #################################################################################

# Considering traps in T* (atleast one human case in vicinity)
actual_dat.sens = na.omit(actual_dat)
X = actual_dat.sens %>% select(-c("ID","n_max", "num_positive_max","n_min",
                                  "num_positive_min","zip","TID","SortID","BUFF_DIST",
                                  "HUVac","score"))
X = apply(X,2,as.numeric)
X  = as.data.frame(X[,-c(3,4,5,6)])
y = actual_dat.sens$score

# Plotting Score
ggplot(data = data.frame("y" = y),aes(x = y)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)

# Finding correlation of all the X variables 
corrplot::corrplot(cor(X))

# Making percentage occupied as a variable
X = X %>% mutate("Pct_HUocc" = HU_Occ/TotHU*100 )
X = X %>% mutate("HispTwoOrMore" = Hispanic+TwoOrMore)
X = X %>% select(-c("HU_Occ","TotHU","TOT18andUp","Hispanic","TwoOrMore"))

#corrplot::corrplot(cor(X))


# Class_xxPct: Percentage coverage of each type (xx) of landscape
ggplot(data = actual_dat.sens,aes(x = Class_11Pct)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)
ggplot(data = actual_dat.sens,aes(x = Class_21Pct)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)
ggplot(data = actual_dat.sens,aes(x = Class_22Pct)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)
ggplot(data = actual_dat.sens,aes(x = Class_23Pct)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)
ggplot(data = actual_dat.sens,aes(x = Class_24Pct)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)
ggplot(data = actual_dat.sens,aes(x = Class_31Pct)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)
ggplot(data = actual_dat.sens,aes(x = Class_41Pct)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)
ggplot(data = actual_dat.sens,aes(x = Class_42Pct)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)
ggplot(data = actual_dat.sens,aes(x = Class_43Pct)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)
ggplot(data = actual_dat.sens,aes(x = Class_52Pct)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)
ggplot(data = actual_dat.sens,aes(x = Class_71Pct)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)
ggplot(data = actual_dat.sens,aes(x = Class_81Pct)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)
ggplot(data = actual_dat.sens,aes(x = Class_82Pct)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)
ggplot(data = actual_dat.sens,aes(x = Class_90Pct)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)
ggplot(data = actual_dat.sens,aes(x = Class_95Pct)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)

# Idea: Instead of taking percentage of cover for each of them, find which type
# has max coverage and then find value of that coverage

X$ClassBest_Pct = apply(X %>% select(starts_with("Class_")),1,max) 
X = X %>% select(-starts_with("Class_"))
extra_cols = startsWith(colnames(actual_dat.sens),"Class_2")
X = cbind.data.frame(X,actual_dat.sens[,extra_cols])

#corrplot::corrplot(cor(X))

ggplot(data = X,aes(x = ClassBest_Pct)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)

ggplot(data = data.frame("score" = y,X),aes(x = ClassBest_Pct, y = score)) +
  geom_point() + geom_smooth(method = "loess",col = "darkgreen")

ggplot(data = data.frame("score" = y,X),aes(x = Class_21Pct, y = score)) +
  geom_point() + geom_smooth(method = "loess",col = "darkgreen")
ggplot(data = data.frame("score" = y,X),aes(x = Class_22Pct, y = score)) +
  geom_point() + geom_smooth(method = "loess",col = "darkgreen")
ggplot(data = data.frame("score" = y,X),aes(x = Class_23Pct, y = score)) +
  geom_point() + geom_smooth(method = "loess",col = "darkgreen")
ggplot(data = data.frame("score" = y,X),aes(x = Class_24Pct, y = score)) +
  geom_point() + geom_smooth(method = "loess",col = "darkgreen")


# Major Landscape Cover

X = X %>% mutate(MajorLC = as.factor(MajorLC))
table(X$MajorLC)
levels(X$MajorLC) = list("21" = c(21),
                         "22" = c(22),
                         "23" = c(23),
                         "24" = c(24),
                         "90" = c(90),
                         "0" = c(11,41,71,81))
# levels(X$MajorLC) = list("21" = c(21),
#                          "22" = c(22),
#                          "23" = c(23),
#                          "24" = c(24),
#                          "90" = c(90),
#                          "0" = c(11,41,71,81,31,82,95))


ggplot(data=data.frame("score" = y,X), 
       mapping = aes(x = MajorLC, y = score,fill = MajorLC)) + 
  geom_boxplot(alpha=0.7) +
  theme(legend.position="none") 



# Average Impervious
ggplot(data = actual_dat.sens,aes(x = AVG_IMP)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)


ggplot(data = actual_dat.sens,aes(x = AVG_IMP, y = score)) +
  geom_point() + geom_smooth(method = "loess",col = "darkgreen")

# Average Canopy
ggplot(data = actual_dat.sens,aes(x = AVG_CANPY)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)


ggplot(data = actual_dat.sens,aes(x = AVG_CANPY, y = score)) +
  geom_point() + geom_smooth(method = "loess",col = "darkgreen")

# Total Population
ggplot(data = actual_dat.sens,aes(x = TPOP2020)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)


ggplot(data = actual_dat.sens,aes(x = TPOP2020, y = score)) +
  geom_point() + geom_smooth(method = "loess",col = "darkgreen")

# Races - Black, White, Hisp+TwoOrMore
X = X %>% mutate(Black_Perc = (Black/TPOP2020)*100,
                 White_Perc = (White/TPOP2020)*100,
                 HispOrTwo_Perc = (HispTwoOrMore/TPOP2020)*100) %>% 
  select(-c(Black,White,HispTwoOrMore))

ggplot(data = data.frame("score" = y,X),aes(x = White_Perc)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)
ggplot(data = data.frame("score" = y,X),aes(x = Black_Perc)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)
ggplot(data = data.frame("score" = y,X),aes(x = HispOrTwo_Perc)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)


ggplot(data = data.frame("score" = y,X),aes(x = White_Perc, y = score)) +
  geom_point() + geom_smooth(method = "loess",col = "darkgreen")
ggplot(data = data.frame("score" = y,X),aes(x = log(Black_Perc), y = score)) +
  geom_point() + geom_smooth(method = "loess",col = "darkgreen")
ggplot(data = data.frame("score" = y,X),aes(x = log(HispOrTwo_Perc), y = score)) +
  geom_point() + geom_smooth(method = "loess",col = "darkgreen")


# High school graduates percentage
ggplot(data = actual_dat.sens,aes(x = HSgradPct)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)

ggplot(data = actual_dat.sens,aes(x = HSgradPct, y = score)) +
  geom_point() + geom_smooth(method = "loess",col = "darkgreen")

# Percentage Occupied Houses
ggplot(data = data.frame("score" = y,X),aes(x = Pct_HUocc)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)

ggplot(data = data.frame("score" = y,X),aes(x = Pct_HUocc, y = score)) +
  geom_point() + geom_smooth(method = "loess",col = "darkgreen")

# Percentage of families in poverty
ggplot(data = data.frame("score" = y,X),aes(x = FamPovPct)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)

ggplot(data = data.frame("score" = y,X),aes(x = FamPovPct, y = score)) +
  geom_point() + geom_smooth(method = "loess",col = "darkgreen")

# n - Number of pools from trap

ggplot(data = actual_dat.sens,aes(x = n)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)

ggplot(data = actual_dat.sens,aes(x = log(n), y = score)) +
  geom_point() + geom_smooth(method = "loess",col = "darkgreen")


# num_positive: Number of positive pools

ggplot(data = actual_dat.sens,aes(x = num_positive)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)


ggplot(data = actual_dat.sens,aes(x = num_positive, y = score)) +
  geom_point() + geom_smooth(method = "loess",col = "darkgreen")

# perc_positive: Percentage of positive pools
X = X %>% mutate(perc_positive = (num_positive/n)*100) 
ggplot(data = data.frame("score" = y,X),aes(x = perc_positive)) + 
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)


ggplot(data = data.frame("score" = y,X),aes(x = perc_positive, y = score)) +
  geom_point() + geom_smooth(method = "loess",col = "darkgreen")


# Final correlation plot for all variables
corrplot::corrplot(cor(X))

#------------- Plot the MAD ----------------------------------------------------
# 1 - Wheaton Mosquito Abatement District
# 3 - WILL COUNTY HIGHWAY DEPT IL , IL,
# 13 - GRUNDY CO HEALTH DEPT IL
# 15 - KANKAKEE CO HEALTH DEPT IL
# 21 - DEKALB CO HEALTH DEPT
# 27 - LASALLE CO HEALTH DEPT IL , ,
# 40 - MCHENRY CO HEALTH DEPT
# 41 - KANE COUNTY HEALTH DEPT IL
# 49 - BOONE COUNTY HEALTH DEPT
# 53 - Claremdon Hills Blackhawk (Small)
# 54 - Glen Ellyn  (Small)
# 55 - Itasca (Small)
# 56 - Milton Twp(Small)
# 57 - West Chicago (Small)
# 58 - Wheaton (Small)
# 59 - South Cook County Mosquito Abatement District 
# 60 - Desplaines Valley Mosquito Abatement District
# 61 - North Shore Mosquito Abatement District
# 62 - Northwest Mosquito Abatement District
# 64 - Lake County Health Department
# 65 - Cook County Health Department
# 66 - Township of Stickney (Small)
# 67 - Township of Stickney (Small)
# 68 - City of Chicago

boundary <- readOGR("IL_Districts/IL_Districts.shp")
boundary <- spTransform(boundary, CRS("+proj=longlat +datum=WGS84"))

# #################################################################################
# ################# Test for which MAD trap belongs to  ###########################
# #####################################################################################
# MAD_num = rep(0,1062)
# for(i in 1:70){
#   MAD = boundary[i,1:2]
#   longlat = actual_dat %>%  select("lat","long")
#   coordinates(longlat) <- ~ long + lat
#   proj4string(longlat) <- proj4string(MAD)
#   ans = over(longlat,MAD)[,1]
#   ans[ans!="NA"]= i
#   ans[is.na(ans)] = 0
#   MAD_num= MAD_num + as.numeric(ans)
# }
boundary <- fortify(boundary)
map_bounds <- c(left = -90, bottom = 41, right = -87, top = 42.5)
coords.map <- get_stadiamap(map_bounds, zoom = 9, maptype = "stamen_toner_lines")
coords.map <- ggmap(coords.map, extent="device", legend="none")

MAD_names = data.frame("Names" = c("Wheaton MAD",
                                   "Will County HD",
                                   "Grundy County HD",
                                   "Kankakee County HD",
                                   "Dekalb County HD",
                                   "LaSalle County HD",
                                   "Winnebago county HD",
                                   "McHenry County HD",
                                   "Kane County HD",
                                   "Boone County HD",
                                   "Claremdon Hills Blackhawk MAD",
                                   "Glen Ellyn",
                                   "Itasca MAD",
                                   "Milton Twp",
                                   "West Chicago",
                                   "Wheaton (Small)",
                                   "South Cook County MAD",
                                   "Desplaines Valley MAD",
                                   "North Shore MAD",
                                   "Northwest MAD",
                                   "Lake County HD",
                                   "Cook County HD",
                                   "Township of Stickney -1",
                                   "Township of Stickney -2",
                                   "City of Chicago"
),
"DeptID" = c("1","3","13","15","21","27","36","40",
             "41","49","53","54","55","56",
             "57","58","59","60","61","62",
             "64","65","66","67","68"))

boundary = left_join(boundary,MAD_names,by = c("id" = "DeptID"))
boundary = na.omit(boundary)

coords.map +
  geom_polygon(aes(x=long, y=lat, fill=Names,color=Names), 
               size=0.8, data=boundary, alpha=0.6) +
  # geom_text(data = coordinates.df,
  #           aes(label = names,x = long_mean,y = lat_mean),
  #           size = 3)+
  theme(legend.text=element_text(size=18),legend.spacing.y = unit(0.25, 'cm'))+
  guides(fill = guide_legend(ncol = 1,byrow = TRUE)) +
  ggtitle("Maps of Mosquito Abatement Districts/ Health Departments") 

# Adding points
trap_results = readRDS("trapAug2022.rds")

coords.map +
  geom_polygon(aes(x=long, y=lat, fill=Names,color=Names), 
               size=0.8, data=boundary, alpha=0.6) +
  # geom_text(data = coordinates.df,
  #           aes(label = names,x = long_mean,y = lat_mean),
  #           size = 3)+
  theme(legend.text=element_text(size=18),legend.spacing.y = unit(0.25, 'cm'))+
  guides(fill = guide_legend(ncol = 1,byrow = TRUE)) +
  ggtitle("Maps of Mosquito Abatement Districts/ Health Departments") +
  geom_point(data=trap_results,  
             aes(x=long, y=lat),size = 1)


# Saving the final pre - processed data for regression and causal inference
write.csv(X,"Processed X for Score regression.csv",row.names = F)











