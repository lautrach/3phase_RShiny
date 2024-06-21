# Note for Anwesha: Refer to WNV After Cluster Sep 22

################################################################################
################### Important Plots ############################################
################################################################################

library(ggmap)
library(RColorBrewer)

trap_results = readRDS("/Users/laura/Desktop/3phase_RShiny/trapAug2022.rds")
trap_results_sens = readRDS("/Users/laura/Desktop/3phase_RShiny/trapsensAug2022.rds")

#Stadia Maps API
ggmap::register_stadiamaps(key = '170f7302-0a84-41f9-b560-ff13b8e1c647')

################## Plotting the locations #####################################
map_bounds <- c(left = -89.2, bottom = 41.3, right = -87.3, top = 42.7)
coords.map <- get_stadiamap(map_bounds, zoom = 9, maptype = "stamen_toner_lite")
coords.map <- ggmap(coords.map, extent="device", legend="none")
(coords.map <- coords.map + geom_point(data=trap_results,  
                                       aes(x=long, y=lat), 
                                       fill="green", shape=23,
                                       alpha=0.8,size = 1) ) 

# Locations for traps with atleast one human case around it (T*)
map_bounds <- c(left = -89.2, bottom = 41.3, right = -87.3, top = 42.7)
coords.map <- get_stadiamap(map_bounds, zoom = 9, maptype = "stamen_toner_lite")
coords.map <- ggmap(coords.map, extent="device", legend="none")
(coords.map <- coords.map + geom_point(data=trap_results_sens,  
                                       aes(x=long, y=lat), 
                                       fill="green", shape=23,
                                       alpha=0.8,size = 1) )

################### Plotting Spec and Sens values ###############################
#Specificity atleast one case
ggplot(data = trap_results,aes(x= avg_specificity)) +
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)+
  geom_vline(aes(xintercept=mean(avg_specificity)),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Histogram of Specificity values for Model 1")
#Specificity atleast one case
ggplot(data = trap_results_sens,aes(x= avg_specificity)) +
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)+
  geom_vline(aes(xintercept=mean(avg_specificity)),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Histogram of Specificity values for traps with atleast one case Model 1")
#Sensitivity atleast one case
ggplot(data = trap_results_sens,aes(x= avg_sensitivity)) +
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)+
  geom_vline(aes(xintercept=mean(avg_specificity)),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Histogram of Sensitivity values for traps with atleast one case Model 1")

################### Plotting Scores ############################################

# Histogram of scores:
ggplot(data = trap_results_sens,aes(x= score)) +
  geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)+
  geom_vline(aes(xintercept=mean(score)),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Histogram of Scores for traps with atleast one case")


# Plotting heatmap of scores on map of Chicago
map_bounds <- c(left = -88.8, bottom = 41.3, right = -87.3, top = 42.4)
coords.map <- get_stadiamap(map_bounds, zoom = 9, maptype = "stamen_toner_lite")
coords.map <- ggmap(coords.map, extent="device", legend="none")
coords.map <- coords.map %+% trap_results_sens + aes(x = long,y = lat,z = score) +
  stat_summary_2d(fun = median,geom = "tile", 
                  binwidth = c(0.028, 0.028),
                  alpha = 0.80)+
  scale_fill_gradientn(colours=(brewer.pal(7, "RdYlGn")))
coords.map <- coords.map + theme_bw()
coords.map

################################################################################
################### Secondary Plots ############################################
################################################################################

############### Plotting heatmapsfor sensitivity and specificity ###############

plot_heatmap = function(df,z = "specificity",binwidth_h=0.1,
                        binwidth_v=0.1,fun=median,point_disp = F){
  dat = df[c("lat","long",z)]
  colnames(dat)[3] = "z" 
  map_bounds <-  c(left = -89.2, bottom = 41.3, right = -87.3, top = 42.7)
  coords.map <- get_stadiamap(map_bounds, zoom = 9, maptype = "stamen_toner_lite")
  coords.map <- ggmap(coords.map, extent="device", legend="none")
  coords.map <- coords.map %+% dat + aes(x = long,y = lat,z = z) +
    stat_summary_2d(fun = fun,geom = "tile", 
                    binwidth = c(binwidth_h, binwidth_v),
                    alpha = 0.5)+
    scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")))
  coords.map <- coords.map + theme_bw()
  if(point_disp == T){
    coords.map <- coords.map + geom_point(data=df,  
                                          aes(x=long, y=lat), 
                                          fill="red", shape=23,
                                          alpha=0.8,size = 0.5)  
  }
  coords.map
}

# Heatmap of specificity of all traps - median with points
plot_heatmap(df = trap_results,z = "avg_specificity",0.05,0.05,median,T)

# Heatmap of specificity of all traps - median
plot_heatmap(df = trap_results,z = "avg_specificity",0.05,0.05,median,F)

# Heatmap of specificity of all traps - max
plot_heatmap(df = trap_results,z = "avg_specificity",0.05,0.05,max,F)

# Heatmap of specificity of all traps - min
plot_heatmap(df = trap_results,z = "avg_specificity",0.05,0.05,min,F)

# Heatmap of sensitivity of traps with atleast one human case - median
plot_heatmap(df = trap_results_sens,z = "avg_sensitivity",0.05,0.05,median,F)

# Heatmap of specificity of traps with atleast one human case - median
plot_heatmap(df = trap_results_sens,z = "avg_specificity",0.05,0.05,median,F)

############### Plotting traps by good/bad (over/below mean score) #############

cutoff = mean(trap_results_sens$score)
Good_bad.df = trap_results_sens
Good_bad.df$Trap_type = as.factor(ifelse(trap_results_sens$score<cutoff,0,1))
levels(Good_bad.df$Trap_type) =c("Bad","Good")
table(Good_bad.df$Trap_type)


map_bounds <- c(left = -89, bottom = 41, right = -87, top = 42.5)
coords.map <- get_stadiamap(map_bounds, zoom = 9, maptype = "stamen_toner_lite")
coords.map <- ggmap(coords.map, extent="device", legend="none")

(map = coords.map+geom_point(data=Good_bad.df,  
                             aes(x=long, y=lat,colour = Trap_type),shape=19,
                             alpha=0.8,size = 2))


