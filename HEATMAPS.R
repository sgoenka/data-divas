install.packages("sf")
install.packages("rgeos")
install.packages("rgdal")
install.packages("maptools")
install.packages("geosphere")

library(rgeos)
library(maptools)
library(rgdal)

library(sf)
library(sp)

shp_file <- "/Users/arytwins/Desktop/data1030/crime/Census 2010_ Tracts for San Francisco/geo_export_a6a3feaf-6d76-47b1-b686-de403b329b81.shp"
sf <- st_read(shp_file)

plot(sf)
plot(st_geometry(sf))
plot(sf['awater10'])

sf_no_water<-sf[!(sf$awater10>70000000),]
plot(sf_no_water)

sf_no_water_alt<-sf[!(sf$awater10>20000000),]
plot(sf_no_water_alt)

sf_play <- sf_no_water_alt

sf_play$probs <- runif(193, min=0, max=1) # generate random probabilities
plot(sf_play['probs'])
sf_play$probs2 <- runif(193, min=0, max=1) # generate random probabilities
plot(sf_play[c('probs', 'probs2')])

# legend on the bottom
plot(sf_play["probs"], key.pos = 1)
plot(sf_play[c('probs', 'probs2')], key.pos = 1)

# experimenting with colors
dev.off()
colors_alt <- colorRampPalette(c('aliceblue', 'red'))(21)
plot(sf_play["probs"], col = colors_alt, border = 'grey')


# area-weighted interpolation, uses st_intersection to interpolate or redistribute 
#attribute values, based on area of overlap
require(ggplot2)
g = st_make_grid(sf_play, n = c(40,20))
a1 = st_interpolate_aw(sf_play['probs'], g, extensive = FALSE)
a2 = st_interpolate_aw(sf_play['probs'], g, extensive = TRUE)
a1$what = "intensive"
a2$what = "extensive"
library(ggplot2)
l = st_cast(sf_play, "LINESTRING")
ggplot() + geom_sf(data = rbind(a1,a2), aes(fill = probs)) + 
  geom_sf(data = l, col = 'lightgray') + facet_grid(what~.) +
  scale_fill_gradient2(low = "lightcyan", high = "red", mid = "lightskyblue3" ,
                       midpoint = 0.5, limit = c(0,1), name="")

# another heatmap
# quartz()
ggplot(sf_play) +
  geom_sf(aes(fill = probs)) + 
            scale_fill_gradient2(low = "lightcyan", high = "red", mid = "lightskyblue3" ,
                                 midpoint = 0.5, limit = c(0,1), name="") +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(3,"cm")) +
  labs(x = "Predicted probability of crime") 


# plot centroids in each tract
centr = st_centroid(sf_play)
dev.off()
plot(sf_play["probs"], col = 'lightblue', border = 'grey')
plot(centr["probs"], pch = 3, col = 'red', add = TRUE)
    
# plot 3 points in one tract                                              
spatial_sf <- as(sf_play, 'Spatial')    
points_pol1 <- spsample(spatial_sf@polygons[[1]], n = 3, "nonaligned")
dev.off()
plot(sf_play["probs"], col = 'lightblue', border = 'grey')
plot(points_pol1, pch = 3, col = 'red', add = TRUE)

# plot 5 points in another tract                                              
spatial_sf <- as(sf_play, 'Spatial')    
points_pol1 <- spsample(spatial_sf@polygons[[1]], n = 4, "nonaligned")
points_pol17 <- spsample(spatial_sf@polygons[[17]], n = 5, "nonaligned")
dev.off()
plot(sf_play["probs"], col = 'lightblue', border = 'grey')
plot(points_pol1, pch = 3, col = 'red', add = TRUE)
plot(points_pol17, pch = 3, col = 'magenta', add = TRUE)


# using ggplot
point.1 <- data.frame(points_pol1)

ggplot() +
  geom_sf(data = sf_play, aes(fill = probs)) + 
  scale_fill_gradient(low = "lightcyan", high = "lightcyan") +
  geom_point(data=point.1, aes(x=x1, y=x2), color ="red") +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position="none") +
  labs(x = "Predicted crimes")
  
# change shape
ggplot() +
  geom_sf(data = sf_play, aes(fill = probs)) + 
  scale_fill_gradient(low = "lightcyan", high = "lightcyan") +
  geom_point(data=point.1, aes(x=x1, y=x2), 
             color ="red", shape = 3, size = 3) +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position="none") +
  labs(x = "Predicted crimes")

ggplot(sf_play) +
  geom_sf(aes(fill = probs2)) + 
  scale_fill_gradient2(low = "bisque", high = "chocolate3", mid = "darksalmon" ,
                       midpoint = 0.5, limit = c(0,1), name="")   +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(3,"cm")) +
  labs(x = "Accuracy of predictions") 


# accuracies
ggplot(sf_play) +
  geom_sf(aes(fill = probs2)) + 
  scale_fill_gradient2(low = "bisque", high = "chocolate3", mid = "darksalmon" ,
                       midpoint = 0.5, limit = c(0,1), name="") +
  geom_point(data=point.1, aes(x=x1, y=x2), 
             color = "black", fill = "red", shape = 23, size = 2.5) +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(3,"cm")) +
  labs(x = "Accuracy of predictions with predictions") 



########### SIMULATED CRIMES
sim.12hr <- read.csv("/Users/arytwins/Desktop/data1030/crime/data/sim_12hr.csv")
sim.12hr.mod <- sim.12hr[c("census_tract","sim_crime","actual_crime")]
colnames(sim.12hr.mod) <- c("name10","sim_crime","actual_crime") 
merged <- merge(x = sf_play, y = sim.12hr.mod, by = "name10", all.x = TRUE)

plot(merged[c('sim_crime','actual_crime')])

require(gridExtra)

sim12 <- ggplot(merged) +
  geom_sf(aes(fill = sim_crime)) + 
  scale_fill_gradient2(low = "mediumblue", high = "goldenrod1", mid = "mediumpurple1" ,
                       midpoint = 4, limit = c(0,9), name="") +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(3,"cm")) +
  labs(x = "Simulated Crimes") 

actual12 <- ggplot(merged) +
  geom_sf(aes(fill = actual_crime)) + 
  scale_fill_gradient2(low = "mediumblue", high = "goldenrod1", mid = "mediumpurple1" ,
                       midpoint = 4, limit = c(0,9), name="") +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(3,"cm")) +
  labs(x = "Actual Crimes") 

#grid.arrange(sim12, actual12, ncol=2)
grid_arrange_shared_legend(sim12,actual12)


grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}


merged_crimes<-merged[!(merged$sim_crime==0),]
merged_sf <- as(merged_crimes, 'Spatial')    

dev.off()
plot(sf_play["probs"], col = 'lightblue', border = 'grey')
for (i in 1:length(merged_sf@data$sim_crime)){
  points_sim <- spsample(spatial_sf@polygons[[i]], n = merged_sf@data$sim_crime[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'red', add = TRUE)
}



######################
sim.12hr <- read.csv("/Users/arytwins/Desktop/data1030/crime/data/sim_12hr.csv")
sim.12hr.mod <- sim.12hr[c("census_tract","sim_crime","actual_crime")]
colnames(sim.12hr.mod) <- c("name10","sim_crime_12h","actual_crime_12h") 
merged <- merge(x = sf_no_water_alt, y = sim.12hr.mod, by = "name10", all.x = TRUE)

sim.24r <- read.csv("/Users/arytwins/Desktop/data1030/crime/data/sim_24hr.csv")
sim.24hr.mod <- sim.24r[c("census_tract","sim_crime","actual_crime")]
colnames(sim.24hr.mod) <- c("name10","sim_crime_24h","actual_crime_24h") 
merged <- merge(x = merged, y = sim.24hr.mod, by = "name10", all.x = TRUE)

sim.48hr <- read.csv("/Users/arytwins/Desktop/data1030/crime/data/sim_48hr.csv")
sim.48hr.mod <- sim.48hr[c("census_tract","sim_crime","actual_crime")]
colnames(sim.48hr.mod) <- c("name10","sim_crime_48h","actual_crime_48h") 
merged <- merge(x = merged, y = sim.48hr.mod, by = "name10", all.x = TRUE)

sim.72hr <- read.csv("/Users/arytwins/Desktop/data1030/crime/data/sim_72hr.csv")
sim.72hr.mod <- sim.72hr[c("census_tract","sim_crime","actual_crime")]
colnames(sim.72hr.mod) <- c("name10","sim_crime_72h","actual_crime_72h") 
merged <- merge(x = merged, y = sim.72hr.mod, by = "name10", all.x = TRUE)

# all
plot(merged[c('sim_crime_12h','actual_crime_12h','sim_crime_24h','actual_crime_24h',
              'sim_crime_48h','actual_crime_48h','sim_crime_72h','actual_crime_72h')])

sim12 <- ggplot(merged) +
  geom_sf(aes(fill = sim_crime_12h)) + 
  scale_fill_gradient2(low = "lightcyan", high = "red", mid = "mediumvioletred" ,
                       midpoint = 4, limit = c(0,9), name="")+
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(3,"cm")) +
  labs(x = "Simulated Crimes") 

actual12 <- ggplot(merged) +
  geom_sf(aes(fill = actual_crime_12h)) + 
  scale_fill_gradient2(low = "lightcyan", high = "red", mid = "mediumvioletred" ,
                       midpoint = 4, limit = c(0,9), name="") +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(3,"cm")) +
  labs(x = "Actual Crimes") 


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(sim12)

grid.arrange(arrangeGrob(sim12 + theme(legend.position="none"), 
                         actual12 + theme(legend.position="none"),
                         nrow = 1,
                         top = textGrob("", vjust = -6, gp = gpar(fontface = "bold", cex = 1.5)),
                         left = textGrob("", rot = 90, vjust = 2.5),
                         bottom = textGrob("Number of crimes in the next 12 hours", vjust =-1)),
             mylegend,
             nrow=2, heights=c(1.2,.2))

# max(max(merged$sim_crime_12h),max(merged$actual_crime_12h))

sim24 <- ggplot(merged) +
  geom_sf(aes(fill = sim_crime_24h)) + 
  scale_fill_gradient2(low = "lightcyan", high = "red", mid = "mediumvioletred" ,
                       midpoint = 6, limit = c(0,13), name="")+
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(3,"cm")) +
  labs(x = "Simulated Crimes") 

actual24 <- ggplot(merged) +
  geom_sf(aes(fill = actual_crime_24h)) + 
  scale_fill_gradient2(low = "lightcyan", high = "red", mid = "mediumvioletred" ,
                       midpoint = 6, limit = c(0,13), name="") +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(3,"cm")) +
  labs(x = "Actual Crimes") 


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(sim24)

grid.arrange(arrangeGrob(sim24 + theme(legend.position="none"), 
                         actual24 + theme(legend.position="none"),
                         nrow = 1,
                         top = textGrob("", vjust = -6, gp = gpar(fontface = "bold", cex = 1.5)),
                         left = textGrob("", rot = 90, vjust = 2.5),
                         bottom = textGrob("Number of crimes in the next 24 hours", vjust =-1)),
             mylegend,
             nrow=2, heights=c(1.2,.2))


# max(max(merged$sim_crime_48h),max(merged$actual_crime_48h))

sim48 <- ggplot(merged) +
  geom_sf(aes(fill = sim_crime_48h)) + 
  scale_fill_gradient2(low = "lightcyan", high = "red", mid = "mediumvioletred" ,
                       midpoint = 8, limit = c(0,19), name="")+
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(3,"cm")) +
  labs(x = "Simulated Crimes") 

actual48 <- ggplot(merged) +
  geom_sf(aes(fill = actual_crime_48h)) + 
  scale_fill_gradient2(low = "lightcyan", high = "red", mid = "mediumvioletred" ,
                       midpoint = 8, limit = c(0,19), name="") +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(3,"cm")) +
  labs(x = "Actual Crimes") 


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(sim48)

grid.arrange(arrangeGrob(sim48 + theme(legend.position="none"), 
                         actual48 + theme(legend.position="none"),
                         nrow = 1,
                         top = textGrob("", vjust = -6, gp = gpar(fontface = "bold", cex = 1.5)),
                         left = textGrob("", rot = 90, vjust = 2.5),
                         bottom = textGrob("Number of crimes in the next 48 hours", vjust =-1)),
             mylegend,
             nrow=2, heights=c(1.2,.2))



# max(max(merged$sim_crime_72h),max(merged$actual_crime_72h))

sim72 <- ggplot(merged) +
  geom_sf(aes(fill = sim_crime_72h)) + 
  scale_fill_gradient2(low = "lightcyan", high = "red", mid = "mediumvioletred" ,
                       midpoint = 13, limit = c(0,31), name="")+
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(3,"cm")) +
  labs(x = "Simulated Crimes") 

actual72 <- ggplot(merged) +
  geom_sf(aes(fill = actual_crime_72h)) + 
  scale_fill_gradient2(low = "lightcyan", high = "red", mid = "mediumvioletred" ,
                       midpoint = 13, limit = c(0,31), name="") +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(3,"cm")) +
  labs(x = "Actual Crimes") 


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(sim72)

grid.arrange(arrangeGrob(sim72 + theme(legend.position="none"), 
                         actual72 + theme(legend.position="none"),
                         nrow = 1,
                         top = textGrob("", vjust = -6, gp = gpar(fontface = "bold", cex = 1.5)),
                         left = textGrob("", rot = 90, vjust = 2.5),
                         bottom = textGrob("Number of crimes in the next 72 hours", vjust =-1)),
             mylegend,
             nrow=2, heights=c(1.2,.2))

# simulations

merged_crimes<-merged[!(merged$sim_crime_12h==0),]
merged_sf <- as(merged_crimes, 'Spatial')    
dev.off()
plot(merged["sim_crime_12h"], col = 'lightblue', border = 'grey')
for (i in 1:length(merged_sf@data$sim_crime_12h)){
  points_sim <- spsample(merged_sf@polygons[[i]], n = merged_sf@data$sim_crime_12h[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'red', add = TRUE)
}

merged_crimes<-merged[!(merged$sim_crime_24h==0),]
merged_sf <- as(merged_crimes, 'Spatial')    
dev.off()
plot(merged["sim_crime_24h"], col = 'lightblue', border = 'grey')
for (i in 1:length(merged_sf@data$sim_crime_24h)){
  points_sim <- spsample(merged_sf@polygons[[i]], n = merged_sf@data$sim_crime_24h[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'red', add = TRUE)
}

merged_crimes<-merged[!(merged$sim_crime_48h==0),]
merged_sf <- as(merged_crimes, 'Spatial')    
dev.off()
plot(merged["sim_crime_48h"], col = 'lightblue', border = 'grey')
for (i in 1:length(merged_sf@data$sim_crime_48h)){
  points_sim <- spsample(merged_sf@polygons[[i]], n = merged_sf@data$sim_crime_48h[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'red', add = TRUE)
}

merged_crimes<-merged[!(merged$sim_crime_72h==0),]
merged_sf <- as(merged_crimes, 'Spatial')    
dev.off()
plot(merged["sim_crime_72h"], col = 'lightblue', border = 'grey')
for (i in 1:length(merged_sf@data$sim_crime_72h)){
  points_sim <- spsample(merged_sf@polygons[[i]], n = merged_sf@data$sim_crime_72h[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'red', add = TRUE)
}


# simulations

merged$Simulations <- merged$sim_crime_12h

merged_crimes.all <- merged[!(merged$sim_crime_12h==0),]
merged_sf <- as(merged_crimes.all, 'Spatial')  

merged_crimes.all2 <- merged[!(merged$sim_crime_24h-merged$sim_crime_12h<=0),]
merged_crimes.all2$xtra24 <- merged_crimes.all2$sim_crime_24h-merged_crimes.all2$sim_crime_12h
merged_sf1 <- as(merged_crimes.all2, 'Spatial')    

merged_crimes.all3 <- merged[!(merged$sim_crime_48h-merged$sim_crime_24h<=0),]
merged_crimes.all3$xtra48 <- merged_crimes.all3$sim_crime_48h-merged_crimes.all3$sim_crime_24h
merged_sf2 <- as(merged_crimes.all3, 'Spatial')  

merged_crimes.all4 <- merged[!(merged$sim_crime_72h-merged$sim_crime_48h<=0),]
merged_crimes.all4$xtra72 <- merged_crimes.all4$sim_crime_72h-merged_crimes.all4$sim_crime_48h
merged_sf4 <- as(merged_crimes.all4, 'Spatial') 


dev.off()
plot(merged["Simulations"], col = 'lightblue', border = 'grey')

for (i in 1:length(merged_sf@data$sim_crime_12h)){
  points_sim <- spsample(merged_sf@polygons[[i]], n = merged_sf@data$sim_crime_12h[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'yellow', add = TRUE)
}
for (i in 1:length(merged_sf1@data$xtra24)){
  points_sim <- spsample(merged_sf1@polygons[[i]], n = merged_sf1@data$xtra24[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'chocolate2', add = TRUE)
}
for (i in 1:length(merged_sf2@data$xtra48)){
  points_sim <- spsample(merged_sf2@polygons[[i]], n = merged_sf2@data$xtra48[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'violetred2', add = TRUE)
}
for (i in 1:length(merged_sf4@data$xtra72)){
  points_sim <- spsample(merged_sf4@polygons[[i]], n = merged_sf4@data$xtra72[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'darkorchid4', add = TRUE)
}


############### PERFORMANCE

performance <- read.csv("/Users/arytwins/Desktop/data1030/crime/data/performance_by_tract.csv")
colnames(performance) <- c("name10","pred_crime","actual_crime","correct_pred","TP","FP","TN","FN","census_tract","det_rate","false_alarm_rate")
merged_perf <- merge(x = sf_no_water_alt, y = performance, by = "name10", all.x = TRUE)


# graphical confusion matrix
plot(merged_perf[c('TP','FN', 'FP', 'TN')])

# Detection rate and false alarm rate
DR <- ggplot(merged_perf) +
  geom_sf(aes(fill = det_rate)) + 
  scale_fill_gradient2(low = "lightcyan", high = "red", mid = "mediumvioletred" ,
                       midpoint = 0.5, limit = c(0,1), name="") +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(1.5,"cm")) +
  labs(x = "Detection Rate") 

FAR <- ggplot(merged_perf) +
  geom_sf(aes(fill = false_alarm_rate)) + 
  scale_fill_gradient2(low = "lightcyan", high = "limegreen", mid = "olivedrab1" ,
                       midpoint = 0.5, limit = c(0,1), name="") +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(1.5,"cm")) +
  labs(x = "False Alarm Rate") 

grid.arrange(DR, FAR, ncol=2)

merged_perf$corr_class <-  (merged_perf$TP+merged_perf$TN)/(merged_perf$TP+merged_perf$TN+ merged_perf$FP+merged_perf$FN)

ggplot(merged_perf) +
  geom_sf(aes(fill = corr_class)) + 
  scale_fill_gradient2(low = "red", high = "limegreen", mid = "olivedrab1" ,
                       midpoint = 0.4, limit = c(0,1), name="") +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(1.5,"cm")) +
  labs(x = "Correct Classification Rate	(TN+TP)/n") 

############# ACTUAL

# simulations

merged_crimes<-merged[!(merged$actual_crime_12h==0),]
merged_sf <- as(merged_crimes, 'Spatial')    
dev.off()
plot(merged["actual_crime_12h"], col = 'lightblue', border = 'grey')
for (i in 1:length(merged_sf@data$actual_crime_12h)){
  points_sim <- spsample(merged_sf@polygons[[i]], n = merged_sf@data$actual_crime_12h[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'blue', add = TRUE)
}

merged_crimes<-merged[!(merged$sim_crime_12h==0),]
merged_sf <- as(merged_crimes, 'Spatial')    
for (i in 1:length(merged_sf@data$sim_crime_12h)){
  points_sim <- spsample(merged_sf@polygons[[i]], n = merged_sf@data$sim_crime_12h[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'red', add = TRUE)
}


merged_crimes<-merged[!(merged$actual_crime_24h==0),]
merged_sf <- as(merged_crimes, 'Spatial')    
dev.off()
plot(merged["actual_crime_24h"], col = 'lightblue', border = 'grey')
for (i in 1:length(merged_sf@data$actual_crime_24h)){
  points_sim <- spsample(merged_sf@polygons[[i]], n = merged_sf@data$actual_crime_24h[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'blue', add = TRUE)
}

merged_crimes<-merged[!(merged$sim_crime_24h==0),]
merged_sf <- as(merged_crimes, 'Spatial')    
for (i in 1:length(merged_sf@data$sim_crime_24h)){
  points_sim <- spsample(merged_sf@polygons[[i]], n = merged_sf@data$sim_crime_24h[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'red', add = TRUE)
}


merged_crimes<-merged[!(merged$actual_crime_48h==0),]
merged_sf <- as(merged_crimes, 'Spatial')    
dev.off()
plot(merged["actual_crime_48h"], col = 'lightblue', border = 'grey')
for (i in 1:length(merged_sf@data$actual_crime_48h)){
  points_sim <- spsample(merged_sf@polygons[[i]], n = merged_sf@data$actual_crime_48h[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'blue', add = TRUE)
}

merged_crimes<-merged[!(merged$sim_crime_48h==0),]
merged_sf <- as(merged_crimes, 'Spatial')    
for (i in 1:length(merged_sf@data$sim_crime_48h)){
  points_sim <- spsample(merged_sf@polygons[[i]], n = merged_sf@data$sim_crime_48h[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'red', add = TRUE)
}


merged_crimes<-merged[!(merged$actual_crime_72h==0),]
merged_sf <- as(merged_crimes, 'Spatial')    
dev.off()
plot(merged["actual_crime_72h"], col = 'lightblue', border = 'grey')
for (i in 1:length(merged_sf@data$sim_crime_72h)){
  points_sim <- spsample(merged_sf@polygons[[i]], n = merged_sf@data$actual_crime_72h[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'blue', add = TRUE)
}
merged_crimes<-merged[!(merged$sim_crime_72h==0),]
merged_sf <- as(merged_crimes, 'Spatial')    
for (i in 1:length(merged_sf@data$sim_crime_72h)){
  points_sim <- spsample(merged_sf@polygons[[i]], n = merged_sf@data$sim_crime_72h[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'red', add = TRUE)
}


# simulations

merged$Simulations <- merged$sim_crime_12h

merged_crimes.all <- merged[!(merged$sim_crime_12h==0),]
merged_sf <- as(merged_crimes.all, 'Spatial')  

merged_crimes.all2 <- merged[!(merged$sim_crime_24h-merged$sim_crime_12h<=0),]
merged_crimes.all2$xtra24 <- merged_crimes.all2$sim_crime_24h-merged_crimes.all2$sim_crime_12h
merged_sf1 <- as(merged_crimes.all2, 'Spatial')    

merged_crimes.all3 <- merged[!(merged$sim_crime_48h-merged$sim_crime_24h<=0),]
merged_crimes.all3$xtra48 <- merged_crimes.all3$sim_crime_48h-merged_crimes.all3$sim_crime_24h
merged_sf2 <- as(merged_crimes.all3, 'Spatial')  

merged_crimes.all4 <- merged[!(merged$sim_crime_72h-merged$sim_crime_48h<=0),]
merged_crimes.all4$xtra72 <- merged_crimes.all4$sim_crime_72h-merged_crimes.all4$sim_crime_48h
merged_sf4 <- as(merged_crimes.all4, 'Spatial') 


dev.off()
plot(merged["Simulations"], col = 'lightblue', border = 'grey')

for (i in 1:length(merged_sf@data$sim_crime_12h)){
  points_sim <- spsample(merged_sf@polygons[[i]], n = merged_sf@data$sim_crime_12h[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'yellow', add = TRUE)
}
for (i in 1:length(merged_sf1@data$xtra24)){
  points_sim <- spsample(merged_sf1@polygons[[i]], n = merged_sf1@data$xtra24[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'chocolate2', add = TRUE)
}
for (i in 1:length(merged_sf2@data$xtra48)){
  points_sim <- spsample(merged_sf2@polygons[[i]], n = merged_sf2@data$xtra48[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'violetred2', add = TRUE)
}
for (i in 1:length(merged_sf4@data$xtra72)){
  points_sim <- spsample(merged_sf4@polygons[[i]], n = merged_sf4@data$xtra72[i] + 1, "nonaligned")
  plot(points_sim, pch = 3, col = 'darkorchid4', add = TRUE)
}


###################### PROBS
probs_1h <- read.csv("/Users/arytwins/Desktop/data1030/crime/data/predicted_probs_1_hr.csv")
probs_1h.mod <- probs_1h[c("census_tract","predicted_prob_crime_1_hr")]
colnames(probs_1h.mod) <- c("name10","pred_probs") 
merged_probs1 <- merge(x = sf_no_water_alt, y = probs_1h.mod, by = "name10", all.x = TRUE)

ggplot(merged_probs1) +
  geom_sf(aes(fill = pred_probs)) + 
  scale_fill_gradient2(low = "lightcyan", high = "red", mid = "indianred1" ,
                       midpoint = 0.25, limit = c(0,0.5), name="") +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position = "bottom",
        legend.key.width=unit(3,"cm")) +
  labs(x = "Predicted probability of crime") 
