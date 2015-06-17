####################################################################################
##                  URBDP 561 URBAN ECONOMICS AND PUBLIC POLICY                 ####
##                  NAME- DEBMALYA SINHA UW STUDENT NO- 1325990                 ####
####################################################################################

#This data was taken from Mr. jay feng's Github repository by his permission
#The distances ffrom I5 was exracted from Google Distance matrix by the help of Mr.Debosmit Ray
# CALLING PACKAGES( tHIS IS MY DEFAULT CALL, SOMETIMES THEY ALL ARE NOT REQUIRED)

library(ggplot2)
library(plyr)
library(reshape2)
library(lubridate)
library(modeest)
library(ggmap)
library(RColorBrewer)
library(classInt)
library(maps)
library(ggthemes)
library(gridExtra)

# calling files
craigslist <- read.csv("Craigs/craigSeattleFinal.csv", header=TRUE)
colnames(craigslist)
summary(craigslist)

# DATA CLEANUPS-

craig<- subset(craigslist, craigslist$i.5ExitDistance != "NA's")
craig$lat<- substr(craig$coord, 1,9)
craig$long<- substr(craig$coord, 12,22)
craig <- craig[, c(1,4,7,9,13,18:20)]
craig$lat<- as.numeric(as.character(craig$lat))
craig$long<- as.numeric(as.character(craig$long))
craig$sqftprice<- craig$price/craig$size
summary(craig)

# PLOTTING THE APARTMETS AVAILABLE O THE MAP#
# CALLING MAP
Seattle_map <- get_map(location= c(lon = -122.3320708, lat = 47.6062095), 
                       zoom=11, maptype= "roadmap", color= "bw")

# pLOTTING
Craigs_location<- 
  ggmap(Seattle_map)+
  geom_point(data = craig,  # Got the points
             mapping= aes(x = long, y = lat, col= sqftprice), size=2) +
  ggtitle("Location of Vacant Units")+
  theme_solarized(light= FALSE) +
  scale_color_continuous(high= "darkorange", low= "blue")+
  theme(plot.title = element_text(size=25, face="bold"))
Craigs_location

# LLINEAR REGRESSION MODEL , TO CHECK WHEATHER ANY RELATIONSHIP EXISTS 
#BETWEEN APARTMENT PRICE PER SQ.FT AND DISTANCE FROM I-5

i5_price <- lm (sqftprice ~ i.5ExitDistance,
                       data= craig)
i5_price
summary(i5_price)

# The adjusted r-square value in the following plot comes from the summary

dist_sqftprice <- qplot( i.5ExitDistance,sqftprice, data=craig, col= "yellow" ,xlim=c(0,7))+
  geom_smooth(method='lm', lwd=1, col= "darkorange")+
  annotate("text", label = "R-squared: 0.086", x = 3.5, y = 5, size = 6, colour = "lightblue3")+
  ggtitle("Relation between Price and I-5 Distance")+
  theme_solarized(light=FALSE)+
  scale_colour_solarized("red")
dist_sqftprice

# MULTIPLE REGRESSION 1, TO CHECK THE INITIAL REGRESSION MODEL WITH 3 INDEPENDANT VARIABLE,
# WITHOUT THE DISTANCE
# THEN PREDICTING VALUES FOR ALL

model_1 <- lm(price ~ beds+
                baths+ 
                size , data= craig)
model_1
summary(model_1)
craig$model1_prediction <- predict(model_1)

#MULTIPLE REGRESSION 2, TO CHECK THE INITIAL REGRESSION MODEL WITH 3 INDEPENDANT VARIABLE,
# AND THE DISTANCE
# THEN PREDICTING VALUES FOR ALL

model_2 <- lm(price ~ beds+
                baths+ 
                size+
                i.5ExitDistance, data= craig)
model_2
summary(model_2)
craig$model2_prediction <- predict(model_2)

# TO CHECK ERROR MARGINS FOR BOTH REGRRESSIONS

craig$model1_error <- ((craig$price - craig$model1_prediction)*100)/craig$price
craig$model2_error <- ((craig$price - craig$model2_prediction)*100)/craig$price
mod_1<- summary(others$model1_error)
mod_2<- summary(others$model2_error)
table<- rbind(mod_1, mod_2)
write.csv(table, "C:/Course readings etc/Transportation & Environment/table.csv")
craig$prediction <- (craig$model2_error/craig$model1_error)
summary(craig$prediction)
summary(craig$price)


# taking out outliers, AS THEY DON'T SPEAK ANY STORY HERE.
outliers <- subset(craig, craig$model1_error <= -50)
others <- subset(craig, craig$model1_error >= -50)

# plot with model 1
error_1_better<- 
  ggmap(Seattle_map)+
  geom_point(data = others, 
             mapping= aes(x = long, y = lat, col= model1_error)) +
  ggtitle("Percentage of Deviation and Location- Model 1")+
  theme_solarized(light= FALSE) +
  scale_color_gradient( low= "yellow", high= "firebrick")+
  theme(plot.title = element_text(size=15, face="bold"))
error_1_better


# plot with model 2
error_2_better<- 
  ggmap(Seattle_map)+
  geom_point(data = others,  # Got the points
             mapping= aes(x = long, y = lat, col= model2_error)) +
  ggtitle("Percentage of Deviation and Location- Model 2")+
  theme_solarized(light= FALSE) +
  scale_color_gradient( low= "yellow", high= "firebrick")+
  theme(plot.title = element_text(size=15, face="bold"))
error_2_better
grid.arrange(error_1_better,error_2_better, ncol=2 )

others$prediction <- (others$model2_error/others$model1_error)
summary(others$prediction)

error_a<- 
  ggmap(Seattle_map)+
  geom_point(data = others,  # Got the points
             mapping= aes(x = long, y = lat, col= prediction)) +
  ggtitle("comparing two models")+
  theme_solarized(light= FALSE) +
  scale_color_gradient( low= "red", high= "white")+
  theme(plot.title = element_text(size=15, face="bold"))
error_a

errorbar_2<- ggplot(others,aes(model2_error)) +
  geom_histogram(binwidth=5, col= "cyan2", fill= "cyan4")+
  ggtitle("Deviation from Actual price- Model 2")+
  theme_solarized(light= FALSE) +
  scale_fill_solarized("red")+
  theme(plot.title = element_text(size=15, face="bold"))
errorbar_2

errorbar_1<- ggplot(others,aes(model1_error)) +
  geom_histogram(binwidth=5, col= "cyan2", fill= "cyan4")+
  stat_function( fun = dnorm, 
    args = with(others, c(mean = 0, sd = sd(model1_error))),
    color="red", lwd=1.5)+
  ggtitle("Deviation from Actual price- Model 1")+
  theme_solarized(light= FALSE) +
  scale_fill_solarized("red")+
  theme(plot.title = element_text(size=15, face="bold"))
errorbar_1

a <- qplot(model1_prediction, model2_prediction, data=craigs,col= DOT, xlim=c(0,60), ylim=c(0,60))+
  geom_smooth(method='lm', lwd=1, col= "darkorange")+
  annotate("text", label = "R-squared: 0.541", x = 30, y = 60, size = 6, colour = "lightblue3")+
  ggtitle("Winter time- Duwamish data")+
  theme_solarized(light=FALSE)+
  scale_colour_solarized("red")


# PLOTS WITH PREDICTIONS

predict1_actual <- qplot( price,model1_prediction, data=craig, col= "yellow", xlim=c(0,6000), ylim=c(0,6000))+
  geom_abline(intercept = 0, slope = 1, lwd=1, col= "darkorange")+
  annotate("text", label = "R-squared: 0.45", x = 3000, y = 5800, size = 6, colour = "lightblue3")+
  ggtitle("Relation between Actual and predicted- Model 1")+
  theme_solarized(light=FALSE)+
  scale_colour_solarized("red")+
  theme(plot.title = element_text(size=15, face="bold"))
predict1_actual

predict2_actual <- qplot( price,model2_prediction, data=craig, col= "yellow", xlim=c(0,6000), ylim=c(0,6000))+
  geom_abline(intercept = 0, slope = 1, lwd=1, col= "darkorange")+
  annotate("text", label = "R-squared: 0.48", x = 3000, y = 5800, size = 6, colour = "lightblue3")+
  ggtitle("Relation between Actual and predicted- Model 2")+
  theme_solarized(light=FALSE)+
  scale_colour_solarized("red")+
  theme(plot.title = element_text(size=15, face="bold"))
predict2_actual
median(craig$model2_error)
median(craig$model1_error)
errorbar_2<- ggplot(others,aes(model2_error)) +
  geom_histogram(binwidth=1, col= "cyan4", fill= "cyan4")+
  ggtitle("Percentage Deviation from Actual price- Model 2")+
  theme_solarized(light= FALSE) +
  scale_fill_solarized("red")+
  theme(plot.title = element_text(size=15, face="bold"))
errorbar_2
errorbar_1<- ggplot(others,aes(model1_error)) +
  geom_histogram(binwidth=1, col= "cyan4", fill= "cyan4")+
  ggtitle("Percentage Deviation from Actual price- Model 1")+
  theme_solarized(light= FALSE) +
  scale_fill_solarized("red")+
  theme(plot.title = element_text(size=15, face="bold"))
errorbar_1

grid.arrange(errorbar_1, errorbar_2,predict1_actual,predict2_actual, ncol=2 )
predict_compare <- qplot( model1_prediction,model2_prediction, data=craig, 
                          col= "yellow", xlim=c(1000,5000), ylim=c(1000,5000))+
  geom_abline(intercept = 0, slope = 1, lwd=1, col= "darkorange")+
  annotate("text", label = "R-squared: 0.48", x = 2000, y = 4800, size = 6, colour = "lightblue3")+
  ggtitle("Relation between Model1 and Model 2")+
  theme_solarized(light=FALSE)+
  scale_colour_solarized("red")+
  theme(plot.title = element_text(size=15, face="bold"))
predict_compare