setwd("H:/Documents/G54PRO/fmri-behaviour")

#-------------------- package needed -------------------
library(ggplot2)
library(magrittr);library(ggpubr)
library(gridExtra)

#-------------------- Load all data into a dataframe -------------------
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

AllPosition <- NULL

for (i in 1:length(temp)) {
  per <- cbind (read.csv(temp[i]), type = substr(temp[i], 1 ,1) , subject = substr(temp[i], 2 ,3) , vision.type = substr(temp[i], 4 ,nchar(temp[i]) - 4))
  AllPosition <- rbind(AllPosition,per)
}
# colums 
names(AllPosition)
# unique test position
unique(AllPosition[,"targetX.512"])[order(unique(AllPosition[,"targetX.512"]))]
unique(AllPosition[,"targetY.384"])[order(unique(AllPosition[,"targetY.384"]))]

#-------------------- Transform data (add columns) -------------------
# + positive difference equal to shift towards right of the supposed 
AllPosition$x.512.diff <- AllPosition$responseX.512-(-AllPosition$targetX.512)
AllPosition$y.384.diff <- AllPosition$responseY.384-(-AllPosition$targetY.384)

AllPosition$radius <- (AllPosition$x.512.diff^2 + AllPosition$y.384.diff^2)^0.5

Outlinerplot <- function (target, aorc, eyetype = NULL) {
  ggplot(subset(AllPosition, targetX.512==target[[1]] & targetY.384==target[[2]] & type == aorc), aes(x="Distant",y=radius)) + 
    geom_boxplot()
}
Outlinerplot(TargetCombo[1,], aorc = "C")

#AllPosition$x.diff <- (1280/2-AllPosition$responseX)-(AllPosition$targetX-1280/2)
#AllPosition$y.diff <- AllPosition$responseY-AllPosition$targetY


AllPosition$ecc.x.error <- AllPosition$x.512.diff/(unique(pos)[1]-unique(pos)[2])/2
AllPosition$ecc.y.error <- AllPosition$y.384.diff/(unique(pos)[1]-unique(pos)[2])/2

#AllPosition$remove[AllPosition$targetX.512 == TargetCombo[i,1] & AllPosition$targetY.384 == TargetCombo[i,2] & AllPosition$type == "C"] <- 0
AllPosition$remove <- NA

# Only adding remove column for Control subject (grouping the control to find outlier)
for (i in 1:nrow(TargetCombo)) {
  dis = subset(AllPosition, targetX.512== TargetCombo[i,1] & targetY.384 == TargetCombo[i,2] & type == "C")$radius
  AllPosition$remove[which(AllPosition$targetX.512 == TargetCombo[i,1] & AllPosition$targetY.384 == TargetCombo[i,2] & AllPosition$type == "C")[c(dis < quantile(dis, 0.25) - 1.5* IQR(dis) | dis > quantile(dis, 0.75) + 1.5* IQR(dis))]] <- 1
}
# There is 60 outlier
table(AllPosition$remove)
outlier = AllPosition[which(AllPosition$remove == 1),]

OutlierPointPlot <- function(target){
  ggplot(AllPosition[which(AllPosition$remove == 1 & AllPosition$targetX.512 ==target[[1]] & AllPosition$targetY.384 == target[[2]]),]) + 
    geom_point(aes(x=responseX.512, y=responseY.384, color=subject)) +
    geom_point(aes(x=targetX.512, y=targetY.384)) +
    geom_point(aes(x=-targetX.512, y=-targetY.384)) +
    geom_point(aes(x=0, y=0),shape=3)
}
OutlierPointPlot(TargetCombo[15,])


#  AllPosition$eccentricity == AllPosition$radius / euclidean dis

#AllPosition$polar.angle.360 <- acos(AllPosition$x.512.diff/AllPosition$radius)/pi*180
# the top have top left and right will come up same degree as bottom left and bottom right
AllPosition$polar.angle.360 <- 0
AllPosition$polar.angle.360[which(AllPosition$y.384.diff > 0)] <- acos(AllPosition[which(AllPosition$y.384.diff > 0),"x.512.diff"]/AllPosition[which(AllPosition$y.384.diff > 0),"radius"])/pi*180 
AllPosition$polar.angle.360[which(AllPosition$y.384.diff < 0)] <- 360 - acos(AllPosition[which(AllPosition$y.384.diff < 0),"x.512.diff"]/AllPosition[which(AllPosition$y.384.diff < 0),"radius"])/pi*180 

# AllPosition[AllPosition$ecc.y.error == 0 & AllPosition$ecc.x.error != 0 ,]$x.512.diff/AllPosition[AllPosition$ecc.y.error == 0 & AllPosition$ecc.x.error != 0 ,]$radius
#cos(0) = 1 == right side 
atan(AllPosition$targetY.384/AllPosition$targetX.512)
atan(TargetCombo$targetY.384/TargetCombo$targetX.512)
atan(TargetCombo$targetY.384/TargetCombo$targetX.512)/pi*180



#DF with outelier remove (Currently only all the control subjects)
AllPositionOutlierRemove <- AllPosition[which(is.na(AllPosition$remove)),]
#  ----- All 1 degree 4

one = AllPositionOutlierRemove[AllPositionOutlierRemove$targetX.512 %in% c(59,24,-24,-59),]
#  ----- All 3 degree
three = AllPositionOutlierRemove[AllPositionOutlierRemove$targetX.512 %in% c(177,73,-73,-177),]
#  ----- All 5 degree
five = AllPositionOutlierRemove[AllPositionOutlierRemove$targetX.512 %in% c(295,122,-122,-295),]
#  ----- All 7 degree
seven = AllPositionOutlierRemove[AllPositionOutlierRemove$targetX.512 %in% c(413,171,-171,-413),]


#-------------------- All controls response plot -------------------
#outlier removed dataset 
ggplot(subset(AllPositionOutlierRemove, type == "C")) + geom_point(aes(x=targetX.512, y=targetY.384)) + geom_point(aes(x=responseX.512, y=responseY.384, color=subject), shape=4) 

#orginal data set
ggplot(subset(AllPosition, type == "C")) + geom_point(aes(x=targetX, y=targetY)) + geom_point(aes(x=responseX, y=responseY, color=subject), shape=4) 

#-------------------- All Unique position -------------------
Y.Target.Position = NULL
for (i in seq(1,nrow(TargetCombo),2)) {
  Y.Target.Position <- c(Y.Target.Position,unique(AllPosition[AllPosition[,"targetX.512"]==TargetCombo[i,],"targetY.384"]))
}  TargetCombo <- data.frame(x = rep(unique(AllPosition[,"targetX.512"])[order(unique(AllPosition[,"targetX.512"]))],2)[order(rep(unique(AllPosition[,"targetX.512"])[order(unique(AllPosition[,"targetX.512"]))],2))] , y = Y.Target.Position)

# better method
TargetCombo <- unique(AllPosition[,c(3,4)])[order(unique(AllPosition[,c(3,4)])[1]),]
row.names(TargetCombo) <- 1:32

ggplot(TargetCombo, aes(x=targetX.512, y=targetY.384)) + geom_point() + labs(x = "x", y = "y")


#---------------- midline corrispondeing position ------------------------
pos <- c()
for (i in 1:nrow(TargetCombo)){
  pos <- c(pos, (TargetCombo[i,1]^2 + TargetCombo[i,2]^2)^0.5)
}
unique(pos)
# each degree of eccentracity 
(unique(pos)[1]-unique(pos)[2])/2
#----------------------------------------
#Type of vision data 
unique(AllPosition[,c("vision.type")])

# Trim dataframe with need info
AllPosition[AllPosition[,c(3)] == TargetCombo[1,1],c(-1,-2,-5,-6,-9,-10)]

#------------------- Plot of repsonse point (-413,171) ---------------------
ggplot(subset(AllPosition, targetX.512==-413 & targetY.384==171 & type=="C")) +
  geom_point(aes(x=413, y=-171)) +
  geom_point(aes(x=responseX.512, y=responseY.384, color=subject), shape=4) +
  labs(x = "x", y ="y")

#---------------------Function to plot one response point -------------------
TargetPoint <- function (target, aorc, eyetype = NULL) {
  ggplot(subset(AllPosition, targetX.512==target[[1]] & targetY.384==target[[2]] & type == aorc)) + 
    geom_point(aes(x=-target[[1]], y=-target[[2]])) + 
    geom_point(aes(x=responseX.512, y=responseY.384, color=subject), shape=4) +
    labs(x = "x", y ="y")
}

TargetPoint(TargetCombo[9,], aorc = "C")
#OUtlinerplot(TargetCombo[9,], aorc = "C")
#OutlierPointPlot(TargetCombo[9,])

#---------------------Loop though each person in a response point for centroid -------------------





#-------------------- Fitting best fit line --------------------
location <- function(x,y, aorc) {
  ggscatter(subset(AllPosition, targetX.512==x & targetY.384 == y & type == aorc), x = "responseX.512", y = "responseY.384", col = "subject" ,  size = 2, add = "reg.line", conf.int = F, cor.coef = TRUE, cor.coeff.args = list(method = "pearson", label.x.npc = 0.85, label.y.npc = 0.55))   + 
    geom_smooth(method="lm", se=FALSE, aes(color= "Overall")) }

location(24,59, "C")


s = subset(AllPosition, targetX.512==24 & targetY.384 == 59 & type == "C")


# midpoint of the x and y range
x = min(s[,"responseX.512"]) + (max(s[,"responseX.512"]) - min(s[,"responseX.512"]))/2
y = min(s[,"responseY.384"]) + (max(s[,"responseY.384"]) - min(s[,"responseY.384"]))/2

# r (regression/correlation) = 
cor(s$responseX.512,s$responseY.384)

model = lm(responseY.384 ~ responseX.512 , data=s)
#responseY.384=intercept+b×responseX.512

model = lm(responseX.512 ~ responseY.384, data=s)

#midpoint of the liner regression
model$coefficients[[1]]+round(min(s[,"responseX.512"]) + (max(s[,"responseX.512"]) - min(s[,"responseX.512"]))/2)*model$coefficients[[2]]

#---------- error of the controls all points ----------------
#---------- Control left and right
# targetX.512 > 0 is the response(left)
mean(one[one$targetX.512 > 0 & one$type == "C" ,"ecc.x.error"]) # the left response are - which is shift towards the left
mean(one[one$targetX.512 < 0 & one$type == "C" ,"ecc.x.error"]) # the right response are - which is shift towards the right 
# the is a general expension
error <- c(mean(one[one$targetX.512 > 0 & one$type == "C" ,"ecc.x.error"]),mean(one[one$targetX.512 < 0 & one$type == "C" ,"ecc.x.error"]))

mean(three[three$targetX.512 > 0 & three$type == "C","ecc.x.error"]) # the left response are - which is shift towards the right
mean(three[three$targetX.512 < 0 & three$type == "C","ecc.x.error"]) # the right response are - which is shift towards the right 
error <- rbind(error,c(mean(three[three$targetX.512 > 0 & three$type == "C","ecc.x.error"]),mean(three[three$targetX.512 < 0 & three$type == "C","ecc.x.error"])))

mean(five[five$targetX.512 > 0 & five$type == "C","ecc.x.error"]) 
mean(five[five$targetX.512 < 0 & five$type == "C" ,"ecc.x.error"])
error <- rbind(error,c(mean(five[five$targetX.512 > 0 & five$type == "C","ecc.x.error"]),mean(five[five$targetX.512 < 0 & five$type == "C","ecc.x.error"])))

mean(seven[seven$targetX.512 > 0 & seven$type == "C","ecc.x.error"]) 
mean(seven[seven$targetX.512 < 0 & seven$type == "C","ecc.x.error"])
error <- rbind(error,c(mean(seven[seven$targetX.512 > 0 & seven$type == "C","ecc.x.error"]),mean(seven[seven$targetX.512 < 0 & seven$type == "C","ecc.x.error"])))

colnames(error) <- c("x.error.left","x.error.right")
rownames(error) <- c(paste(seq(1,7,2),"degree"))

#---------- Control vertical / up and down / left and right
error <- cbind(error, vertical.left = c.(
  mean(one[one$targetX.512 > 0 & one$type == "C" ,"ecc.y.error"]),
  mean(three[three$targetX.512 > 0 & three$type == "C" ,"ecc.y.error"]),
  mean(five[five$targetX.512 > 0 & five$type == "C" ,"ecc.y.error"]),
  mean(seven[seven$targetX.512 > 0 & seven$type == "C" ,"ecc.y.error"])))  

error <- cbind(error, vertical.right. = c(
  mean(one[one$targetX.512 < 0 & one$type == "C" ,"ecc.y.error"]),
  mean(three[three$targetX.512 < 0 & three$type == "C" ,"ecc.y.error"]),
  mean(five[five$targetX.512 < 0 & five$type == "C" ,"ecc.y.error"]),
  mean(seven[seven$targetX.512 < 0 & seven$type == "C" ,"ecc.y.error"])))  

error <- cbind(error, vertical.upper = c(
  mean(one[one$targetY.384 < 0 & one$type == "C" ,"ecc.y.error"]),
  mean(three[three$targetY.384 < 0 & three$type == "C" ,"ecc.y.error"]),
  mean(five[five$targetY.384 < 0 & five$type == "C" ,"ecc.y.error"]),
  mean(seven[seven$targetY.384 < 0 & seven$type == "C" ,"ecc.y.error"])))

error <- cbind(error, vertical.lower = c(
  mean(one[one$targetY.384 > 0 & one$type == "C" ,"ecc.y.error"]),
  mean(three[three$targetY.384 > 0 & three$type == "C" ,"ecc.y.error"]),
  mean(five[five$targetY.384 > 0 & five$type == "C" ,"ecc.y.error"]),
  mean(seven[seven$targetY.384 > 0 & seven$type == "C" ,"ecc.y.error"])))

#--------------------- Polar Angle --------------
one$polar.angle.360

mean(one[one$targetX.512 > 0 & one$type == "C" ,"polar.angle.360"],na.rm=T)
mean(one[one$targetX.512 < 0 & one$type == "C" ,"polar.angle.360"],na.rm=T)

mean(three[three$targetX.512 > 0 & three$type == "C" ,"polar.angle.360"],na.rm=T)
mean(three[three$targetX.512 < 0 & three$type == "C" ,"polar.angle.360"],na.rm=T)

mean(five[five$targetX.512 > 0 & five$type == "C" ,"polar.angle.360"],na.rm=T)
mean(five[five$targetX.512 < 0 & five$type == "C" ,"polar.angle.360"],na.rm=T)
#----------------- 


#centrality by average
avgcentra <- aggregate(AllPositionOutlierRemove, by = list(AllPositionOutlierRemove$targetX.512, AllPositionOutlierRemove$targetY.384, subject = AllPositionOutlierRemove$subject, vision.type = AllPositionOutlierRemove$vision.type, type = AllPositionOutlierRemove$type), mean)[,c(3:5,8,9,12,13,21)]

ggplot(subset(aggregate(AllPositionOutlierRemove, by = list(AllPositionOutlierRemove$targetX.512, AllPositionOutlierRemove$targetY.384, subject = AllPositionOutlierRemove$subject, AllPositionOutlierRemove$vision.type, type = AllPositionOutlierRemove$type), mean)[,c(3,5,8,9,12,13)], type == "C")) + geom_point(aes(x=targetX.512, y=targetY.384)) + geom_point(aes(x=responseX.512, y=responseY.384, color=subject), shape=4) + 

ggplot(subset(aggregate(AllPositionOutlierRemove, by = list(AllPositionOutlierRemove$targetX.512, AllPositionOutlierRemove$targetY.384, subject = AllPositionOutlierRemove$subject, vision.type = AllPositionOutlierRemove$vision.type, type = AllPositionOutlierRemove$type), mean)[,c(3:5,8,9,12,13)], type == "C" & vision.type == "MonGood")) + geom_point(aes(x=targetX.512, y=targetY.384)) + geom_point(aes(x=responseX.512, y=responseY.384, color=subject), shape=4) 
ggplot(subset(aggregate(AllPositionOutlierRemove, by = list(AllPositionOutlierRemove$targetX.512, AllPositionOutlierRemove$targetY.384, subject = AllPositionOutlierRemove$subject, vision.type = AllPositionOutlierRemove$vision.type, type = AllPositionOutlierRemove$type), mean)[,c(3:5,8,9,12,13)], type == "C" & vision.type == "MonBad")) + geom_point(aes(x=targetX.512, y=targetY.384)) + geom_point(aes(x=responseX.512, y=responseY.384, color=subject), shape=4) 


(AllPositionOutlierRemove$responseX.512^2 + AllPositionOutlierRemove$responseY.384^2)^0.5


avgcentra[avgcentra$targetX.512 %in% c(59,24,-24,-59)&avgcentra$type == "C" ,]


ggplot(subset(AllPositionOutlierRemove, targetX.512==-413 & targetY.384==-171 & subject == "ez")) + 
  geom_point(aes(x=targetX.512, y=targetY.384)) + 
  geom_point(aes(x=responseX.512, y=responseY.384, color=vision.type), shape=4) +
  geom_point(aes(x=413,y=171)) +
  geom_point(aes(x=subset(avgcentra, targetX.512==-413 & targetY.384==-171 & subject == "ez")[1,6],y=subset(avgcentra, targetX.512==-413 & targetY.384==-171 & subject == "ez")[1,7]),shape=10) #+
#  geom_point(aes(x=subset(avgcentra, targetX.512==-413 & targetY.384==-171 & subject == "ez")[2,6],y=subset(avgcentra, targetX.512==-413 & targetY.384==-171 & subject == "ac")[2,7]),shape=10)

((subset(avgcentra, targetX.512==-413 & targetY.384==-171 & subject == "ez")$responseX.512-413)^2 + (subset(avgcentra, targetX.512==-413 & targetY.384==-171 & subject == "ez")$responseY.384-171)^2)^0.5


