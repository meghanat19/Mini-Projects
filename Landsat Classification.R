#SAMSI Landsat Classification Project 
library(fields)
library(ggplot2)
library(gridExtra)
load("vegetation_data_bigger.Rdata")

#preliminary steps
qplot(x=inds[,1], y=inds[,2], fill=cdl, geom="tile", main="Landcover", xlab="", ylab="")
qplot(x=inds[,1], y=inds[,2], fill=cdl_small, geom="tile", main="Landcover", xlab="", ylab="")

#finding max evi by landtype (box plot)
max_evi <- apply(EVI_Landsat, 1, max, na.rm=TRUE)
head(max_evi)
qplot(cdl_small, max_evi, geom="boxplot")

#finding min evi by landtype (box plot)
min_evi <- apply(EVI_Landsat, 1, min, na.rm=TRUE)
head(min_evi)
qplot(cdl_small, min_evi, geom="boxplot")

#subsetting landtypes to create plots for the spring greenup
evi<- EVI_Landsat
str(EVI_Landsat)
str(cdl_small)

corn <-subset(evi, cdl_small=="corn")
water <-subset(evi, cdl_small=="open water")
soybeans <-subset(evi, cdl_small=="soybeans")
grasslands <-subset(evi, cdl_small=="grasslands/herb")
wetlands <-subset(evi, cdl_small=="woody wetlands")
developed <-subset(evi, cdl_small=="developed")
deciduous <-subset(evi, cdl_small=="deciduous forest")

#plotting land covers for start of season
p1 <- qplot(dates, colMeans(corn,na.rm=T, dim=1),main="corn", ylab="EVI")
p2 <- qplot(dates, colMeans(water,na.rm=T, dim=1),main="water", ylab="EVI")
p3 <- qplot(dates, colMeans(soybeans,na.rm=T, dim=1),main="soybeans", ylab="EVI")
p4 <- qplot(dates, colMeans(developed,na.rm=T, dim=1),main="developed", ylab="EVI")
p5 <- qplot(dates, colMeans(deciduous,na.rm=T, dim=1),main="deciduous forest", ylab="EVI")
p6 <- qplot(dates, colMeans(wetlands,na.rm=T, dim=1),main="wetlands", ylab="EVI")
p7 <- qplot(dates, colMeans(grasslands,na.rm=T, dim=1),main="grasslands", ylab="EVI")
grid.arrange(p1,p2,p3,p4,p5,p6,p7, ncol=2)



# Duration of Season 
start<-apply(EVI_Landsat, 1, function(x) which(x > 0.2)[1])
start[is.na(start)] <- 0
dates[start]

l<-which(corn[1,] > 0.2)
dates[max(l)]-dates[min(l)]

l<-which(deciduous_forest[1,] > 0.2)
dates[max(l)]-dates[min(l)]

l<-which(developed[1,] > 0.2)
dates[max(l)]-dates[min(l)]

l<-which(soybeans[1,] > 0.2)
dates[max(l)]-dates[min(l)]

l<-which(wetlands[1,] > 0.2)
dates[max(l)]-dates[min(l)]

l<-which(grassland_herb[1,] > 0.2)
dates[max(l)]-dates[min(l)]

l<-which(water[1,] > 0.2)
dates[max(l)]-dates[min(l)]

# Average Reflectance Values
redAverage <- apply(Landsat_red, 1, mean, na.rm=TRUE)
blueAverage <- apply(Landsat_blue, 1, mean, na.rm=TRUE)
greenAverage <-apply(Landsat_green, 1, mean, na.rm=TRUE)
nirAverage <- apply(Landsat_nir, 1, mean, na.rm=TRUE)

# Plots for Average Reflectance Values
par(mfrow=c(2,2))
plot(cdl_small, blueAverage, main = "Blue Average")
plot(cdl_small, redAverage, main = "Red Average")
plot(cdl_small, greenAverage, main = "Green Average")
plot(cdl_small, nirAverage, main = "Nir Average")

#Differences in Reflectance Values
#Calculating maximum reflectance values
max_Red <- apply(Landsat_red, 1, max, na.rm=TRUE)
max_Blue <- apply(Landsat_blue, 1, max, na.rm = TRUE)
max_Green <- apply(Landsat_green, 1, max, na.rm = TRUE)
max_Nir <- apply(Landsat_nir, 1, max, na.rm = TRUE)

#Calculating minimum reflectance values
min_Red <- apply(Landsat_red, 1, min, na.rm=TRUE)
min_Blue <- apply(Landsat_blue, 1, min, na.rm=TRUE)
min_Green <- apply(Landsat_green, 1, min, na.rm=TRUE)
min_Nir <- apply(Landsat_nir, 1, min, na.rm=TRUE)

#Difference between the two
dif_Red <- max_Red - min_Red
dif_Blue <- max_Blue - min_Blue
dif_Green <- max_Green - min_Green
dif_Nir <- max_Nir - min_Nir

#Plots 4 Differences
par(mfrow=c(2,2))
plot(cdl_small, dif_Red, main = "Difference between Max and Minimum Red Reflectance")
plot(cdl_small, dif_Blue, main = "Difference between Max and Minimum Blue Reflectance")
plot(cdl_small, dif_Green, main = "Difference between Max and Minimum Green Reflectance")
plot(cdl_small, dif_Nir, main = "Difference between Max and Minimum Nir Reflectance")

# Difference Between Max and Min EVI 
max_evi <- apply(EVI_Landsat, 1, max, na.rm=TRUE)
min_evi <- apply(EVI_Landsat, 1, min, na.rm=TRUE)
max_evi - min_evi



#Ganlin Ye #13
load("C:/Users/gye2/Downloads/2018 samsi/vegetation_data_bigger.Rdata")
n<-length(cdl_small)
table(cdl_small)
View(cdl_small[9])
landsat_new<-cbind(cdl_small,EVI_Landsat)
landsat_new[9,1]
for (i in 1:n){
  if (landsat_new[i,1]==1|landsat_new[i,1]==6){
    landsat_new[i,1]<-1
  }else {
    landsat_new[i,1]<-0
  }
}
landsat_new[,1]
install.packages("ggplot")
library(ggplot)
landsat_new <- as.data.frame(landsat_new)

Agri_meanl<-c()
Agri_meanl<- colMeans(landsat_new[landsat_new$cdl_small=="1",2:65],na.rm = TRUE,dims = 1)
natu_meanl<-c()
natu_meanl<- colMeans(landsat_new[landsat_new$cdl_small=="0",2:65],na.rm = TRUE,dims = 1)

plot(dates,Agri_meanl, pch=5, xlab="date", ylab="EVI",main = "the mean EVI of Agriculture and Nature in Landsat")
points(dates,natu_meanl, col="red", pch=4)
legend("topright", c("Agriculture", "nature plants"), col=c(1:2), pch=c(5:4))

# ##########################################################################################
landsat_location<-cbind(cdl_small,inds_ll,EVI_Landsat)
#landcover and location
par(mfrow=c(1,2))
plot(landsat_location[,1],landsat_location[,2],ylab="longitude")
plot(landsat_location[,1],landsat_location[,3],ylab="Latitude")
par(mfrow=c(1,1))
plot(landsat_location[,1],landsat_location[,2]*landsat_location[,3],ylab="longitude*Latitude")

#########################################################################################











#################################################
#####		temperature at max/min evi		  #######
#################################################

load("vegetation_data_bigger.Rdata")
load("Landsat_reflectance_2008.Rdata")
#x-axis: maximum EVI
#y-axis: temperature of maximum EVI
max_evi <- apply(EVI_Landsat, 1, max, na.rm=TRUE)
#head(max_evi)
max_evi_time <- apply(EVI_Landsat, 1, which.max)
head(max_evi_time)
dates[max_evi_time]

d2 <- seq.Date(as.Date("2008-01-01"), as.Date("2008-12-31"), by=1)
gdd_sub <- gdd_mat[,d2 %in% dates]
length(gdd_sub)
gdd_mat

tem_at_maxi_evi = max_evi_time
for (i in 1:length(max_evi_time)){
  tem_at_maxi_evi[i] <- gdd_sub[i,max_evi_time[i]]
}
tem_at_maxi_evi

plot(cdl_small,tem_at_maxi_evi)

min_evi <- apply(EVI_Landsat,1,min,na.rm=TRUE)
head(min_evi)
min_evi_time <- apply(EVI_Landsat,1,which.min)
head(min_evi_time)
dates[min_evi_time]

d3 <- seq.Date(as.Date("2008-01-01"),as.Date("2008-12-31"),by=1)
gdd_sub2 <- gdd_mat[,d3 %in% dates]
gdd_sub2
tem_at_min_evi = min_evi_time
for (j in 1:length(min_evi_time)){
  tem_at_min_evi[j] <- gdd_sub2[j,min_evi_time[j]]
}

tem_at_min_evi

plot(cdl_small,tem_at_min_evi)

#################################################
#####	    	growth rate of plants		  #######
#################################################

#length(min_evi_time)
#length(max_evi_time)
#time_difference = min_evi_time
#for (i in 1:length(min_evi_time)){
#  time_difference[i]<-max_evi_time[i]-min_evi_time[i]
#}
#time_difference

length(max_evi)
length(min_evi)
EVI_difference = max_evi
for (j in 1:length(min_evi)){
  EVI_difference[j]<-max_evi[j]-min_evi[j]
}
EVI_difference

length(EVI_difference)
length(time_difference)
rate = EVI_difference
for (k in 1:length(EVI_difference)){
  rate[k] <- EVI_difference[k]/time_difference[k]
}
rate

#Extract different types of land
new <- cbind(cdl,EVI_Landsat)
cdl
corn <- subset(new,cdl=="corn")
corn <- corn[,2:ncol(corn)]
corn

#Get the start date
start<-apply(EVI_Landsat, 1, function(x) which(x > 0.2)[1])
start[is.na(start)] <- 1
start
#lcorn <- vector(mode="integer", length=nrow(start))
#for (i in 1:nrow(start)){
#  lcorn[i] <- min(which(corn[i,] > 0.2) )
#}
#lcorn
#length(lcorn)

#Get the start evi
length(start)
time_difference = max_evi_time - start
time_difference
start_evi<- start

for (i in 1:length(start)){
  start_evi[i]<-EVI_Landsat[i,start[i]]
}
start_evi

#Get the evi_differeces for each location between start and maximum evi
length(start_evi)
length(max_evi)
evi_difference <- max_evi - start_evi
evi_difference[is.na(evi_difference)] <- 0

slope <- evi_difference/time_difference
log_slope <- abs(log(slope))
length(slope)
length(cdl_small)
plot(cdl_small, slope)
plot(cdl_small, log_slope)





#logistic regression water vs. vegetation 
#creating 0 and 1 factors for water vs. vegatation 
ag_natveg <- as.character(cdl_small)
ag_natveg[cdl_small %in% c("open water")] <- 0
ag_natveg[!(cdl_small %in% c("open water"))] <- 1
table(ag_natveg)
ag_natveg <- as.numeric(ag_natveg)

n <- length(ag_natveg)
train_id <- sample(1:n, 0.7*n)
test_id <- which(!( (1:n) %in% train_id))

#features data set 
data<-features 

ag_vg_tr <- ag_natveg[train_id]
ag_vg_ts <- ag_natveg[test_id]

dat_train <- data.frame(ag_vg_tr, data[train_id,])
dat_test <- data[test_id,]

fit_lg <- glm(ag_vg_tr ~ ., family=binomial(link='logit'), data=dat_train)

prob_natveg <- predict(fit_lg, dat_test, type="response")
cl <- as.numeric(prob_natveg > 0.5)
qplot(x=dat_test$max_evi, y=dat_test$max_evi_time, colour=factor(cl))
a <- table(ag_vg_ts, as.numeric(cl))
a
1 - sum(diag(a))/sum(a)


Multinomial Logistic Regression
library(fields)
library(ggplot2)
library(gridExtra)
library(nnet)

load("vegetation_data_bigger.Rdata")
load("features.Rdata")

########################################
# Problem: This model seems to overfit.#
########################################

n <- length(cdl_small)
set.seed(123)
train_id <- sample(1:n, 0.7*n)
test_id <- which(!( (1:n) %in% train_id))

# Removing NAs 
features2 <- features
features2[,3][is.infinite(features2[,3])] <- 0
X <- features2[,c(1,2,3,4,5,6,7,8,9,10,11,13,14,16)]

cdl_tr <- cdl_small[train_id]
cdl_ts <- cdl_small[test_id]

dat_train <- data.frame(cdl_tr, X[train_id,])
dat_test <- data.frame(cdl_ts, X[test_id,])
############################################################################################
# Below are 2 models. The model with the best features is already uncommented. The best    #
# features were determined by running the summary and step function below                  #
#                                                                                          #
############################################################################################

#fit_mlg <- multinom(cdl_tr ~ ., data=dat_train)

fit_mlg <- multinom(formula = cdl_tr ~ max_evi + max_evi_time + redAverage + 
                      blueAverage + greenAverage + nirAverage + dif_Blue + dif_Green + 
                      dif_Nir + dif_Red + tem_at_maxi_evi + tem_at_min_evi + start, 
                    data = dat_train)


##################################    Test Data Results  ###################################
summary(fit_mlg)

pred_cl <- predict(fit_mlg, dat_test, type="class")
b <- table(cdl_ts, pred_cl)
b

1 - sum(diag(b))/sum(b)

################################   Training Data Results  ##################################
pred_c <- predict(fit_mlg, dat_train, type="class")
a <- table(cdl_tr, pred_c)
a

1 - sum(diag(a))/sum(a)

############################################################################################
# The functions below were used when running                                               #
# fit_mlg <- multinom(cdl_tr ~ ., data=dat_train)  (Model with all features)               #                         #
# The combination of these functions showed which features were important. T               #              
# They are commented below because the best model is running first by default.             #
# Change models and uncomment code if you want to see results.                             #
############################################################################################

#s <- step(fit_mlg)
#summary(s)




#################################
#add location variable into featrue
location<-inds_ll[,1]*inds_ll[,2]
data<-cbind(features,location)
#################################

#https://www.overleaf.com/16417922dbxxqbzfqrjx#/62980705/
