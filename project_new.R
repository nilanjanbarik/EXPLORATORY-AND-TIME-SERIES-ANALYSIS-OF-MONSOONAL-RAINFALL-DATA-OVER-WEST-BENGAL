##import the data from the excel

Monsoonal_Rainfall_Data<-read.csv(file="E:\\EDUCATION\\6th Semestar\\PROJECT\\Data_Annual_Rain_Tmax_Tmin_1951-2015 (2).csv",head=TRUE)

##view the dataset

Monsoonal_Rainfall_Data


##some basic statistics

M<-mean(Monsoonal_Rainfall_Data$mm)
M.D<-median(Monsoonal_Rainfall_Data$mm)
STD<-sd(Monsoonal_Rainfall_Data$mm)
Minimum_rainfall<-min(Monsoonal_Rainfall_Data$mm)
Maximum_rainfall<-max(Monsoonal_Rainfall_Data$mm)
Range<- range(Monsoonal_Rainfall_Data$mm)
cv<-STD/M*100



##Boxplot of the dataset

boxplot(Monsoonal_Rainfall_Data$mm,ylim=c(3,7),xlab="Year", ylab="Precipitation (mm/day)",main="Boxplot of the Daily Avarage Monsoonal Rainfall in West Bengal")



##Decadal boxplots

par(mfrow=c(1,7))
boxplot(Monsoonal_Rainfall_Data$mm[1:10],ylim=c(3,7),xlab="1951-1960", ylab="Precipitation (mm/day)",main="Decadal Boxplot of 1951-1960")
boxplot(Monsoonal_Rainfall_Data$mm[11:20],ylim=c(3,7),xlab="1961-1970", ylab="Precipitation (mm/day)",main="Decadal Boxplot of 1961-1970")
boxplot(Monsoonal_Rainfall_Data$mm[21:30],ylim=c(3,7),xlab="1970-1980", ylab="Precipitation (mm/day)",main="Decadal Boxplot of 1971-1980")
boxplot(Monsoonal_Rainfall_Data$mm[31:40],ylim=c(3,7),xlab="1981-1990", ylab="Precipitation (mm/day)",main="Decadal Boxplot of 1981-1990")
boxplot(Monsoonal_Rainfall_Data$mm[41:50],ylim=c(3,7),xlab="1991-2000", ylab="Precipitation (mm/day)",main="Decadal Boxplot of 1991-2000")
boxplot(Monsoonal_Rainfall_Data$mm[51:60],ylim=c(3,7),xlab="2001-2010", ylab="Precipitation (mm/day)",main="Decadal Boxplot of 2001-2010")
boxplot(Monsoonal_Rainfall_Data$mm[61:65],ylim=c(3,7),xlab="2011-2015", ylab="Precipitation (mm/day)",main="Decadal Boxplot of 2011-2015")



##Analyzing the boxplot

info<-boxplot(Monsoonal_Rainfall_Data$mm,ylim=c(3,7),xlab="Year", ylab="Precipitation (mm/day)",main="Boxplot of the Daily Avarage Monsoonal Rainfall in West Bengal")
info


##Histogram of the dataset

hist(Monsoonal_Rainfall_Data$mm,probability = TRUE,xlab="Precipitation(mm/day)",ylab="Frequency Density",main="Histogram of the Daily Avarage Monsoonal Rainfall Data in West Bengal")
lines(density(Monsoonal_Rainfall_Data$mm),lwd=2,col="blue")


##Mean Deviation Plot
Mean_Deviation<-Monsoonal_Rainfall_Data$mm-M
barplot(Mean_Deviation,width = 1,ylim=c(-1.5,1.5),xlab="Year",ylab="Mean Deviation",main="Mean Deviation Plot")
abline(0,0)

hist(Mean_Deviation,probability = TRUE)






##Trend Line##
#linear

fit <- glm(Monsoonal_Rainfall_Data$mm~Monsoonal_Rainfall_Data$ï..Year)
co <- coef(fit)
plot(Monsoonal_Rainfall_Data$ï..Year,Monsoonal_Rainfall_Data$mm,,type="l",xlim=c(1951,2015),ylim=c(3.5,6.5),xlab="Year",ylab="Precipitation (mm/day)",main="Linear Trend")
abline(fit, col="blue", lwd=2)


##Moving Average 
install.packages("zoo")
library(zoo)  ##install library 'Zoo'

MA <- rollmean(Monsoonal_Rainfall_Data$mm, k = 10) 
plot(Monsoonal_Rainfall_Data$ï..Year,Monsoonal_Rainfall_Data$mm,,type="l",xlim=c(1951,2015),ylim=c(3.5,6.5),xlab="Year",ylab="Precipitation (mm/day)",main="Linear Trend")
lines(MA, col="green", lwd=2)



plot(Monsoonal_Rainfall_Data$ï..Year,Monsoonal_Rainfall_Data$mm,type="b",bg="blue",pch=16,xlab="Year",ylab="Precipitation (mm/day)",main="Moving Average")
lines(Monsoonal_Rainfall_Data$ï..Year[5:60],MA, col="blue",type = "l", lwd=2)




#dekadal trend

##Mann-Kendall Test

install.packages("trend")
library(trend)

mk.test(Monsoonal_Rainfall_Data$mm, alternative = c("two.sided"), continuity = TRUE)

##Sen's Slope

sens.slope(Monsoonal_Rainfall_Data$mm)


## First decade trend 
fit1 <- glm(Monsoonal_Rainfall_Data$mm[1:10]~Monsoonal_Rainfall_Data$ï..Year[1:10])
co1 <- coef(fit1)
mk.test(Monsoonal_Rainfall_Data$mm[1:10], alternative = c("two.sided"), continuity = TRUE)
sens.slope(Monsoonal_Rainfall_Data$mm[1:10])


## Second decade trend 
fit2 <- glm(Monsoonal_Rainfall_Data$mm[11:20]~Monsoonal_Rainfall_Data$ï..Year[11:20])
co2 <- coef(fit2)
mk.test(Monsoonal_Rainfall_Data$mm[11:20], alternative = c("two.sided"), continuity = TRUE)
sens.slope(Monsoonal_Rainfall_Data$mm[11:20])


## Third decade trend \
fit3 <- glm(Monsoonal_Rainfall_Data$mm[21:30]~Monsoonal_Rainfall_Data$ï..Year[21:30])
co3 <- coef(fit3)
mk.test(Monsoonal_Rainfall_Data$mm[21:30], alternative = c("two.sided"), continuity = TRUE)
sens.slope(Monsoonal_Rainfall_Data$mm[21:30])


## Fourth decade trend 
fit4 <- glm(Monsoonal_Rainfall_Data$mm[31:40]~Monsoonal_Rainfall_Data$ï..Year[31:40])
co4 <- coef(fit4)
mk.test(Monsoonal_Rainfall_Data$mm[31:40], alternative = c("two.sided"), continuity = TRUE)
sens.slope(Monsoonal_Rainfall_Data$mm[31:40])


## Fifth decade trend 
fit5 <- glm(Monsoonal_Rainfall_Data$mm[41:50]~Monsoonal_Rainfall_Data$ï..Year[41:50])
co5 <- coef(fit5)
mk.test(Monsoonal_Rainfall_Data$mm[41:50], alternative = c("two.sided"), continuity = TRUE)
sens.slope(Monsoonal_Rainfall_Data$mm[41:50])


## Sixth decade trend 
fit6 <- glm(Monsoonal_Rainfall_Data$mm[51:60]~Monsoonal_Rainfall_Data$ï..Year[51:60])
co6 <- coef(fit6)
mk.test(Monsoonal_Rainfall_Data$mm[51:60], alternative = c("two.sided"), continuity = TRUE)
sens.slope(Monsoonal_Rainfall_Data$mm[51:60])


## Last decade trend 
fit7 <- glm(Monsoonal_Rainfall_Data$mm[61:65]~Monsoonal_Rainfall_Data$ï..Year[61:65])
co7 <- coef(fit7)
mk.test(Monsoonal_Rainfall_Data$mm[61:65], alternative = c("two.sided"), continuity = TRUE)
sens.slope(Monsoonal_Rainfall_Data$mm[61:65])


## decadal plot with overal trend 

plot(Monsoonal_Rainfall_Data$ï..Year, Monsoonal_Rainfall_Data$mm, xlab = "Year", ylab = "Precipitation (mm/day)",
     col = "blue",bg = "blue", pch = 16, cex=1,xaxs = "i")
lines(Monsoonal_Rainfall_Data$ï..Year, Monsoonal_Rainfall_Data$mm, col = "black")
lines(Monsoonal_Rainfall_Data$ï..Year[1:10], predict(fit1), col = "red")
lines(Monsoonal_Rainfall_Data$ï..Year[11:20], predict(fit2), col = "red")
lines(Monsoonal_Rainfall_Data$ï..Year[21:30], predict(fit3), col = "red")
lines(Monsoonal_Rainfall_Data$ï..Year[31:40], predict(fit4), col = "red")
lines(Monsoonal_Rainfall_Data$ï..Year[41:50], predict(fit5), col = "red")
lines(Monsoonal_Rainfall_Data$ï..Year[51:60], predict(fit6), col = "red")
lines(Monsoonal_Rainfall_Data$ï..Year[61:65], predict(fit7), col = "red")
lines(Monsoonal_Rainfall_Data$ï..Year, predict(fit), col = "blue", lty = 2,lwd=2)





##############################################################



##plotting scatter plot

plot(Monsoonal_Rainfall_Data$mm)
plot(Monsoonal_Rainfall_Data$mm,type = "l")

##plotting the scatter plot using ggplot

ggplot(Monsoonal_Rainfall_Data,aes(x=ï..Year,y=mm,group= 1))+geom_line()+scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

##creating a cdf of the data

CDF<-ecdf(Monsoonal_Rainfall_Data$mm)
plot(CDF)

##Standardization of the data

Standadized_data<-scale(Monsoonal_Rainfall_Data$mm,center = TRUE,scale = TRUE)
Standadized_data

hist(Standadized_data,probability = TRUE)
lines(density(Standadized_data),col="green",lwd=4)







##Decadal Exploratory analysis

#first decade

M1<-mean(Monsoonal_Rainfall_Data$mm[1:10])
M.D1<-median(Monsoonal_Rainfall_Data$mm[1:10])
STD1<-sd(Monsoonal_Rainfall_Data$mm[1:10])
Minimum_rainfall1<-min(Monsoonal_Rainfall_Data$mm[1:10])
Maximum_rainfall1<-max(Monsoonal_Rainfall_Data$mm[1:10])
cv1<-STD1/M1*100

#second decade

M2<-mean(Monsoonal_Rainfall_Data$mm[11:20])
M.D2<-median(Monsoonal_Rainfall_Data$mm[11:20])
STD2<-sd(Monsoonal_Rainfall_Data$mm[11:20])
Minimum_rainfall2<-min(Monsoonal_Rainfall_Data$mm[11:20])
Maximum_rainfall2<-max(Monsoonal_Rainfall_Data$mm[11:20])
cv2<-STD2/M2*100

#third decade

M3<-mean(Monsoonal_Rainfall_Data$mm[21:30])
M.D3<-median(Monsoonal_Rainfall_Data$mm[21:30])
STD3<-sd(Monsoonal_Rainfall_Data$mm[21:30])
Minimum_rainfall3<-min(Monsoonal_Rainfall_Data$mm[21:30])
Maximum_rainfall3<-max(Monsoonal_Rainfall_Data$mm[21:30])
cv3<-STD3/M3*100

#fourth decade

M4<-mean(Monsoonal_Rainfall_Data$mm[31:40])
M.D4<-median(Monsoonal_Rainfall_Data$mm[31:40])
STD4<-sd(Monsoonal_Rainfall_Data$mm[31:40])
Minimum_rainfall4<-min(Monsoonal_Rainfall_Data$mm[31:40])
Maximum_rainfall4<-max(Monsoonal_Rainfall_Data$mm[31:40])
cv4<-STD4/M4*100

#fifth decade

M5<-mean(Monsoonal_Rainfall_Data$mm[41:50])
M.D5<-median(Monsoonal_Rainfall_Data$mm[41:50])
STD5<-sd(Monsoonal_Rainfall_Data$mm[41:50])
Minimum_rainfall5<-min(Monsoonal_Rainfall_Data$mm[41:50])
Maximum_rainfall5<-max(Monsoonal_Rainfall_Data$mm[41:50])
cv5<-STD5/M5*100

#sixth decade

M6<-mean(Monsoonal_Rainfall_Data$mm[51:60])
M.D6<-median(Monsoonal_Rainfall_Data$mm[51:60])
STD6<-sd(Monsoonal_Rainfall_Data$mm[51:60])
Minimum_rainfall6<-min(Monsoonal_Rainfall_Data$mm[51:60])
Maximum_rainfall6<-max(Monsoonal_Rainfall_Data$mm[51:60])
cv6<-STD6/M6*100

#seventh decade

M7<-mean(Monsoonal_Rainfall_Data$mm[61:65])
M.D7<-median(Monsoonal_Rainfall_Data$mm[61:65])
STD7<-sd(Monsoonal_Rainfall_Data$mm[61:65])
Minimum_rainfall7<-min(Monsoonal_Rainfall_Data$mm[61:65])
Maximum_rainfall7<-max(Monsoonal_Rainfall_Data$mm[61:65])
cv7<-STD7/M7*100

##decadal_exploratory_analysis
Mean<-c(M1,M2,M3,M4,M5,M6,M7,M)
Median<-c(M.D1,M.D2,M.D3,M.D4,M.D5,M.D6,M.D7,M.D)
Standard_Deviation<-c(STD1,STD2,STD3,STD4,STD5,STD6,STD7,STD)
Max_value<-c(Maximum_rainfall1,Maximum_rainfall2,Maximum_rainfall3,Maximum_rainfall4,Maximum_rainfall5,Maximum_rainfall6,Maximum_rainfall7,Maximum_rainfall)
Min_value<-c(Minimum_rainfall1,Minimum_rainfall2,Minimum_rainfall3,Minimum_rainfall4,Minimum_rainfall5,Minimum_rainfall6,Minimum_rainfall7,Minimum_rainfall)
Coefficient_of_Variation<-c(cv1,cv2,cv3,cv4,cv5,cv6,cv7,cv)
data.frame(Mean,Median,Standard_Deviation,Max_value,Min_value,Coefficient_of_Variation)
