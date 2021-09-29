# ET estimation for sensor 101 in West Weber 2019

# Packages ####
library(readxl)
library(writexl)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(multcompView)
library(tidyverse)

# Load the data and define the variables for sensor 101####
setwd("C:/User/Graduate/Box/DiviningWater/West Weber 2019/Soil sensor data/Sensor 101") # set the working directory in the folder containing the data file
data.101=read_excel("DailySM.101.xlsx") # load the SM data from excel
data.101=na.omit(data.101) # eliminate the days with no data

n=length(data.101$Date.101) # Number of days with data
ETo=data.101$ETo_mm         # Reference ET (mm)

# Sensor depth (mm)
sd1=sd2=sd3=7*25.4        # sensor 1, 2 and 3: from surface to 7"
sd4=sd5=sd6=(7+9)*25.4    # sensor 4, 5 and 6: from 7" to 16"
sd7=sd8=sd9=(7+9+13)*25.4 # sensor 7, 8 and 9: from 16" to 29"
sd10=(7+9+13+14)*25.4     # sensor 10: from 29" to 43"

# Soil moisture - volumetric water content %
sm1=data.101$sm1.101
sm2=data.101$sm2.101
sm3=data.101$sm3.101
sm4=data.101$sm4.101
sm5=data.101$sm5.101
sm6=data.101$sm6.101
sm7=data.101$sm7.101
sm8=data.101$sm8.101
sm9=data.101$sm9.101
sm10=data.101$sm10.101

# Plot daily soil moisture data
# Left side of the bed
plot (data.101$Date.101, sm1, ylim=c(0, 60), type='l', lwd=3, col='coral3', 
      main='Daily SM - Left side of the bed', xlab='', ylab='Soil water content (%)') 
lines(data.101$Date.101, sm4, type='l', lwd=3, col='steelblue3')
lines(data.101$Date.101, sm7, type='l', lwd=3, col='gold3')
lines(data.101$Date.101, sm10, type='l', lwd=3, col='olivedrab3')
legend('bottom', lty=1, lwd=3, legend=c('Sensor 1: 7"','Sensor 4: 16"','Sensor 7: 29"','Sensor 10: 43"'), col = c('coral3','steelblue3','gold3','olivedrab3'), ncol=2)
# Right side of the bed
plot (data.101$Date.101, sm2, ylim=c(0, 60), type='l', lwd=3, col='coral3', 
      main='Daily SM - Right side of the bed', xlab='', ylab='Soil water content (%)') 
lines(data.101$Date.101, sm5, type='l', lwd=3, col='steelblue3')
lines(data.101$Date.101, sm8, type='l', lwd=3, col='gold3')
lines(data.101$Date.101, sm10, type='l', lwd=3, col='olivedrab3')
legend('bottom', lty=1, lwd=3, legend=c('Sensor 2: 7"','Sensor 5: 16"','Sensor 8: 29"','Sensor 10: 43"'), col = c('coral3','steelblue3','gold3','olivedrab3'), ncol=2)
# Furrow
plot (data.101$Date.101, sm3, ylim=c(0, 60), type='l', lwd=3, col='coral3', 
      main='Daily SM - Furrow', xlab='', ylab='Soil water content (%)') 
lines(data.101$Date.101, sm6, type='l', lwd=3, col='steelblue3')
lines(data.101$Date.101, sm9, type='l', lwd=3, col='gold3')
lines(data.101$Date.101, sm10, type='l', lwd=3, col='olivedrab3')
legend('bottom', lty=1, lwd=3, legend=c('Sensor 3: 7"','Sensor 6: 16"','Sensor 9: 29"','Sensor 10: 43"'), col = c('coral3','steelblue3','gold3','olivedrab3'), ncol=2)

# Root depth (mm)
rd=data.101$Zr

# plot root depth with sensor depth
plot(data.101$Date.101, -rd, 
     type='h', col='deeppink4', lwd='3', ylim=c(-1200, 0),
     main='Onion root depth, West Weber 2019', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=3)
abline(h=-sd1, col='coral3', lwd=3)
abline(h=-sd4, col='steelblue3', lwd=3)
abline(h=-sd7, col='gold3', lwd=3)
abline(h=-sd10, col='olivedrab3', lwd=3)
legend('bottomleft', inset=0.02, lty=1, lwd=3, legend=c('Sensor 1, 2, 3: 7"','Sensor 4, 5, 6: 16"','Sensor 7, 8, 9: 29"','Sensor 10: 43"'), col=c('coral3','steelblue3','gold3','olivedrab3'), ncol=1)


# Method A: Water balance on the soil profile as a whole ####
# This method estimates ET (mm/day) by applying the water balance method to the 
# entire soil profile at once.

# Left side of the onion bed 
# Total water content (mm) of the soil profile
wc.1=(sm1*sd1+sm4*sd4+sm7*sd7+sm10*sd10)/100

# ET estimation
ET.A.L=c()
for(i in 2:n) { 
  ET.A.L[i]=wc.1[i-1]-wc.1[i]}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET.A.L[i]<0) {ET.A.L[i]=0}}         
plot(data.101$Date.101, ET.A.L, 
     type='h', lwd=2,
     main='ET from the left side of the bed', xlab='', ylab='ET (mm)')

# Right side of the onion bed 
# Total water content (mm) of the soil profile
wc.2=(sm2*sd2+sm5*sd5+sm8*sd8+sm10*sd10)/100

# ET estimation
ET.A.R=c()
for(i in 2:n) { 
  ET.A.R[i]=wc.2[i-1]-wc.2[i]}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET.A.R[i]<0) {ET.A.R[i]=0}}         
plot(data.101$Date.101, ET.A.R, 
     type='h', lwd=2,
     main='ET from right side of the bed', xlab='', ylab='ET (mm)')

# Furrow to the right of the onion bed 
# Total water content (mm) of the soil profile
wc.3=(sm3*sd3+sm6*sd6+sm9*sd9+sm10*sd10)/100

# ET estimation
ET.A.F=c()
for(i in 2:n) { 
  ET.A.F[i]=wc.3[i-1]-wc.3[i]}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET.A.F[i]<0) {ET.A.F[i]=0}}         
plot(data.101$Date.101, ET.A.F, 
     type='h', lwd=2,
     main='ET from the furrow', xlab='', ylab='ET (mm)')

# Method B: Water balance on each sensors soil depth NOT accounting for root growth ####
# This method estimates ET (mm/day) by applying the water balance to each soil
# layer and then adds them all up after having eliminated all negative 
# contributions. Method B does not account for root growth throughout the season.

# ET from soil depth 1
ET1.101=c() 
for(i in 2:n) { 
  ET1.101[i]=(sm1[i-1]-sm1[i])/100*sd1}
for(i in 2:n) { # eliminate negative ET values
  if (ET1.101[i]<0) {ET1.101[i]=0}}         
plot(data.101$Date.101, ET1.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 1', xlab='', ylab='ET (mm)')

# ET from soil depth 2
ET2.101=c() 
for(i in 2:n) { 
  ET2.101[i]=(sm2[i-1]-sm2[i])/100*sd2}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.101[i]<0) {ET2.101[i]=0}}         
plot(data.101$Date.101, ET2.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 2', xlab='', ylab='ET (mm)')

# ET from soil depth 3
ET3.101=c()
for(i in 2:n) { 
  ET3.101[i]=(sm3[i-1]-sm3[i])/100*sd3}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.101[i]<0) {ET3.101[i]=0}}         
plot(data.101$Date.101, ET3.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 3', xlab='', ylab='ET (mm)')

# ET from soil depth 4
ET4.101=c()
for(i in 2:n) { 
  ET4.101[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd1)}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.101[i]<0) {ET4.101[i]=0}}         
plot(data.101$Date.101, ET4.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 4', xlab='', ylab='ET (mm)')

# ET from soil depth 5
ET5.101=c() 
for(i in 2:n) { 
  ET5.101[i]=(sm5[i-1]-sm5[i])/100*(sd5-sd2)}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.101[i]<0) {ET5.101[i]=0}}         
plot(data.101$Date.101, ET5.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 5', xlab='', ylab='ET (mm)')

# ET from soil depth 6
ET6.101=c()
for(i in 2:n) {
  ET6.101[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd3) }
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.101[i]<0) {ET6.101[i]=0}}         
plot(data.101$Date.101, ET6.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 6', xlab='', ylab='ET (mm)')

# ET from soil depth 7
ET7.101=c()
for(i in 2:n) {
  ET7.101[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd4) }
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.101[i]<0) {ET7.101[i]=0} }         
plot(data.101$Date.101, ET7.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 7', xlab='', ylab='ET (mm)')

# ET from soil depth 8
ET8.101=c()
for(i in 2:n) {
  ET8.101[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd5) }
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.101[i]<0) {ET8.101[i]=0} }         
plot(data.101$Date.101, ET8.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 8', xlab='', ylab='ET (mm)')

# ET from soil depth 9
ET9.101=c()
for(i in 2:n) {
  ET9.101[i]=(sm9[i-1]-sm9[i])/100*(sd9-sd6) }
for(i in 2:n) { # eliminate negative ET contributions
  if (ET9.101[i]<0) {ET9.101[i]=0} }         
plot(data.101$Date.101, ET9.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 9', xlab='', ylab='ET (mm)')

# ET from soil depth 10
ET10.101=c()
for(i in 2:n) {
  ET10.101[i]=(sm10[i-1]-sm10[i])/100*(sd10-sd9) }
for(i in 2:n) { # eliminate negative ET contributions
  if (ET10.101[i]<0) {ET10.101[i]=0} }         
plot(data.101$Date.101, ET10.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 10', xlab='', ylab='ET (mm)')

# Total ET for the left side of the onion bed
ET.B.L=ET1.101+ET4.101+ET7.101+ET10.101
ET.B.L[1]=NA # can't calculate ET the first day
plot(data.101$Date.101, ET.B.L,
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for the left side of the bed', xlab='', ylab='ET (mm)')
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)

# Heatmap
x=data.101$Date.101
y=paste('sensor', c(10,7,4,1))
ET=c(ET10.101, ET7.101, ET4.101, ET1.101)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) for the left side of the bed') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Total ET for the right side of the onion bed
ET.B.R=ET2.101+ET5.101+ET8.101+ET10.101
ET.B.R[1]=NA # can't calculate ET the first day
plot(data.101$Date.101, ET.B.R,
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for the right side of the bed', xlab='', ylab='ET (mm)')
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)

# Heatmap
x=data.101$Date.101
y=paste('sensor', c(10,8,5,2))
ET=c(ET10.101, ET8.101, ET5.101, ET2.101)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) for the right side of the bed') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Total ET for the furrow
ET.B.F=ET3.101+ET6.101+ET9.101+ET10.101
ET.B.F[1]=NA # can't calculate ET the first day
plot(data.101$Date.101, ET.B.F,
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for the furrow', xlab='', ylab='ET (mm)')
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)

# Heatmap
x=data.101$Date.101
y=paste('sensor', c(10,8,5,2))
ET=c(ET10.101, ET9.101, ET6.101, ET3.101)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) for the furrow') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Method C: Water balance on each sensors soil depth accounting for root growth ####
# This method estimates ET (mm/day) by applying the water balance to each soil
# layer and then adds them all up after having eliminated all negative 
# contributions. Method C takes root growth throughout the season in account.

# Adjusted root depth: the average with previous day is used for the calculations.
RD=c()      
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today

# Sub-station 1

# ET from soil depth 1
ET1.101=c()
for(i in 2:n) {
  if (RD[i] < sd1) {
    ET1.101[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  else {
    ET1.101[i]=(sm1[i-1]-sm1[i])/100*sd1}}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET1.101[i]<0) {ET1.101[i]=0}}         

plot(data.101$Date.101, ET1.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 1', xlab='', ylab='ET (mm)')

# ET from soil depth 2
ET2.101=c()
for(i in 2:n) {
  if (RD[i] < sd2) {
    ET2.101[i]=(sm2[i-1]-sm2[i])/100*RD[i] }
  else {
    ET2.101[i]=(sm2[i-1]-sm2[i])/100*sd2}}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET2.101[i]<0) {ET2.101[i]=0}}         

plot(data.101$Date.101, ET2.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 2', xlab='', ylab='ET (mm)')

# ET from soil depth 3
ET3.101=c()
for(i in 2:n) {
  if (RD[i] < sd3) {
    ET3.101[i]=(sm3[i-1]-sm3[i])/100*RD[i] }
  else {
    ET3.101[i]=(sm3[i-1]-sm3[i])/100*sd3}}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET3.101[i]<0) {ET3.101[i]=0}}         

plot(data.101$Date.101, ET3.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 3', xlab='', ylab='ET (mm)')

# ET from soil depth 4
ET4.101=c()
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET4.101[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd4) {
    ET4.101[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd1) }
  else {
    ET4.101[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd1) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET4.101[i]<0) {ET4.101[i]=0}}    

plot(data.101$Date.101, ET4.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 4', xlab='', ylab='ET (mm)')

# ET from soil depth 5
ET5.101=c()
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET5.101[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd5) {
    ET5.101[i]=(sm5[i-1]-sm5[i])/100*(RD[i]-sd2) }
  else {
    ET5.101[i]=(sm5[i-1]-sm5[i])/100*(sd5-sd2) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET5.101[i]<0) {ET5.101[i]=0}}    

plot(data.101$Date.101, ET5.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 5', xlab='', ylab='ET (mm)')

# ET from soil depth 6
ET6.101=c()
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET6.101[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd6) {
    ET6.101[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd3) }
  else {
    ET6.101[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd3) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET6.101[i]<0) {ET6.101[i]=0}}    

plot(data.101$Date.101, ET6.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 6', xlab='', ylab='ET (mm)')

# ET from soil depth 7
ET7.101=c()
for(i in 2:n) {
  if (RD[i] <= sd4) {
    ET7.101[i]=0}
  else if (sd4 < RD[i] & RD[i] <= sd7) {
    ET7.101[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd4) }
  else {
    ET7.101[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd4) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET7.101[i]<0) {ET7.101[i]=0} }   

plot(data.101$Date.101, ET7.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 7', xlab='', ylab='ET (mm)')

# ET from soil depth 8
ET8.101=c()
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET8.101[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd8) {
    ET8.101[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd5) }
  else {
    ET8.101[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd5) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET8.101[i]<0) {ET8.101[i]=0} }   

plot(data.101$Date.101, ET8.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 8', xlab='', ylab='ET (mm)')

# ET from soil depth 9
ET9.101=c()
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET9.101[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd9) {
    ET9.101[i]=(sm9[i-1]-sm9[i])/100*(RD[i]-sd6) }
  else {
    ET9.101[i]=(sm9[i-1]-sm9[i])/100*(sd9-sd6) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET9.101[i]<0) {ET9.101[i]=0} }   

plot(data.101$Date.101, ET9.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 9', xlab='', ylab='ET (mm)')

# ET from soil depth 10
ET10.101=c()
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET10.101[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd7) {
    ET10.101[i]=(sm10[i-1]-sm10[i])/100*(RD[i]-sd7) }
  else {
    ET10.101[i]=(sm10[i-1]-sm10[i])/100*(sd10-sd7) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET10.101[i]<0) {ET10.101[i]=0} }   

plot(data.101$Date.101, ET10.101, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 10', xlab='', ylab='ET (mm)')

# Total ET for left side
ET.C.L=ET1.101+ET4.101+ET7.101+ET10.101
plot(data.101$Date.101, ET.C.L,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET - left side', xlab='', ylab='ET (mm)')
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)

# Heatmap
x=data.101$Date.101
y=paste('sensor', c(10, 7, 4, 1))
ET=c(ET10.101, ET7.101, ET4.101, ET1.101)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - left side') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Total ET for right side
ET.C.R=ET2.101+ET5.101+ET8.101+ET10.101
plot(data.101$Date.101, ET.C.R,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET - right side', xlab='', ylab='ET (mm)')
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)

# Heatmap
x=data.101$Date.101
y=paste('sensor', c(10, 8, 5, 2))
ET=c(ET10.101, ET8.101, ET5.101, ET2.101)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - right side') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Total ET for the furrow
ET.C.F=ET3.101+ET6.101+ET9.101+ET10.101
plot(data.101$Date.101, ET.C.F,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET - right side', xlab='', ylab='ET (mm)')
lines(data.101$Date.101, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)

# Heatmap
x=data.101$Date.101
y=paste('sensor', c(10, 9, 6, 3))
ET=c(ET10.101, ET9.101, ET6.101, ET3.101)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - furrow') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Method D: Derivatives on the soil profile as a whole ####
# This method estimates ET (mm/day) by analyzing the first and second derivatives
# of the soil moisture plots. When the first derivative is negative and the second
# derivative is positive we are assuming that water is leaving the system only via 
# evapotranspiration at a rate equal to the slope of the soil moisture graph.

w=3 # window used to calculate the derivatives in days

# Left side of the onion bed
# Total water content (mm) of the soil profile
wc.L=(sm1*sd1+sm4*(sd4-sd1)+sm7*(sd7-sd4)+sm10*(sd10-sd7))/100

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc.L[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}
plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives - left side', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

left=data.frame(Date=data.101$Date.101, SM=wc.L, f1, f2) # data frame with the results for the left side

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
left$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (left$f1[i]<0 & left$f2[i]>0) {left$ET[i]=abs(left$f1[i])} 
}
plot(left$Date, left$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - left side', xlab='', ylab='ET (mm)',
     ylim=c(0,15))
ET.D.L=left$ET

# Right side of the onion bed
# total water content (mm) of the soil profile
wc.R=(sm2*sd2+sm5*(sd5-sd2)+sm8*(sd8-sd5)+sm10*(sd10-sd8))/100

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc.R[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}
plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives - right side', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

right=data.frame(Date=data.101$Date.101, SM=wc.R, f1, f2) # data frame with the results for the right side

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
right$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (right$f1[i]<0 & right$f2[i]>0) {right$ET[i]=abs(right$f1[i])} 
}
plot(right$Date, right$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - right side', xlab='', ylab='ET (mm)',
     ylim=c(0,15))
ET.D.R=right$ET

# Furrow
# total water content (mm) of the soil profile
wc.F=(sm3*sd3+sm6*(sd6-sd3)+sm9*(sd9-sd6)+sm10*(sd10-sd9))/100

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc.F[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}
plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives - furrow', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

furrow=data.frame(Date=data.101$Date.101, SM=wc.F, f1, f2) # data frame with the results for the furrow

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
furrow$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (furrow$f1[i]<0 & furrow$f2[i]>0) {furrow$ET[i]=abs(furrow$f1[i])} 
}
plot(furrow$Date, furrow$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - furrow', xlab='', ylab='ET (mm)',
     ylim=c(0,15))
ET.D.F=furrow$ET

# Method E: Derivatives NOT accounting for root growth####
# Same as method D but on each soil layer individually and then summed up and not accounting for
# root growth over the season.

w=3 # window used to calculate the derivatives in days

# soil depth 1
SM1=sm1*sd1/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 1', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor1=data.frame(Date=data.101$Date.101, SM=SM1, f1, f2) # data frame with the results for sensor1

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor1$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor1$f1[i]<0 & sensor1$f2[i]>0) {sensor1$ET[i]=abs(sensor1$f1[i])} 
}
sensor1$ET[is.na(sensor1$ET)]=0 # replaces NA values with zero
plot(sensor1$Date, sensor1$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 1', xlab='', ylab='ET (mm)',
     ylim=c(0,10))

# soil depth 2
SM2=sm2*sd2/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM2[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 2', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor2=data.frame(Date=data.101$Date.101, SM=SM2, f1, f2) # data frame with the results for sensor2

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor2$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor2$f1[i]<0 & sensor2$f2[i]>0) {sensor2$ET[i]=abs(sensor2$f1[i])} 
}
sensor2$ET[is.na(sensor2$ET)]=0 # replaces NA values with zero
plot(sensor2$Date, sensor2$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET from - soil depth 2', xlab='', ylab='ET (mm)',
     ylim=c(0,10))

# soil depth 3
SM3=sm3*sd3/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM3[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 3', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor3=data.frame(Date=data.101$Date.101, SM=SM3, f1, f2) # data frame with the results for sensor3

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor3$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor3$f1[i]<0 & sensor3$f2[i]>0) {sensor3$ET[i]=abs(sensor3$f1[i])} 
}
sensor3$ET[is.na(sensor3$ET)]=0 # replaces NA values with zero
plot(sensor3$Date, sensor3$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 3', xlab='', ylab='ET (mm)',
     ylim=c(0, 10))

# soil depth 4
SM4=sm4*(sd4-sd1)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM4[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 4', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor4=data.frame(Date=data.101$Date.101, SM=SM4, f1, f2) # data frame with the results for sensor4

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor4$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor4$f1[i]<0 & sensor4$f2[i]>0) {sensor4$ET[i]=abs(sensor4$f1[i])} 
}
sensor4$ET[is.na(sensor4$ET)]=0 # replaces NA values with zero
plot(sensor4$Date, sensor4$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 4', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# soil depth 5
SM5=sm5*(sd5-sd2)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM5[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 5', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor5=data.frame(Date=data.101$Date.101, SM=SM5, f1, f2) # data frame with the results for sensor5

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor5$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor5$f1[i]<0 & sensor5$f2[i]>0) {sensor5$ET[i]=abs(sensor5$f1[i])} 
}
sensor5$ET[is.na(sensor5$ET)]=0 # replaces NA values with zero
plot(sensor5$Date, sensor5$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 5', xlab='', ylab='ET (mm)',
     ylim=c(0,5))

# soil depth 6
SM6=sm6*(sd6-sd3)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM6[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 6', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor6=data.frame(Date=data.101$Date.101, SM=SM6, f1, f2) # data frame with the results for sensor6

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor6$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor6$f1[i]<0 & sensor6$f2[i]>0) {sensor6$ET[i]=abs(sensor6$f1[i])} 
}
sensor6$ET[is.na(sensor6$ET)]=0 # replaces NA values with zero
plot(sensor6$Date, sensor6$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET from - soil depth 2', xlab='', ylab='ET (mm)',
     ylim=c(0,5))

# soil depth 7
SM7=sm7*(sd7-sd4)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM7[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 2', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor7=data.frame(Date=data.101$Date.101, SM=SM7, f1, f2) # data frame with the results for sensor7

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor7$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor7$f1[i]<0 & sensor7$f2[i]>0) {sensor7$ET[i]=abs(sensor7$f1[i])} 
}
sensor7$ET[is.na(sensor7$ET)]=0 # replaces NA values with zero
plot(sensor7$Date, sensor7$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 7', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# soil depth 8
SM8=sm8*(sd8-sd5)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM8[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 2', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor8=data.frame(Date=data.101$Date.101, SM=SM8, f1, f2) # data frame with the results for sensor8

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor8$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor8$f1[i]<0 & sensor8$f2[i]>0) {sensor8$ET[i]=abs(sensor8$f1[i])} 
}
sensor8$ET[is.na(sensor8$ET)]=0 # replaces NA values with zero
plot(sensor8$Date, sensor8$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 8', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# soil depth 9
SM9=sm9*(sd9-sd6)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM9[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 2', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor9=data.frame(Date=data.101$Date.101, SM=SM9, f1, f2) # data frame with the results for sensor9

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor9$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor9$f1[i]<0 & sensor9$f2[i]>0) {sensor9$ET[i]=abs(sensor9$f1[i])} 
}
sensor9$ET[is.na(sensor9$ET)]=0 # replaces NA values with zero
plot(sensor9$Date, sensor9$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 9', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# soil depth 10
SM10=sm10*(sd10-sd7)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM10[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 2', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor10=data.frame(Date=data.101$Date.101, SM=SM10, f1, f2) # data frame with the results for sensor10

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor10$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor10$f1[i]<0 & sensor10$f2[i]>0) {sensor10$ET[i]=abs(sensor10$f1[i])} 
}
sensor10$ET[is.na(sensor10$ET)]=0 # replaces NA values with zero
plot(sensor10$Date, sensor10$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 10', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# Total ET for the left side of the onion bed
ET.E.L=sensor1$ET+
       sensor4$ET+
       sensor7$ET+
       sensor10$ET
plot(data.101$Date.101, ET.E.L, 
     type='h', lwd=2,
     main='Daily ET - left side', xlab='', ylab='ET (mm)',
     ylim=c(0,8))

# Heatmap
x=data.101$Date.101
y=paste('sensor', c(10,7,4,1))
ET=c(sensor10$ET, sensor7$ET, sensor4$ET, sensor1$ET)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - left side') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Total ET for the right side of the onion bed
ET.E.R=sensor2$ET+
       sensor5$ET+
       sensor8$ET+
       sensor10$ET
plot(data.101$Date.101, ET.E.R, 
     type='h', lwd=2,
     main='Daily ET - right side', xlab='', ylab='ET (mm)',
     ylim=c(0,8))

# Heatmap
x=data.101$Date.101
y=paste('sensor', c(10,8,5,2))
ET=c(sensor10$ET, sensor8$ET, sensor5$ET, sensor2$ET)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - right side') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Total ET for the furrow
ET.E.F=sensor3$ET+
       sensor6$ET+
       sensor9$ET+
       sensor10$ET
plot(data.101$Date.101, ET.E.F, 
     type='h', lwd=2,
     main='Daily ET - furrow', xlab='', ylab='ET (mm)',
     ylim=c(0,8))

# Heatmap
x=data.101$Date.101
y=paste('sensor', c(10,9,6,3))
ET=c(sensor10$ET, sensor9$ET, sensor6$ET, sensor3$ET)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - furrow') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Method F: Derivatives accounting for root growth ####
# Same as method F but accounting for root growth over the season.

w=3 # window used to calculate the derivatives in days

# soil depth 1
SM1=sm1*sd1/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 1', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor1=data.frame(Date=data.101$Date.101, SM=SM1, f1, f2) # data frame with the results for sensor1

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor1$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor1$f1[i]<0 & sensor1$f2[i]>0) {sensor1$ET[i]=abs(sensor1$f1[i])} 
}
sensor1$ET[is.na(sensor1$ET)]=0 # replaces NA values with zero
plot(sensor1$Date, sensor1$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 1', xlab='', ylab='ET (mm)',
     ylim=c(0,10))

# soil depth 2
SM2=sm2*sd2/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM2[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 2', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor2=data.frame(Date=data.101$Date.101, SM=SM2, f1, f2) # data frame with the results for sensor2

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor2$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor2$f1[i]<0 & sensor2$f2[i]>0) {sensor2$ET[i]=abs(sensor2$f1[i])} 
}
sensor2$ET[is.na(sensor2$ET)]=0 # replaces NA values with zero
plot(sensor2$Date, sensor2$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET from - soil depth 2', xlab='', ylab='ET (mm)',
     ylim=c(0,10))

# soil depth 3
SM3=sm3*sd3/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM3[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 3', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor3=data.frame(Date=data.101$Date.101, SM=SM3, f1, f2) # data frame with the results for sensor3

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor3$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor3$f1[i]<0 & sensor3$f2[i]>0) {sensor3$ET[i]=abs(sensor3$f1[i])} 
}
sensor3$ET[is.na(sensor3$ET)]=0 # replaces NA values with zero
plot(sensor3$Date, sensor3$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 3', xlab='', ylab='ET (mm)',
     ylim=c(0, 10))

# soil depth 4
SM4=sm4*(sd4-sd1)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM4[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 4', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor4=data.frame(Date=data.101$Date.101, SM=SM4, f1, f2) # data frame with the results for sensor4

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor4$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor4$f1[i]<0 & sensor4$f2[i]>0) {sensor4$ET[i]=abs(sensor4$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr[i]<sd1) 
  {sensor4$ET[i]=NA}
}

sensor4$ET[is.na(sensor4$ET)]=0 # replaces NA values with zero
plot(sensor4$Date, sensor4$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 4', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# soil depth 5
SM5=sm5*(sd5-sd2)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM5[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 5', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor5=data.frame(Date=data.101$Date.101, SM=SM5, f1, f2) # data frame with the results for sensor5

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor5$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor5$f1[i]<0 & sensor5$f2[i]>0) {sensor5$ET[i]=abs(sensor5$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr[i]<sd2) 
  {sensor5$ET[i]=NA}
}

sensor5$ET[is.na(sensor5$ET)]=0 # replaces NA values with zero
plot(sensor5$Date, sensor5$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 5', xlab='', ylab='ET (mm)',
     ylim=c(0,5))

# soil depth 6
SM6=sm6*(sd6-sd3)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM6[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 6', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor6=data.frame(Date=data.101$Date.101, SM=SM6, f1, f2) # data frame with the results for sensor6

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor6$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor6$f1[i]<0 & sensor6$f2[i]>0) {sensor6$ET[i]=abs(sensor6$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr[i]<sd3) 
  {sensor6$ET[i]=NA}
}

sensor6$ET[is.na(sensor6$ET)]=0 # replaces NA values with zero
plot(sensor6$Date, sensor6$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET from - soil depth 6', xlab='', ylab='ET (mm)',
     ylim=c(0,5))

# soil depth 7
SM7=sm7*(sd7-sd4)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM7[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 7', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor7=data.frame(Date=data.101$Date.101, SM=SM7, f1, f2) # data frame with the results for sensor7

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor7$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor7$f1[i]<0 & sensor7$f2[i]>0) {sensor7$ET[i]=abs(sensor7$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr[i]<sd4) 
  {sensor7$ET[i]=NA}
}

sensor7$ET[is.na(sensor7$ET)]=0 # replaces NA values with zero
plot(sensor7$Date, sensor7$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 7', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# soil depth 8
SM8=sm8*(sd8-sd5)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM8[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 7', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor8=data.frame(Date=data.101$Date.101, SM=SM8, f1, f2) # data frame with the results for sensor8

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor8$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor8$f1[i]<0 & sensor8$f2[i]>0) {sensor8$ET[i]=abs(sensor8$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr[i]<sd5) 
  {sensor8$ET[i]=NA}
}

sensor8$ET[is.na(sensor8$ET)]=0 # replaces NA values with zero
plot(sensor8$Date, sensor8$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 8', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# soil depth 9
SM9=sm9*(sd9-sd6)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM9[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 9', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor9=data.frame(Date=data.101$Date.101, SM=SM9, f1, f2) # data frame with the results for sensor9

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor9$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor9$f1[i]<0 & sensor9$f2[i]>0) {sensor9$ET[i]=abs(sensor9$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr[i]<sd6) 
  {sensor9$ET[i]=NA}
}

sensor9$ET[is.na(sensor9$ET)]=0 # replaces NA values with zero
plot(sensor9$Date, sensor9$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 9', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# soil depth 10
SM10=sm10*(sd10-sd7)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM10[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c()   # 2nd derivative
f2[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}

plot(data.101$Date.101, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for sensor 10', xlab='', ylab='')
lines(data.101$Date.101, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

sensor10=data.frame(Date=data.101$Date.101, SM=SM10, f1, f2) # data frame with the results for sensor10

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor10$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor10$f1[i]<0 & sensor10$f2[i]>0) {sensor10$ET[i]=abs(sensor10$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr[i]<sd7) 
  {sensor10$ET[i]=NA}
}

sensor10$ET[is.na(sensor10$ET)]=0 # replaces NA values with zero
plot(sensor10$Date, sensor10$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 10', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# Total ET for the left side of the onion bed
ET.F.L=sensor1$ET+
       sensor4$ET+
       sensor7$ET+
       sensor10$ET
plot(data.101$Date.101, ET.F.L, 
     type='h', lwd=2,
     main='Daily ET - left side', xlab='', ylab='ET (mm)',
     ylim=c(0, 12))

# Heatmap
x=data.101$Date.101
y=paste('sensor', c(10,7,4,1))
ET=c(sensor10$ET, sensor7$ET, sensor4$ET, sensor1$ET)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - left side') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Total ET for the right side of the onion bed
ET.F.R=sensor2$ET+
  sensor5$ET+
  sensor8$ET+
  sensor10$ET
plot(data.101$Date.101, ET.F.R, 
     type='h', lwd=2,
     main='Daily ET - right side', xlab='', ylab='ET (mm)',
     ylim=c(0,12))

# Heatmap
x=data.101$Date.101
y=paste('sensor', c(10,8,5,2))
ET=c(sensor10$ET, sensor8$ET, sensor5$ET, sensor2$ET)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - right side') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Total ET for the furrow
ET.F.F=sensor3$ET+
  sensor6$ET+
  sensor9$ET+
  sensor10$ET
plot(data.101$Date.101, ET.F.F, 
     type='h', lwd=2,
     main='Daily ET - furrow', xlab='', ylab='ET (mm)',
     ylim=c(0,12))

# Heatmap
x=data.101$Date.101
y=paste('sensor', c(10,9,6,3))
ET=c(sensor10$ET, sensor9$ET, sensor6$ET, sensor3$ET)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - furrow') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Comparison of the methods ####

# Left side of the bed
method=rep(c('A','B', 'C','D','E', 'F'), each=n)
ET.L=c(ET.A.L, ET.B.L, ET.C.L, ET.D.L, ET.E.L, ET.F.L) # Evapotranspiration
comparison.L=data.frame(date=data.101$Date.101, method, ET.L)

# Box-plot for ET
ggplot(comparison.L, aes(x = method, y = ET.L, fill = date)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Daily ET method comparison - left side") +
  xlab("Methods")+ylab("ET (mm)") + 
  geom_boxplot(color="black", fill="darkslategray3")

# Right side of the bed
method=rep(c('A','B', 'C','D','E', 'F'), each=n)
ET.R=c(ET.A.R, ET.B.R, ET.C.R, ET.D.R, ET.E.R, ET.F.R) # Evapotranspiration
comparison.R=data.frame(date=data.101$Date.101, method, ET.R)

# Box-plot for ET
ggplot(comparison.R, aes(x = method, y = ET.R, fill = date)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Daily ET method comparison - right side") +
  xlab("Methods")+ylab("ET (mm)") + 
  geom_boxplot(color="black", fill="darkslategray3")

# Furrow
method=rep(c('A','B', 'C','D','E', 'F'), each=n)
ET.F=c(ET.A.F, ET.B.F, ET.C.F, ET.D.F, ET.E.F, ET.F.F) # Evapotranspiration
comparison.F=data.frame(date=data.101$Date.101, method, ET.F)

# Box-plot for ET
ggplot(comparison.F, aes(x = method, y = ET.F, fill = date)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Daily ET method comparison - furrow") +
  xlab("Methods")+ylab("ET (mm)") + 
  geom_boxplot(color="black", fill="darkslategray3")


# Save the results ####
Results.L=data.frame(Date=data.101$Date.101, ETo, ET.A.L, ET.B.L, ET.C.L, ET.D.L, ET.E.L, ET.F.L)
write_xlsx(Results.L, path="C:/Users/graduate/Box/DiviningWater/WestWeber2019/Soil sensor data/Sensor 101/Results.L.xlsx") 

Results.R=data.frame(Date=data.101$Date.101, ETo, ET.A.R, ET.B.R, ET.C.R, ET.D.R, ET.E.R, ET.F.R)
write_xlsx(Results.R, path="C:/Users/graduate/Box/DiviningWater/WestWeber2019/Soil sensor data/Sensor 101/Results.R.xlsx") 

Results.F=data.frame(Date=data.101$Date.101, ETo, ET.A.F, ET.B.F, ET.C.F, ET.D.F, ET.E.F, ET.F.F)
write_xlsx(Results.F, path="C:/Users/graduate/Box/DiviningWater/WestWeber2019/Soil sensor data/Sensor 101/Results.F.xlsx") 
