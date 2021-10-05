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
# setwd("C:/User/Graduate/Box/DiviningWater/West Weber 2019/Soil sensor data/Sensor 101") # set the working directory in the folder containing the data file
data=read_excel("DailySM.101.xlsx") # load the SM data from excel
data=na.omit(data) # eliminate the days with no data

n=length(data$Date) # Number of days with data
ETo=data$ETo_mm         # Reference ET (mm)

# Soil moisture - volumetric water content %
sm1=data$sm1.101
sm2=data$sm2.101
sm3=data$sm3.101
sm4=data$sm4.101
sm5=data$sm5.101
sm6=data$sm6.101
sm7=data$sm7.101
sm8=data$sm8.101
sm9=data$sm9.101
sm10=data$sm10.101
# Soil moisture averages
sm7=(sm1*11.5+sm2*11.5+sm3*16)/(11.5+11.5+16)   # 7" depth
sm16=(sm4*11.5+sm5*11.5+sm6*16)/(11.5+11.5+16)  # 16" depth
sm29=(sm7*11.5+sm8*11.5+sm9*16)/(11.5+11.5+16)  # 29" depth
sm43=sm10                                       # 43" depth

# Plot daily soil moisture data
plot (data$Date, sm7, ylim=c(20, 50), type='l', lwd=3, col='coral3', 
      main='Daily soil moisture', xlab='', ylab='Soil water content (%)') 
lines(data$Date, sm16, type='l', lwd=3, col='steelblue3')
lines(data$Date, sm29, type='l', lwd=3, col='gold3')
lines(data$Date, sm43, type='l', lwd=3, col='olivedrab3')
legend('bottom', lty=1, lwd=3, inset=0.02,
       legend=c('7"','16"','29"','43"'), col = c('coral3','steelblue3','gold3','olivedrab3'), ncol=4)

# Root depth (mm)
rd=data$Zr

# Sensor depth (mm)
sd7=7*25.4        # sensor 1, 2 and 3: from surface to 7"
sd16=(7+9)*25.4    # sensor 4, 5 and 6: from 7" to 16"
sd29=(7+9+13)*25.4 # sensor 7, 8 and 9: from 16" to 29"
sd43=(7+9+13+14)*25.4     # sensor 10: from 29" to 43"

# plot root depth with sensor depth
plot(data$Date, -rd, 
     type='h', col='deeppink4', lwd='3', ylim=c(-1200, 0),
     main='Onion root depth, West Weber 2019', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=3)
abline(h=-sd7, col='coral3', lwd=3)
abline(h=-sd16, col='steelblue3', lwd=3)
abline(h=-sd29, col='gold3', lwd=3)
abline(h=-sd43, col='olivedrab3', lwd=3)
legend('bottomleft', inset=0.02, lty=1, lwd=3, legend=c('7"','16"','29"','43"'), col=c('coral3','steelblue3','gold3','olivedrab3'), ncol=1)

# Method A: Water balance on the soil profile as a whole ####
# This method estimates ET (mm/day) by applying the water balance method to the 
# entire soil profile at once.

# Total water content (mm) of the soil profile
wc=(sm7*sd7+sm16*(sd16-sd7)+sm29*(sd29-sd16)+sm43*(sd43-sd29))/100

# ET estimation
ET.A=c()
for(i in 2:n) { 
  ET.A[i]=wc[i-1]-wc[i]}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET.A[i]<0) {ET.A[i]=0}}  

ET.A[ET.A==0]=NA

plot(data$Date, ET.A, 
     type='h', lwd=3, col='deepskyblue3',
     main='ET - Method A', xlab='', ylab='ET (mm)')
lines(data$Date, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)

# Method B: Water balance on each sensors soil depth NOT accounting for root growth ####
# This method estimates ET (mm/day) by applying the water balance to each soil
# layer and then adds them all up after having eliminated all negative 
# contributions. Method B does not account for root growth throughout the season.

# ET from soil depth 7"
ET.B.7=c() 
for(i in 2:n) { 
  ET.B.7[i]=(sm7[i-1]-sm7[i])/100*sd7}
for(i in 2:n) { # eliminate negative ET values
  if (ET.B.7[i]<0) {ET.B.7[i]=0}}         
plot(data$Date, ET.B.7, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 7"', xlab='', ylab='ET (mm)')

# ET from soil depth 16"
ET.B.16=c()
for(i in 2:n) { 
  ET.B.16[i]=(sm16[i-1]-sm16[i])/100*(sd16-sd7)}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET.B.16[i]<0) {ET.B.16[i]=0}}         
plot(data$Date, ET.B.16, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 16"', xlab='', ylab='ET (mm)')

# ET from soil depth 29"
ET.B.29=c()
for(i in 2:n) {
  ET.B.29[i]=(sm29[i-1]-sm29[i])/100*(sd29-sd16) }
for(i in 2:n) { # eliminate negative ET contributions
  if (ET.B.29[i]<0) {ET.B.29[i]=0} }         
plot(data$Date, ET.B.29, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 29"', xlab='', ylab='ET (mm)')

# ET from soil depth 43"
ET.B.43=c()
for(i in 2:n) {
  ET.B.43[i]=(sm43[i-1]-sm43[i])/100*(sd43-sd29) }
for(i in 2:n) { # eliminate negative ET contributions
  if (ET.B.43[i]<0) {ET.B.43[i]=0} }         
plot(data$Date, ET.B.43, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 43"', xlab='', ylab='ET (mm)')

# Total ET 
ET.B=ET.B.7+ET.B.16+ET.B.29+ET.B.43
ET.B[1]=NA # can't calculate ET the first day
ET.B[ET.B==0]=NA

plot(data$Date, ET.B,
     type='h', col='deepskyblue3', lwd=2,
     main='Daily ET - method B', xlab='', ylab='ET (mm)')
lines(data$Date, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'deepskyblue3'), lwd=2, inset=0.02)

# Heatmap
x=data$Date
y=paste('soil depth', c('43"', '29"', '16"', '7"'))
ET=c(ET.B.43, ET.B.29, ET.B.16, ET.B.7)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - method B') +
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

# ET from soil depth 7"
ET.C.7=c()
for(i in 2:n) {
  if (RD[i] < sd7) {
    ET.C.7[i]=(sm7[i-1]-sm7[i])/100*RD[i] }
  else {
    ET.C.7[i]=(sm7[i-1]-sm7[i])/100*sd7}}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET.C.7[i]<0) {ET.C.7[i]=0}}         

plot(data$Date, ET.C.7, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 7"', xlab='', ylab='ET (mm)')

# ET from soil depth 16"
ET.C.16=c()
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET.C.16[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd16) {
    ET.C.16[i]=(sm16[i-1]-sm16[i])/100*(RD[i]-sd7) }
  else {
    ET.C.16[i]=(sm16[i-1]-sm16[i])/100*(sd16-sd7) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET.C.16[i]<0) {ET.C.16[i]=0}}    

plot(data$Date, ET.C.16, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 16"', xlab='', ylab='ET (mm)')

# ET from soil depth 29"
ET.C.29=c()
for(i in 2:n) {
  if (RD[i] <= sd16) {
    ET.C.29[i]=0}
  else if (sd16 < RD[i] & RD[i] <= sd29) {
    ET.C.29[i]=(sm29[i-1]-sm29[i])/100*(RD[i]-sd16) }
  else {
    ET.C.29[i]=(sm29[i-1]-sm29[i])/100*(sd29-sd16) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET.C.29[i]<0) {ET.C.29[i]=0} }   

plot(data$Date, ET.C.29, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 29"', xlab='', ylab='ET (mm)')

# ET from soil depth 43"
ET.C.43=c()
for(i in 2:n) {
  if (RD[i] <= sd29) {
    ET.C.43[i]=0}
  else if (sd29 < RD[i] & RD[i] <= sd29) {
    ET.C.43[i]=(sm43[i-1]-sm43[i])/100*(RD[i]-sd29) }
  else {
    ET.C.43[i]=(sm43[i-1]-sm43[i])/100*(sd43-sd29) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET.C.43[i]<0) {ET.C.43[i]=0} }   

plot(data$Date, ET.C.43, 
     type='h', lwd=2, ylim=c(0, 10),
     main='ET from soil depth 43"', xlab='', ylab='ET (mm)')

# Total ET
ET.C=ET.C.7+ET.C.16+ET.C.29+ET.C.43
ET.C[ET.C==0]=NA

plot(data$Date, ET.C,
     ylim=c(0, 15),
     type='h', col='deepskyblue3', lwd=3,
     main='Daily ET - method C', xlab='', ylab='ET (mm)')
lines(data$Date, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'deepskyblue3'), lwd=3, inset=0.02)

# Heatmap
x=data$Date
y=paste('soil depth', c('43"', '29"', '16"', '7"'))
ET=c(ET.C.43, ET.C.29, ET.C.16, ET.C.7)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - method C') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Method D: Derivatives on the soil profile as a whole ####
# This method estimates ET (mm/day) by analyzing the first and second derivatives
# of the soil moisture plots. When the first derivative is negative and the second
# derivative is positive we are assuming that water is leaving the system only via 
# evapotranspiration at a rate equal to the slope of the soil moisture graph.

w=3 # window used to calculate the derivatives in days

# Total water content (mm) of the soil profile
wc=(sm7*sd7+sm16*(sd16-sd7)+sm29*(sd29-sd16)+sm43*(sd43-sd29))/100

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc[low_val:hig_val]
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
plot(data$Date, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives', xlab='', ylab='')
lines(data$Date, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

method_D=data.frame(Date=data$Date, SM=wc, f1, f2) # data frame with the results for the method_D side

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
method_D$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (method_D$f1[i]<0 & method_D$f2[i]>0) {method_D$ET[i]=abs(method_D$f1[i])} 
}

plot(method_D$Date, method_D$ET,
     type='h', lwd=3, col='deepskyblue3', ylim=c(0, 12),
     main='Daily ET - method D', xlab='', ylab='ET (mm)')
lines(data$Date, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'deepskyblue3'), lwd=2, inset=0.02)

ET.D=method_D$ET

# Method E: Derivatives NOT accounting for root growth####
# Same as method D but on each soil layer individually and then summed up and not accounting for
# root growth over the season.

w=3 # window used to calculate the derivatives in days

# soil depth 7"
wc7=sm7*sd7/100 # water content (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc7[low_val:hig_val]
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

plot(data$Date, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 7"', xlab='', ylab='')
lines(data$Date, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

method_E_7=data.frame(Date=data$Date, SM=sm7, f1, f2) # data frame with the results for method_E_7

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
method_E_7$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (method_E_7$f1[i]<0 & method_E_7$f2[i]>0) {method_E_7$ET[i]=abs(method_E_7$f1[i])} 
}
method_E_7$ET[is.na(method_E_7$ET)]=0 # replaces NA values with zero
plot(method_E_7$Date, method_E_7$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 7"', xlab='', ylab='ET (mm)',
     ylim=c(0,10))

# soil depth 16"
wc16=sm16*(sd16-sd7)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc16[low_val:hig_val]
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

plot(data$Date, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 16"', xlab='', ylab='')
lines(data$Date, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

method_E_16=data.frame(Date=data$Date, wc=wc16, f1, f2) # data frame with the results for method_E_16

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
method_E_16$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (method_E_16$f1[i]<0 & method_E_16$f2[i]>0) {method_E_16$ET[i]=abs(method_E_16$f1[i])} 
}
method_E_16$ET[is.na(method_E_16$ET)]=0 # replaces NA values with zero
plot(method_E_16$Date, method_E_16$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 16"', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# soil depth 29"
wc29=sm29*(sd29-sd16)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc29[low_val:hig_val]
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

plot(data$Date, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 29"', xlab='', ylab='')
lines(data$Date, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

method_E_29=data.frame(Date=data$Date, wc=wc29, f1, f2) # data frame with the results for method_E_29

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
method_E_29$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (method_E_29$f1[i]<0 & method_E_29$f2[i]>0) {method_E_29$ET[i]=abs(method_E_29$f1[i])} 
}
method_E_29$ET[is.na(method_E_29$ET)]=0 # replaces NA values with zero
plot(method_E_29$Date, method_E_29$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 29"', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# soil depth 43"
wc43=sm43*(sd43-sd29)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc43[low_val:hig_val]
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

plot(data$Date, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 43', xlab='', ylab='')
lines(data$Date, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

method_E_43=data.frame(Date=data$Date, wc=wc43, f1, f2) # data frame with the results for method_E_43

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
method_E_43$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (method_E_43$f1[i]<0 & method_E_43$f2[i]>0) {method_E_43$ET[i]=abs(method_E_43$f1[i])} 
}
method_E_43$ET[is.na(method_E_43$ET)]=0 # replaces NA values with zero
plot(method_E_43$Date, method_E_43$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 43"', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# Total ET 
ET.E=method_E_7$ET+
     method_E_16$ET+
     method_E_29$ET+
     method_E_43$ET
ET.E[ET.E==0]=NA

plot(data$Date, ET.E, 
     type='h', lwd=3, col='deepskyblue3', ylim=c(0,12),
     main='Daily ET - method E', xlab='', ylab='ET (mm)')
lines(data$Date, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)

# Heatmap
x=data$Date
y=paste('soil depth', c('43"', '29"', '16"', '7"'))
ET=c(method_E_43$ET, method_E_29$ET, method_E_16$ET, method_E_7$ET)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - method E') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Method F: Derivatives accounting for root growth ####
# Same as method F but accounting for root growth over the season.

w=3 # window used to calculate the derivatives in days

# soil depth 1
wc7=sm7*sd7/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc7[low_val:hig_val]
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

plot(data$Date, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 7"', xlab='', ylab='')
lines(data$Date, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

Method_F_7=data.frame(Date=data$Date, wc=wc7, f1, f2) # data frame with the results for Method_F_7

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
Method_F_7$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (Method_F_7$f1[i]<0 & Method_F_7$f2[i]>0) {Method_F_7$ET[i]=abs(Method_F_7$f1[i])} 
}
Method_F_7$ET[is.na(Method_F_7$ET)]=0 # replaces NA values with zero
plot(Method_F_7$Date, Method_F_7$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 7"', xlab='', ylab='ET (mm)',
     ylim=c(0,10))

# soil depth 16"
wc16=sm16*(sd16-sd7)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc16[low_val:hig_val]
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

plot(data$Date, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 16"', xlab='', ylab='')
lines(data$Date, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

Method_F_16=data.frame(Date=data$Date, wc=wc16, f1, f2) # data frame with the results for Method_F_16

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
Method_F_16$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (Method_F_16$f1[i]<0 & Method_F_16$f2[i]>0) {Method_F_16$ET[i]=abs(Method_F_16$f1[i])} 
}

for (i in 1:n) {
  if (data$Zr[i]<sd7) 
  {Method_F_16$ET[i]=NA}
}

Method_F_16$ET[is.na(Method_F_16$ET)]=0 # replaces NA values with zero
plot(Method_F_16$Date, Method_F_16$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 16"', xlab='', ylab='ET (mm)',
     ylim=c(0, 5))

# soil depth 29"
wc29=wc29*(sd29-sd16)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc29[low_val:hig_val]
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

plot(data$Date, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 29"', xlab='', ylab='')
lines(data$Date, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

Method_F_29=data.frame(Date=data$Date, wc=wc29, f1, f2) # data frame with the results for Method_F_29

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
Method_F_29$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (Method_F_29$f1[i]<0 & Method_F_29$f2[i]>0) {Method_F_29$ET[i]=abs(Method_F_29$f1[i])} 
}

for (i in 1:n) {
  if (data$Zr[i]<sd16) 
  {Method_F_29$ET[i]=NA}
}

Method_F_29$ET[is.na(Method_F_29$ET)]=0 # replaces NA values with zero
plot(Method_F_29$Date, Method_F_29$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 29"', xlab='', ylab='ET (mm)',
     ylim=c(0, 15))

# soil depth 43"
wc43=sm43*(wc43-wc29)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c()   # 1st derivative
f1[1]=NA # Non-computable for the first day

for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc43[low_val:hig_val]
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

plot(data$Date, f1, 
     type='o', pch=19, cex=0.8, lwd=3, col='darkgoldenrod', 
     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
     main='SM derivatives for soil depth 43"', xlab='', ylab='')
lines(data$Date, f2, 
      type='o', pch=19, cex=0.8, lwd=3, col='brown2')
abline(h=0, lwd=3)
legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=3, inset=0.01)

Method_F_43=data.frame(Date=data$Date, wc=wc43, f1, f2) # data frame with the results for Method_F_43

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
Method_F_43$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (Method_F_43$f1[i]<0 & Method_F_43$f2[i]>0) {Method_F_43$ET[i]=abs(Method_F_43$f1[i])} 
}

for (i in 1:n) {
  if (data$Zr[i]<sd29) 
  {Method_F_43$ET[i]=NA}
}

Method_F_43$ET[is.na(Method_F_43$ET)]=0 # replaces NA values with zero
plot(Method_F_43$Date, Method_F_43$ET,
     type='h', lwd=3, col='deepskyblue',
     main='ET - soil depth 43"', xlab='', ylab='ET (mm)',
     ylim=c(0, 15))

# Total ET for the left side of the onion bed
ET.F=Method_F_7$ET+
     Method_F_16$ET+
     Method_F_16$ET+
     Method_F_43$ET
ET.F[ET.F==0]=NA

plot(data$Date, ET.F, 
     type='h', lwd=3, col='deepskyblue3',
     main='Daily ET - method F', xlab='', ylab='ET (mm)',
     ylim=c(0, 12))
lines(data$Date, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)

# Heatmap
x=data$Date
y=paste('soil depth', c('43"', '29"', '16"', '7"'))
ET=c(Method_F_43$ET, Method_F_29$ET, Method_F_16$ET, Method_F_7$ET)
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Daily ET (mm) - method F') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

# Comparison of the methods ####

method=rep(c('A','B', 'C','D','E', 'F'), each=n)
ET=c(ET.A, ET.B, ET.C, ET.D, ET.E, ET.F) # Evapotranspiration
comparison=data.frame(date=data$Date, method, ET)

# Box-plot for ET
ggplot(comparison, aes(x = method, y = ET, fill = date)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Daily ET method comparison") +
  xlab("Methods")+ylab("ET (mm)") + 
  geom_boxplot(color="black", fill="darkslategray3")

# Results ####
Results=data.frame(Date=as.Date(data$Date), Irrigation=data$Irrigation, ETo, ET.A, ET.B, ET.C, ET.D, ET.E, ET.F)
Results[is.na(Results)]=0

# Irrigation days: ET=ETo ####
for (i in 1:n) {
  if (Results$Irrigation[i]==1) 
  {Results$ET.A[i]=Results$ETo[i]
   Results$ET.B[i]=Results$ETo[i]
   Results$ET.C[i]=Results$ETo[i]
   Results$ET.D[i]=Results$ETo[i]
   Results$ET.E[i]=Results$ETo[i]
   Results$ET.F[i]=Results$ETo[i]}
}


# Calculate Kc=ET/ETo ####
Kc.A=Results$ET.A/ETo
Kc.B=Results$ET.B/ETo
Kc.C=Results$ET.C/ETo
Kc.D=Results$ET.D/ETo
Kc.E=Results$ET.E/ETo
Kc.F=Results$ET.F/ETo

Results=data.frame(Date=as.Date(data$Date), Irrigation=data$Irrigation, ETo, 
                   ET.A=Results$ET.A, Kc.A, ET.B=Results$ET.B, Kc.B, ET.C=Results$ET.C, Kc.C,
                   ET.D=Results$ET.D, Kc.D, ET.E=Results$ET.E, Kc.E, ET.F=Results$ET.F, Kc.F)

# Eliminate Kc values greater than 1.3 - FAO 56
for (i in 1:n) {
  if (Results$Kc.A[i] > 1.3) {Results$Kc.A[i]=Results$ET.A[i]=0}
}
for (i in 1:n) { 
  if (Results$Kc.B[i] > 1.3) {Results$Kc.B[i]=Results$ET.B[i]=0}
}
for (i in 1:n) {
  if (Results$Kc.C[i] > 1.3) {Results$Kc.C[i]=Results$ET.C[i]=0}
}
for (i in 1:n) {
  if (Results$Kc.D[i] > 1.3) {Results$Kc.D[i]=Results$ET.D[i]=0}
}
for (i in 1:n) {
  if (Results$Kc.E[i] > 1.3) {Results$Kc.E[i]=Results$ET.E[i]=0}
}
for (i in 1:n) {
  if (Results$Kc.F[i] > 1.3) {Results$Kc.F[i]=Results$ET.F[i]=0}
}

# Save in datasheet ####
Results[4:16==0]=NA
write_xlsx(Results, path="C:/Users/graduate/Box/DiviningWater/WestWeber2019/Soil sensor data/Sensor 101/Results.xlsx") 

