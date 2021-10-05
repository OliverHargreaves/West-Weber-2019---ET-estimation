# Summary statistics for the West Weber 2019 research site 

# Packages ####
library(readxl)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(multcompView)

# Load data ####
data=read_excel("Results.101.xlsx") # load the SM data from excel
data[is.na(data)]=0

# Plot the results ####

# Method A
plot(data$Date, data$ET.A, 
     type='h', lwd=3, col='darkturquoise',
     main='ET - Method A', xlab='', ylab='ET (mm)',
     ylim=c(0, 12))
lines(data$Date, data$ETo,
      lwd=3, col='darkseagreen3')
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkturquoise'), lwd=3, inset=0.02)

plot(data$Date, data$Kc.A, 
     type='h', lwd=3, col='orchid3',
     main='Kc - Method A', xlab='', ylab='ET/ETo',
     ylim=c(0, 1.3))

# Method B
plot(data$Date, data$ET.B, 
     type='h', lwd=3, col='darkturquoise',
     main='ET - Method B', xlab='', ylab='ET (mm)',
     ylim=c(0, 12))
lines(data$Date, data$ETo,
      lwd=3, col='darkseagreen3')
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkturquoise'), lwd=3, inset=0.02)

plot(data$Date, data$Kc.B, 
     type='h', lwd=3, col='orchid3',
     main='Kc - Method B', xlab='', ylab='ET/ETo',
     ylim=c(0, 1.3))

# Method C
plot(data$Date, data$ET.C, 
     type='h', lwd=3, col='darkturquoise',
     main='ET - Method C', xlab='', ylab='ET (mm)',
     ylim=c(0, 12))
lines(data$Date, data$ETo,
      lwd=3, col='darkseagreen3')
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkturquoise'), lwd=3, inset=0.02)

plot(data$Date, data$Kc.C, 
     type='h', lwd=3, col='orchid3',
     main='Kc - Method C', xlab='', ylab='ET/ETo',
     ylim=c(0, 1.3))

# Method D
plot(data$Date, data$ET.D, 
     type='h', lwd=3, col='darkturquoise',
     main='ET - Method D', xlab='', ylab='ET (mm)',
     ylim=c(0, 12))
lines(data$Date, data$ETo,
      lwd=3, col='darkseagreen3')
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkturquoise'), lwd=3, inset=0.02)

plot(data$Date, data$Kc.D, 
     type='h', lwd=3, col='orchid3',
     main='Kc - Method D', xlab='', ylab='ET/ETo',
     ylim=c(0, 1.3))

# Method E
plot(data$Date, data$ET.E, 
     type='h', lwd=3, col='darkturquoise',
     main='ET - Method E', xlab='', ylab='ET (mm)',
     ylim=c(0, 12))
lines(data$Date, data$ETo,
      lwd=3, col='darkseagreen3')
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkturquoise'), lwd=3, inset=0.02)

plot(data$Date, data$Kc.E, 
     type='h', lwd=3, col='orchid3',
     main='Kc - Method E', xlab='', ylab='ET/ETo',
     ylim=c(0, 1.3))

# Method F
plot(data$Date, data$ET.F, 
     type='h', lwd=3, col='darkturquoise',
     main='ET - Method F', xlab='', ylab='ET (mm)',
     ylim=c(0, 12))
lines(data$Date, data$ETo,
      lwd=3, col='darkseagreen3')
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkturquoise'), lwd=3, inset=0.02)

plot(data$Date, data$Kc.F, 
     type='h', lwd=3, col='orchid3',
     main='Kc - Method F', xlab='', ylab='ET/ETo',
     ylim=c(0, 1.3))

# Cumulative ET time series ####
ET.A.cum=cumsum(data$ET.A)
ET.B.cum=cumsum(data$ET.B)
ET.C.cum=cumsum(data$ET.C)
ET.D.cum=cumsum(data$ET.D)
ET.E.cum=cumsum(data$ET.E)
ET.F.cum=cumsum(data$ET.F)

plot(data$Date, ET.A.cum, type='l', lwd=2, col='aquamarine', main='Cumulative ET', xlab='', ylab='ET (mm)')
lines(data$Date, ET.B.cum, type='l', lwd=2, col='chocolate')
lines(data$Date, ET.C.cum, type='l', lwd=2, col='chartreuse')
lines(data$Date, ET.D.cum, type='l', lwd=2, col='goldenrod')
lines(data$Date, ET.E.cum, type='l', lwd=2, col='brown1')
lines(data$Date, ET.F.cum, type='l', lwd=2, col='deeppink')
legend('topleft', legend=c(paste('Method', LETTERS[seq(from=1, to=6)])), col=c('aquamarine', 'chocolate', 'chartreuse', 'goldenrod', 'brown1', 'deeppink'), lwd=3, inset=0.02)  


# Calculate summary statistics ####
summ=summary(data[4:15])

ETo.sum=sum(data$ETo)
ET.A.sum=sum(data$ET.A)
ET.B.sum=sum(data$ET.B)
ET.C.sum=sum(data$ET.C)
ET.D.sum=sum(data$ET.D)
ET.E.sum=sum(data$ET.E)
ET.F.sum=sum(data$ET.F)
Method=c('A','B','C','D','E','F')
sum.df=data.frame(Method, ET.Season=c(ET.A.sum, ET.B.sum, ET.C.sum, ET.D.sum, ET.E.sum, ET.F.sum))

plot(1:6, sum.df$ET.Season, 
     main='Total ET for the season', xlab='Method', ylab='ET (mm)', ylim=c(0, max(sum.df$ET.Season)), xaxt='n',
     type='h', lwd=8, col='darkturquoise')
axis(1, at=1:6, labels=Method)

abline(h=ETo.sum, col='darkseagreen3', lwd=8)


# Box plots ####
n=length(data$Date)
method=rep(c('A','B', 'C','D','E', 'F'), each=n)
ET=c(data$ET.A, data$ET.B, data$ET.C, data$ET.D, data$ET.E, data$ET.F) # Evapotranspiration
Kc=c(data$Kc.A, data$Kc.B, data$Kc.C, data$Kc.D, data$Kc.E, data$Kc.F) # Evapotranspiration
Left.df=data.frame(date=data$Date, method, ET, Kc)
  
# ET 
ggplot(Left.df, aes(x = method, y = ET, fill = date)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Daily ET method comparison") +
  xlab("Methods")+ylab("ET (mm)") + 
  geom_boxplot(color="black", fill="darkturquoise")

# Kc 
ggplot(Left.df, aes(x = method, y = Kc, fill = date)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Daily Kc method comparison") +
  xlab("Methods")+ylab("ET/ETo") + 
  geom_boxplot(color="black", fill="orchid3")

# Violin plots ####

# ET 
ggplot(Left.df, aes(x = method, y = ET, fill = date)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Daily ET method comparison") +
  xlab("Methods")+ylab("ET (mm)") + 
  geom_violin(color="black", fill="darkturquoise")

# Kc 
ggplot(Left.df, aes(x = method, y = Kc, fill = date)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Daily Kc method comparison") +
  xlab("Methods")+ylab("ET/ETo") + 
  geom_violin(color="black", fill="orchid3")

# ANOVA + Tukey test ####

# ET
model=lm( Left.df$ET ~ Left.df$method )
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'Left.df$method', confevel=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown")

# Kc
model=lm(Left.df$Kc ~ Left.df$method)
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'Left.df$method', confevel=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown")

