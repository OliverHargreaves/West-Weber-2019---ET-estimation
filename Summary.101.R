# Summary statistics for the West Weber 2019 research site 

# Packages ####
library(readxl)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(multcompView)

# Load data ####
data=read_excel("Results.L_Edited.xlsx") # load the SM data from excel
data=data[1:15]

# Plot the results ####

# Method A
plot(data$Date, data$ET.A.L, 
     type='h', lwd=3, col='darkslategray3',
     main='Method A - Left side', xlab='', ylab='ET (mm)',
     ylim=c(0, max(data$ET.A.L)))
lines(data$Date, data$ETo,
      lwd=3, col='darkseagreen3')

plot(data$Date, data$Kc.A.L, 
     type='h', lwd=3, col='orchid3',
     main='Method A - Left side', xlab='', ylab='ET/ETo',
     ylim=c(0, max(data$Kc.A.L)))

# Method B
plot(data$Date, data$ET.B.L, 
     type='h', lwd=3, col='darkslategray3',
     main='Method B - Left side', xlab='', ylab='ET (mm)',
     ylim=c(0, max(data$ET.B.L)))
lines(data$Date, data$ETo,
      lwd=3, col='darkseagreen3')

plot(data$Date, data$Kc.B.L, 
     type='h', lwd=3, col='orchid3',
     main='Method B - Left side', xlab='', ylab='ET/ETo',
     ylim=c(0, max(data$Kc.B.L)))

# Method C
plot(data$Date, data$ET.C.L, 
     type='h', lwd=3, col='darkslategray3',
     main='Method B - Left side', xlab='', ylab='ET (mm)',
     ylim=c(0, max(data$ET.C.L)))
lines(data$Date, data$ETo,
      lwd=3, col='darkseagreen3')

plot(data$Date, data$Kc.C.L, 
     type='h', lwd=3, col='orchid3',
     main='Method C - Left side', xlab='', ylab='ET/ETo',
     ylim=c(0, max(data$Kc.C.L)))

# Method D
plot(data$Date, data$ET.D.L, 
     type='h', lwd=3, col='darkslategray3',
     main='Method D - Left side', xlab='', ylab='ET (mm)',
     ylim=c(0, max(data$ET.D.L)))
lines(data$Date, data$ETo,
      lwd=3, col='darkseagreen3')

plot(data$Date, data$Kc.D.L, 
     type='h', lwd=3, col='orchid3',
     main='Method D - Left side', xlab='', ylab='ET/ETo',
     ylim=c(0, max(data$Kc.D.L)))

# Method E
plot(data$Date, data$ET.E.L, 
     type='h', lwd=3, col='darkslategray3',
     main='Method E - Left side', xlab='', ylab='ET (mm)',
     ylim=c(0, max(data$ET.E.L)))
lines(data$Date, data$ETo,
      lwd=3, col='darkseagreen3')

plot(data$Date, data$Kc.E.L, 
     type='h', lwd=3, col='orchid3',
     main='Method E - Left side', xlab='', ylab='ET/ETo',
     ylim=c(0, max(data$Kc.E.L)))

# Method F
plot(data$Date, data$ET.F.L, 
     type='h', lwd=3, col='darkslategray3',
     main='Method F - Left side', xlab='', ylab='ET (mm)',
     ylim=c(0, max(data$ET.F.L)))
lines(data$Date, data$ETo,
      lwd=3, col='darkseagreen3')

plot(data$Date, data$Kc.F.L, 
     type='h', lwd=3, col='orchid3',
     main='Method F - Left side', xlab='', ylab='ET/ETo',
     ylim=c(0, max(data$Kc.F.L)))

# Calculate summary statistics ####
summ=summary(data[4:15])

ETo.sum=sum(data$ETo)
ET.A.L.sum=sum(data$ET.A.L)
ET.B.L.sum=sum(data$ET.B.L)
ET.C.L.sum=sum(data$ET.C.L)
ET.D.L.sum=sum(data$ET.D.L)
ET.E.L.sum=sum(data$ET.E.L)
ET.F.L.sum=sum(data$ET.F.L)
Method=c('A','B','C','D','E','F')
sum.df=data.frame(Method, ET.Season=c(ET.A.L.sum, ET.B.L.sum, ET.C.L.sum, ET.D.L.sum, ET.E.L.sum, ET.F.L.sum))

plot(1:6, sum.df$ET.Season, 
     main='Total ET for the season', xlab='Method', ylab='ET (mm)', ylim=c(0, max(sum.df$ET.Season)), xaxt='n',
     type='h', lwd=8, col='darkslategray3')
axis(1, at=1:6, labels=Method)

abline(h=ETo.sum, col='darkseagreen3', lwd=8)


# Box plots ####
n=length(data$Date)
method=rep(c('A','B', 'C','D','E', 'F'), each=n)
ET.L=c(data$ET.A.L, data$ET.B.L, data$ET.C.L, data$ET.D.L, data$ET.E.L, data$ET.F.L) # Evapotranspiration
Kc.L=c(data$Kc.A.L, data$Kc.B.L, data$Kc.C.L, data$Kc.D.L, data$Kc.E.L, data$Kc.F.L) # Evapotranspiration
Left.df=data.frame(date=data$Date, method, ET.L, Kc.L)
  
# ET 
ggplot(Left.df, aes(x = method, y = ET.L, fill = date)) +
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

# Kc 
ggplot(Left.df, aes(x = method, y = Kc.L, fill = date)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Daily Kc method comparison - left side") +
  xlab("Methods")+ylab("ET/ETo") + 
  geom_boxplot(color="black", fill="orchid3")

# Violin plots ####

# ET 
ggplot(Left.df, aes(x = method, y = ET.L, fill = date)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Daily ET method comparison - left side") +
  xlab("Methods")+ylab("ET (mm)") + 
  geom_violin(color="black", fill="darkslategray3")

# Kc 
ggplot(Left.df, aes(x = method, y = Kc.L, fill = date)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Daily Kc method comparison - left side") +
  xlab("Methods")+ylab("ET/ETo") + 
  geom_violin(color="black", fill="orchid3")

# ANOVA + Tukey test ####

# ET
model=lm( Left.df$ET.L ~ Left.df$method )
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'Left.df$method', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown")

# Kc
model=lm(Left.df$Kc.L ~ Left.df$method)
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'Left.df$method', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown")

