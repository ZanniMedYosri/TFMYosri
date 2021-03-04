#Scripts introducing the ENVir data
Database<-read.csv("Hake.csv",header = TRUE, sep = ";", encoding="UTF-8")
ENVi<-read.csv("NAO.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "hke.27.8c9a")
ENV<-subset(ENVi, ENVi[,2] > "1981")
df <- data.frame(x = z$SSB, y = z$SP,i = ENV$Mean)


model <- nls(y~a*x + b*x^2+i, start=list(a=1.4, b=-0.0000226), data=df)
summary(model)

av <- seq(0, max(z$SSB)*1.5, by=100)
bv <- predict(model, newdata=(list(x=av)))       # the mane must be "x", as in the model

xMax <- max(av[max(which(bv>0))+1], max(z$SSB))  # maximun Biomass for plots
a <- seq(length(z$SSB) - 1)
Firstyea<-subset(z, z[,1] == "1978")
Lastyea<-subset(z, z[,1] == "2018")
plot(z$SSB, z$SP, pch=15,  xlab="ssb", ylab="Surplus Production",xlim=c(0, xMax), ylim=c(0, max(bv)*1.2),type="b", col="red")
arrows(z$SSB[a], z$SP[a], z$SSB[a + 1], z$SP[a + 1],length = 0.1,col="black", angle = 10) 
points(Firstyea$SSB,Firstyea$SP,pch = 21, cex=2, col="blue",bg="blue",lwd=2)
points(Lastyea$SSB,Lastyea$SP,pch = 24, cex=2, col="blue",bg="blue",lwd=2)
lines(av, bv, col="red", type="l")
#Now for the ANglerfish

setwd("C:/Users/zanni/Desktop/TFM/TFMYosri")
Database<-read.csv("Anglerfish.csv",header = TRUE, sep= ";", encoding="UTF-8")
ENVi<-read.csv("NAO.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "mon.27.78abd")
ENV<-subset(ENVi, ENVi[,2] > "1985")
df <- data.frame(x = z$SSB, y = z$SP,i = ENV$Mean)
# Fitting the model 
model <- nls(y~a*x + b*x^2+ i, start=list(a=1.4, b=-0.0000226), data=df)
summary(model)
# Predict figures
av <- seq(0, max(z$SSB)*1.5, by=100)
bv <- predict(model, newdata=(list(x=av)))       # the mane must be "x", as in the model
# Plot
xMax <- max(av[max(which(bv>0))+1], max(z$SSB))  # maximun Biomass for plots
a <- seq(length(z$SSB) - 1)
Firstyea<-subset(z, z[,1] == "1986")
Lastyea<-subset(z, z[,1] == "2018")
plot(z$SSB, z$SP, pch=15,  xlab="ssb", ylab="Surplus Production",xlim=c(0, xMax), ylim=c(0, max(bv)*1.5),type="b", col="red")
arrows(z$SSB[a], z$SP[a], z$SSB[a + 1], z$SP[a + 1],length = 0.1,col="black", angle = 10) 
points(Firstyea$SSB,Firstyea$SP,pch = 21, cex=2, col="blue",bg="blue",lwd=2)
points(Lastyea$SSB,Lastyea$SP,pch = 24, cex=2, col="blue",bg="blue",lwd=2)

lines(av, bv, col="red", type="l")
