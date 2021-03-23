#Scripts introducing the ENVir data
# Despues del analisis de corelacion .
#Vamos a integrar los covariados ambientales AMO en el modelo para la Sardina Sur y el HAKE DEL norte
AMO<-read.csv("AMO.csv",header = TRUE, sep = ";", encoding="UTF-8")#DATOS DEL AMO
Mean<-apply(AMO[,c(2:13)],1, mean)#APlicar la media
ENVi<-as.data.frame(cbind(AMO,Mean))#Datos  de AMO con Media
Database<-read.csv("Sardine.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "pil.27.8c9a"& Database[,1]<"2019")#Datos de la Sardina SUr

ENVlag5<-subset(ENVi,   ENVi[,1]< "2014" & ENVi[,1]>"1972" )#Data con LAG5
ENVlag4<-subset(ENVi,   ENVi[,1]< "2015" & ENVi[,1]>"1973" )#Data con LAG4
ENVlag3<-subset(ENVi,   ENVi[,1]< "2016" & ENVi[,1]>"1974" )#Data con LAG3
ENVlag2<-subset(ENVi,   ENVi[,1]< "2017" & ENVi[,1]>"1975" )#Data con LAG2
ENVlag1<-subset(ENVi,   ENVi[,1]< "2018" & ENVi[,1]>"1976" )#Data con LAG1
ENV<-subset(ENVi,  ENVi[,1] > "1977" & ENVi[,1]< "2019" )#Data sin lag
#Aplicacion de la estandarizacion :
Mean<-ENVlag5$Mean
scaled.dat <- scale(Mean)
S<-scaled.dat
# check that we get mean of 0 and sd of 1
colMeans(scaled.dat)  # faster version of apply(scaled.dat, 2, mean)
apply(scaled.dat, 2, sd)
df <- data.frame(x = z$SSB, y = z$SP,i = S)#Datos con AMO Scaled
df1 <- data.frame(x = z$SSB, y = z$SP)#Datos sin AMO 
df2<- data.frame(x = z$SSB, y = z$SP,i= ENVlag5$Mean)#Datos con AMO no scaled
model <- nls(y~a*x + b*x^2+c*i, start=list(a=1.4, b=-0.0000226,c=1), data=df)
summary(model)
model1 <- nls(y~a*x + b*x^2, start=list(a=1.4, b=-0.0000226), data=df1)
summary(model1)
model2 <- nls(y~a*x + b*x^2+c*i, start=list(a=1.4, b=-0.0000226,c=1), data=df2)
summary(model2)
av <- seq(0, max(z$SSB)*1.5, by=100)
bv <- predict(model, newdata=(list(x=av)))       
bv1 <- predict(model1, newdata=(list(x=av))) 
bv2 <- predict(model2, newdata=(list(x=av)))

xMax <- max(av[max(which(bv>0))+1], max(z$SSB))  # maximun Biomass for plots
a <- seq(length(z$SSB) - 1)
Firstyea<-subset(z, z[,1] == "1982")
Lastyea<-subset(z, z[,1] == "2018")
plot(z$SSB, z$SP, pch=15,  xlab="ssb", ylab="Surplus Production",xlim=c(0, xMax), ylim=c(min(z$SP), max(bv)*2.5),type="b", col="red")
arrows(z$SSB[a], z$SP[a], z$SSB[a + 1], z$SP[a + 1],length = 0.1,col="black", angle = 10) 
points(Firstyea$SSB,Firstyea$SP,pch = 21, cex=2, col="blue",bg="blue",lwd=2)
points(Lastyea$SSB,Lastyea$SP,pch = 24, cex=2, col="blue",bg="blue",lwd=2)
lines(av, bv, col="red", type="l")#AMO SCALED
lines(av, bv1, col="blue", type="l")#Without AMO
lines(av, bv2, col="yellow", type="l")#AMO not scaled
# No sé pero me sale estos gráficos raros así
# y luego lo hice pra la Merluza Norte también 

# Ahora vamos a integrar los covariados ambientales para la Merluza de NORTE
AMO<-read.csv("AMO.csv",header = TRUE, sep = ";", encoding="UTF-8")#DATA AMO
Mean<-apply(AMO[,c(2:13)],1, mean)#Mean calculation
ENVi<-as.data.frame(cbind(AMO,Mean))#DATA with MEAN
Database<-read.csv("Hake.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "hke.27.3a46-8abd"& Database[,1]<"2019")
ENVlag5<-subset(ENVi,   ENVi[,1]< "2014" & ENVi[,1]>"1972" )#Data with LAG5
ENVlag4<-subset(ENVi,   ENVi[,1]< "2015" & ENVi[,1]>"1973" )#Data with LAG4
ENVlag3<-subset(ENVi,   ENVi[,1]< "2016" & ENVi[,1]>"1974" )#Data with LAG3
ENVlag2<-subset(ENVi,   ENVi[,1]< "2017" & ENVi[,1]>"1975" )#Data with LAG2
ENVlag1<-subset(ENVi,   ENVi[,1]< "2018" & ENVi[,1]>"1976" )#Data with LAG1
ENV<-subset(ENVi,   ENVi[,1]< "2019" & ENVi[,1]>"1977" )#Data without Lag
Mean<-ENVlag5$Mean#Data para la media del Lag 5
scaled.dat <- scale(Mean)#Hacer la estandarización de la media 
S<-scaled.dat# check that we get mean of 0 and sd of 1
colMeans(scaled.dat)  # faster version of apply(scaled.dat, 2, mean)
apply(scaled.dat, 2, sd)
df <- data.frame(x = z$SSB, y = z$SP,i = S)#x=ssb,y=SP,i=Scaled data
df1 <- data.frame(x = z$SSB, y = z$SP)#Datos sin variables de AMO
df2 <- data.frame(x = z$SSB, y = z$SP,i = ENVlag5$Mean)#Datos con variables de AMO pero no Estandaricados   
# Fit models
model <- nls(y~a*x + b*x^2+ c*i, start=list(a=1.4, b=-0.0000226,c=1), data=df)
summary(model)
model1 <- nls(y~a*x + b*x^2, start=list(a=1.4, b=-0.0000226), data=df1)
summary(model1)
model2 <- nls(y~a*x + b*x^2+c*i, start=list(a=1.4, b=-0.0000226,c=1), data=df2)
summary(model2)
# Predict figures
av <- seq(0, max(z$SSB)*1.5, by=100)
bv <- predict(model, newdata=(list(x=av))) 
bv1 <- predict(model1, newdata=(list(x=av))) 
bv2 <- predict(model2, newdata=(list(x=av))) 
# Plot
xMax <- max(av[max(which(bv>0))+1], max(z$SSB))  # maximun Biomass for plots
a <- seq(length(z$SSB) - 1)
Firstyea<-subset(z, z[,1] == "1978")
Lastyea<-subset(z, z[,1] == "2018")
plot(z$SSB, z$SP, pch=15,  xlab="ssb", ylab="Surplus Production",xlim=c(0, xMax), ylim=c(0, max(bv)*1.2),type="b", col="red")
arrows(z$SSB[a], z$SP[a], z$SSB[a + 1], z$SP[a + 1],length = 0.1,col="black", angle = 10) 
points(Firstyea$SSB,Firstyea$SP,pch = 21, cex=2, col="blue",bg="blue",lwd=2)
points(Lastyea$SSB,Lastyea$SP,pch = 24, cex=2, col="blue",bg="blue",lwd=2)

lines(av, bv, col="red", type="l")#Model with AMO sclaed
lines(av, bv1, col="blue", type="l")#Model sin amo
lines(av, bv2, col="yellow", type="l")#Model with AMO not Scaled


#Analyse de corrélation 
Database<-read.csv("Hake.csv",header = TRUE, sep = ";", encoding="UTF-8")
ENVi<-read.csv("NAO.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "hke.27.8c9a"& Database[,1]<"2019" )
ENVlag5<-subset(ENVi,   ENVi[,1]< "2014" & ENVi[,1]>"1976" )
ENVlag4<-subset(ENVi,   ENVi[,1]< "2015" & ENVi[,1]>"1977" )
ENVlag3<-subset(ENVi,   ENVi[,1]< "2016" & ENVi[,1]>"1978" )
ENVlag2<-subset(ENVi,   ENVi[,1]< "2017" & ENVi[,1]>"1979" )
ENVlag1<-subset(ENVi,   ENVi[,1]< "2018" & ENVi[,1]>"1980" )
ENV<-subset(ENVi,  ENVi[,1] > "1981" & ENVi[,1]< "2019" )
SPE<-1.271e+00*z$SSB+-2.068e-05*z$SSB^2

z<-as.data.frame(cbind(z,SPE))

df0<- data.frame(  SPR=z$SP-z$SPE) 
df <- data.frame( SP = z$SP,SSB = z$SSB,SPR=df0$SPR,NAO5 = ENVlag5$Mean,NAO4 = ENVlag4$Mean, NAO3 = ENVlag3$Mean,NAO2 = ENVlag2$Mean,NAO1 = ENVlag1$Mean,NAO=  ENV$Mean)

cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$Mean, 
                method = "pearson")
res
# pour le Hake Norte 
Database<-read.csv("Hake.csv",header = TRUE, sep = ";", encoding="UTF-8")
ENVi<-read.csv("NAO.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "hke.27.3a46-8abd"& Database[,1]<"2019")
ENVlag5<-subset(ENVi,   ENVi[,1]< "2014" & ENVi[,1]>"1972" )
ENVlag4<-subset(ENVi,   ENVi[,1]< "2015" & ENVi[,1]>"1973" )
ENVlag3<-subset(ENVi,   ENVi[,1]< "2016" & ENVi[,1]>"1974" )
ENVlag2<-subset(ENVi,   ENVi[,1]< "2017" & ENVi[,1]>"1975" )
ENVlag1<-subset(ENVi,   ENVi[,1]< "2018" & ENVi[,1]>"1976" )
ENV<-subset(ENVi,   ENVi[,1]< "2019" & ENVi[,1]>"1977" )
SPE<-1.252e+00*z$SSB+-3.039e-06*z$SSB^2

z<-as.data.frame(cbind(z,SPE))

df0<- data.frame(  SPR=z$SP-z$SPE) 
df <- data.frame( SP = z$SP,SSB = z$SSB,SPE=df0$SPR,NAO5 = ENVlag5$Mean,NAO4 = ENVlag4$Mean, NAO3 = ENVlag3$Mean,NAO2 = ENVlag2$Mean,NAO1 = ENVlag1$Mean,NAO=  ENV$Mean)
cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$Mean, 
                method = "pearson")
res

# ici on voit bien que le p value nous a donné des valeurs superieurs a 0,1 tout les deux donc on voit que pour la Nao il n'ya pas de correlation entre la Nao et la SP pour le Merlue
et donc maintenant on va voir pour l anglerfish  .

Database<-read.csv("Anglerfish.csv",header = TRUE, sep = ";", encoding="UTF-8")
ENVi<-read.csv("NAO.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "mon.27.8c9a"& Database[,1]<"2019")
ENVlag5<-subset(ENVi,   ENVi[,1]< "2014" & ENVi[,1]>"1974" )
ENVlag4<-subset(ENVi,   ENVi[,1]< "2015" & ENVi[,1]>"1975" )
ENVlag3<-subset(ENVi,   ENVi[,1]< "2016" & ENVi[,1]>"1976" )
ENVlag2<-subset(ENVi,   ENVi[,1]< "2017" & ENVi[,1]>"1977" )
ENVlag1<-subset(ENVi,   ENVi[,1]< "2018" & ENVi[,1]>"1978" )
ENV<-subset(ENVi,  ENVi[,1] > "1979" & ENVi[,1]< "2019" )
SPE<-9.686e-01*z$SSB+-6.004e-05*z$SSB^2

z<-as.data.frame(cbind(z,SPE))

df0<- data.frame(  SPR=z$SP-z$SPE) 
df <- data.frame( SP = z$SP,SSB = z$SSB,SPR=df0$SPR,NAO5 = ENVlag5$Mean,NAO4 = ENVlag4$Mean, NAO3 = ENVlag3$Mean,NAO2 = ENVlag2$Mean,NAO1 = ENVlag1$Mean,NAO=  ENV$Mean)


cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$Mean, 
                method = "pearson")
res

# pour l'anglerfish Nord
Database<-read.csv("Anglerfish.csv",header = TRUE, sep = ";", encoding="UTF-8")
ENVi<-read.csv("NAO.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "mon.27.78abd"& Database[,1]<"2019")

ENVlag5<-subset(ENVi,   ENVi[,1]< "2014" & ENVi[,1]>"1980" )
ENVlag4<-subset(ENVi,   ENVi[,1]< "2015" & ENVi[,1]>"1981" )
ENVlag3<-subset(ENVi,   ENVi[,1]< "2016" & ENVi[,1]>"1982" )
ENVlag2<-subset(ENVi,   ENVi[,1]< "2017" & ENVi[,1]>"1983" )
ENVlag1<-subset(ENVi,   ENVi[,1]< "2018" & ENVi[,1]>"1984" )
ENV<-subset(ENVi,  ENVi[,1] > "1985" & ENVi[,1]< "2019" )
SPE<-1.242e+00*z$SSB+-1.339e-05*z$SSB^2

z<-as.data.frame(cbind(z,SPE))

df0<- data.frame(  SPR=z$SP-z$SPE) 
df <- data.frame( SP = z$SP,SSB = z$SSB,SPR=df0$SPR,NAO5 = ENVlag5$Mean,NAO4 = ENVlag4$Mean, NAO3 = ENVlag3$Mean,NAO2 = ENVlag2$Mean,NAO1 = ENVlag1$Mean,NAO=  ENV$Mean)


cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$Mean, 
                method = "pearson")
res
# pour l'anglerfish non plus . on va essayer les autres Espéces .
Database<-read.csv("Sardine.csv",header = TRUE, sep = ";", encoding="UTF-8")
ENVi<-read.csv("NAO.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "pil.27.8c9a"& Database[,1]<"2019")

ENVlag5<-subset(ENVi,   ENVi[,1]< "2014" & ENVi[,1]>"1972" )
ENVlag4<-subset(ENVi,   ENVi[,1]< "2015" & ENVi[,1]>"1973" )
ENVlag3<-subset(ENVi,   ENVi[,1]< "2016" & ENVi[,1]>"1974" )
ENVlag2<-subset(ENVi,   ENVi[,1]< "2017" & ENVi[,1]>"1975" )
ENVlag1<-subset(ENVi,   ENVi[,1]< "2018" & ENVi[,1]>"1976" )
ENV<-subset(ENVi,  ENVi[,1] > "1977" & ENVi[,1]< "2019" )
SPE<-4.513e-01*z$SSB+-3.513e-07*z$SSB^2

z<-as.data.frame(cbind(z,SPE))

df0<- data.frame(  SPR=z$SP-z$SPE) 
df <- data.frame( SP = z$SP,SSB = z$SSB,SPR=df0$SPR,NAO5 = ENVlag5$Mean,NAO4 = ENVlag4$Mean, NAO3 = ENVlag3$Mean,NAO2 = ENVlag2$Mean,NAO1 = ENVlag1$Mean,NAO=  ENV$Mean)
cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$Mean, 
                method = "pearson")
res

Database<-read.csv("Sardine.csv",header = TRUE, sep = ";", encoding="UTF-8")
ENVi<-read.csv("NAO.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "pil.27.8abd"& Database[,1]<"2019")
ENVlag5<-subset(ENVi,   ENVi[,1]< "2014" & ENVi[,1]>"1994" )
ENVlag4<-subset(ENVi,   ENVi[,1]< "2015" & ENVi[,1]>"1995" )
ENVlag3<-subset(ENVi,   ENVi[,1]< "2016" & ENVi[,1]>"1996" )
ENVlag2<-subset(ENVi,   ENVi[,1]< "2017" & ENVi[,1]>"1997" )
ENVlag1<-subset(ENVi,   ENVi[,1]< "2018" & ENVi[,1]>"1998" )
ENV<-subset(ENVi,  ENVi[,1] > "1999" & ENVi[,1]< "2019" )
SPE<-7.669e-01 *z$SSB+-4.373e-06*z$SSB^2

z<-as.data.frame(cbind(z,SPE))

df0<- data.frame(  SPR=z$SP-z$SPE) 
df <- data.frame( SP = z$SP,SSB = z$SSB,SPR=df0$SPR,NAO5 = ENVlag5$Mean,NAO4 = ENVlag4$Mean, NAO3 = ENVlag3$Mean,NAO2 = ENVlag2$Mean,NAO1 = ENVlag1$Mean,NAO=  ENV$Mean)
cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$Mean, 
                method = "pearson")
res

# pour la sardine on a eu une p value plus faible mais toujours plus grande que 0,1
Database<-read.csv("Megrim3.csv",header = TRUE, sep = ";", encoding="UTF-8")
ENVi<-read.csv("NAO.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "meg.27.8c9a"& Database[,1]<"2019")
ENVlag5<-subset(ENVi,   ENVi[,1]< "2014" & ENVi[,1]>"1980" )
ENVlag4<-subset(ENVi,   ENVi[,1]< "2015" & ENVi[,1]>"1981" )
ENVlag3<-subset(ENVi,   ENVi[,1]< "2016" & ENVi[,1]>"1982" )
ENVlag2<-subset(ENVi,   ENVi[,1]< "2017" & ENVi[,1]>"1983" )
ENVlag1<-subset(ENVi,   ENVi[,1]< "2018" & ENVi[,1]>"1984" )
ENV<-subset(ENVi,  ENVi[,1] > "1985" & ENVi[,1]< "2019" )
SPE<-3.084e-01 *z$SSB+-5.863e-06*z$SSB^2

z<-as.data.frame(cbind(z,SPE))

df0<- data.frame(  SPR=z$SP-z$SPE) 
df <- data.frame( SP = z$SP,SSB = z$SSB,SPR=df0$SPR,NAO5 = ENVlag5$Mean,NAO4 = ENVlag4$Mean, NAO3 = ENVlag3$Mean,NAO2 = ENVlag2$Mean,NAO1 = ENVlag1$Mean,NAO=  ENV$Mean)
cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$Mean, 
                method = "pearson")
res

Database<-read.csv("Megrim3.csv",header = TRUE, sep = ";", encoding="UTF-8")
ENVi<-read.csv("NAO.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "meg.27.7b-k8abd"& Database[,1]<"2019")
ENVlag5<-subset(ENVi,   ENVi[,1]< "2014" & ENVi[,1]>"1978" )
ENVlag4<-subset(ENVi,   ENVi[,1]< "2015" & ENVi[,1]>"1979" )
ENVlag3<-subset(ENVi,   ENVi[,1]< "2016" & ENVi[,1]>"1980" )
ENVlag2<-subset(ENVi,   ENVi[,1]< "2017" & ENVi[,1]>"1981" )
ENVlag1<-subset(ENVi,   ENVi[,1]< "2018" & ENVi[,1]>"1982" )
ENV<-subset(ENVi,  ENVi[,1] > "1983" & ENVi[,1]< "2019" )
SPE<-6.102e-01 *z$SSB+-4.731e-06*z$SSB^2

z<-as.data.frame(cbind(z,SPE))

df0<- data.frame(  SPR=z$SP-z$SPE) 
df <- data.frame( SP = z$SP,SSB = z$SSB,SPR=df0$SPR,NAO5 = ENVlag5$Mean,NAO4 = ENVlag4$Mean, NAO3 = ENVlag3$Mean,NAO2 = ENVlag2$Mean,NAO1 = ENVlag1$Mean,NAO=  ENV$Mean)
cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$Mean, 
                method = "pearson")
res
# Maintenant pour la AMO 
AMO<-read.csv("AMO.csv",header = TRUE, sep = ";", encoding="UTF-8")
Mean<-apply(AMO[,c(2:13)],1, mean)
ENVi<-as.data.frame(cbind(AMO,Mean))
Database<-read.csv("Hake.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "hke.27.8c9a"& Database[,1]<"2019" )
ENVlag5<-subset(ENVi,   ENVi[,1]< "2014" & ENVi[,1]>"1976" )
ENVlag4<-subset(ENVi,   ENVi[,1]< "2015" & ENVi[,1]>"1977" )
ENVlag3<-subset(ENVi,   ENVi[,1]< "2016" & ENVi[,1]>"1978" )
ENVlag2<-subset(ENVi,   ENVi[,1]< "2017" & ENVi[,1]>"1979" )
ENVlag1<-subset(ENVi,   ENVi[,1]< "2018" & ENVi[,1]>"1980" )
ENV<-subset(ENVi,  ENVi[,1] > "1981" & ENVi[,1]< "2019" )
SPE<-1.271e+00*z$SSB+-2.068e-05*z$SSB^2

z<-as.data.frame(cbind(z,SPE))

df0<- data.frame(  SPR=z$SP-z$SPE) 
df <- data.frame( SP = z$SP,SSB = z$SSB,SPR=df0$SPR,AMO5 = ENVlag5$Mean,AMO4 = ENVlag4$Mean, AMO3 = ENVlag3$Mean,AMO2 = ENVlag2$Mean,AMO1 = ENVlag1$Mean,AMO=  ENV$Mean)


cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$mean, 
                method = "pearson")
res
# pour le Hake Norte 
AMO<-read.csv("AMO.csv",header = TRUE, sep = ";", encoding="UTF-8")
Mean<-apply(AMO[,c(2:13)],1, mean)
ENVi<-as.data.frame(cbind(AMO,Mean))
Database<-read.csv("Hake.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "hke.27.3a46-8abd"& Database[,1]<"2019")
ENVlag5<-subset(ENVi,   ENVi[,1]< "2014" & ENVi[,1]>"1972" )
ENVlag4<-subset(ENVi,   ENVi[,1]< "2015" & ENVi[,1]>"1973" )
ENVlag3<-subset(ENVi,   ENVi[,1]< "2016" & ENVi[,1]>"1974" )
ENVlag2<-subset(ENVi,   ENVi[,1]< "2017" & ENVi[,1]>"1975" )
ENVlag1<-subset(ENVi,   ENVi[,1]< "2018" & ENVi[,1]>"1976" )
ENV<-subset(ENVi,   ENVi[,1]< "2019" & ENVi[,1]>"1977" )
SPE<-1.252e+00*z$SSB+-3.039e-06*z$SSB^2

z<-as.data.frame(cbind(z,SPE))

df0<- data.frame(  SPR=z$SP-z$SPE) 
df <- data.frame( SP = z$SP,SSB = z$SSB,SPE=df0$SPR,AMO5 = ENVlag5$Mean,AMO4 = ENVlag4$Mean, AMO3 = ENVlag3$Mean,AMO2 = ENVlag2$Mean,AMO1 = ENVlag1$Mean,AMO=  ENV$Mean)
cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05,title = "Merluza Norte",tl.cex=0.8)
)


# ici on voit bien que le p value nous a donné des valeurs superieurs a 0,1 tout les deux donc on voit que pour la Nao il n'ya pas de correlation entre la Nao et la SP pour le Merlue
et donc maintenant on va voir pour l anglerfish  .
AMO<-read.csv("AMO.csv",header = TRUE, sep = ";", encoding="UTF-8")
Mean<-apply(AMO[,c(2:13)],1, mean)
ENVi<-as.data.frame(cbind(AMO,Mean))
Database<-read.csv("Anglerfish.csv",header = TRUE, sep = ";", encoding="UTF-8")
ENVi<-read.csv("NAO.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "mon.27.8c9a"& Database[,1]<"2019")
ENVlag5<-subset(ENVi,   ENVi[,1]< "2014" & ENVi[,1]>"1974" )
ENVlag4<-subset(ENVi,   ENVi[,1]< "2015" & ENVi[,1]>"1975" )
ENVlag3<-subset(ENVi,   ENVi[,1]< "2016" & ENVi[,1]>"1976" )
ENVlag2<-subset(ENVi,   ENVi[,1]< "2017" & ENVi[,1]>"1977" )
ENVlag1<-subset(ENVi,   ENVi[,1]< "2018" & ENVi[,1]>"1978" )
ENV<-subset(ENVi,  ENVi[,1] > "1979" & ENVi[,1]< "2019" )
SPE<-9.686e-01*z$SSB+-6.004e-05*z$SSB^2

z<-as.data.frame(cbind(z,SPE))

df0<- data.frame(  SPR=z$SP-z$SPE) 
df <- data.frame( SP = z$SP,SSB = z$SSB,SPR=df0$SPR,AMO5 = ENVlag5$Mean,AMO4 = ENVlag4$Mean, AMO3 = ENVlag3$Mean,AMO2 = ENVlag2$Mean,AMO1 = ENVlag1$Mean,AMO=  ENV$Mean)


cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$Mean, 
                method = "pearson")
res

# pour l'anglerfish Nord
AMO<-read.csv("AMO.csv",header = TRUE, sep = ";", encoding="UTF-8")
Mean<-apply(AMO[,c(2:13)],1, mean)
ENVi<-as.data.frame(cbind(AMO,Mean))
Database<-read.csv("Anglerfish.csv",header = TRUE, sep = ";", encoding="UTF-8")
ENVi<-read.csv("NAO.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "mon.27.78abd"& Database[,1]<"2019")

ENVlag5<-subset(ENVi,   ENVi[,1]< "2014" & ENVi[,1]>"1980" )
ENVlag4<-subset(ENVi,   ENVi[,1]< "2015" & ENVi[,1]>"1981" )
ENVlag3<-subset(ENVi,   ENVi[,1]< "2016" & ENVi[,1]>"1982" )
ENVlag2<-subset(ENVi,   ENVi[,1]< "2017" & ENVi[,1]>"1983" )
ENVlag1<-subset(ENVi,   ENVi[,1]< "2018" & ENVi[,1]>"1984" )
ENV<-subset(ENVi,  ENVi[,1] > "1985" & ENVi[,1]< "2019" )
SPE<-1.242e+00*z$SSB+-1.339e-05*z$SSB^2

z<-as.data.frame(cbind(z,SPE))

df0<- data.frame(  SPR=z$SP-z$SPE) 
df <- data.frame( SP = z$SP,SSB = z$SSB,SPR=df0$SPR,AMO5 = ENVlag5$Mean,AMO4 = ENVlag4$Mean, AMO3 = ENVlag3$Mean,AMO2 = ENVlag2$Mean,AMO1 = ENVlag1$Mean,AMO=  ENV$Mean)


cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$Mean, 
                method = "pearson")
res
# pour l'anglerfish non plus . on va essayer les autres Espéces .
AMO<-read.csv("AMO.csv",header = TRUE, sep = ";", encoding="UTF-8")
Mean<-apply(AMO[,c(2:13)],1, mean)
ENVi<-as.data.frame(cbind(AMO,Mean))
Database<-read.csv("Sardine.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "pil.27.8c9a"& Database[,1]<"2019")

ENVlag5<-subset(ENVi,   ENVi[,1]< "2014" & ENVi[,1]>"1972" )
ENVlag4<-subset(ENVi,   ENVi[,1]< "2015" & ENVi[,1]>"1973" )
ENVlag3<-subset(ENVi,   ENVi[,1]< "2016" & ENVi[,1]>"1974" )
ENVlag2<-subset(ENVi,   ENVi[,1]< "2017" & ENVi[,1]>"1975" )
ENVlag1<-subset(ENVi,   ENVi[,1]< "2018" & ENVi[,1]>"1976" )
ENV<-subset(ENVi,  ENVi[,1] > "1977" & ENVi[,1]< "2019" )
SPE<-4.513e-01*z$SSB+-3.513e-07*z$SSB^2

z<-as.data.frame(cbind(z,SPE))

df0<- data.frame(  SPR=z$SP-z$SPE) 
df <- data.frame( SP = z$SP,SSB = z$SSB,SPR=df0$SPR,AMO5 = ENVlag5$Mean,AMO4 = ENVlag4$Mean, AMO3 = ENVlag3$Mean,AMO2 = ENVlag2$Mean,AMO1 = ENVlag1$Mean,AMO=  ENV$Mean)
cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$Mean, 
                method = "pearson")
res
AMO<-read.csv("AMO.csv",header = TRUE, sep = ";", encoding="UTF-8")
Mean<-apply(AMO[,c(2:13)],1, mean)
ENVi<-as.data.frame(cbind(AMO,Mean))
Database<-read.csv("Sardine.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "pil.27.8abd"& Database[,1]<"2019")
ENVlag5<-subset(ENVi,   ENVi[,1]< "2014" & ENVi[,1]>"1994" )
ENVlag4<-subset(ENVi,   ENVi[,1]< "2015" & ENVi[,1]>"1995" )
ENVlag3<-subset(ENVi,   ENVi[,1]< "2016" & ENVi[,1]>"1996" )
ENVlag2<-subset(ENVi,   ENVi[,1]< "2017" & ENVi[,1]>"1997" )
ENVlag1<-subset(ENVi,   ENVi[,1]< "2018" & ENVi[,1]>"1998" )
ENV<-subset(ENVi,  ENVi[,1] > "1999" & ENVi[,1]< "2019" )
SPE<-7.669e-01 *z$SSB+-4.373e-06*z$SSB^2

z<-as.data.frame(cbind(z,SPE))

df0<- data.frame(  SPR=z$SP-z$SPE) 
df <- data.frame( SP = z$SP,SSB = z$SSB,SPR=df0$SPR,AMO5 = ENVlag5$Mean,AMO4 = ENVlag4$Mean, AMO3 = ENVlag3$Mean,AMO2 = ENVlag2$Mean,AMO1 = ENVlag1$Mean,AMO=  ENV$Mean)
cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$Mean, 
                method = "pearson")
res

# pour la sardine on a eu une p value plus faible mais toujours plus grande que 0,1
AMO<-read.csv("AMO.csv",header = TRUE, sep = ";", encoding="UTF-8")
Mean<-apply(AMO[,c(2:13)],1, mean)
ENVi<-as.data.frame(cbind(AMO,Mean))
Database<-read.csv("Megrim3.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "meg.27.8c9a"& Database[,1]<"2019")
ENVlag5<-subset(ENVi,   ENVi[,1]< "2014" & ENVi[,1]>"1980" )
ENVlag4<-subset(ENVi,   ENVi[,1]< "2015" & ENVi[,1]>"1981" )
ENVlag3<-subset(ENVi,   ENVi[,1]< "2016" & ENVi[,1]>"1982" )
ENVlag2<-subset(ENVi,   ENVi[,1]< "2017" & ENVi[,1]>"1983" )
ENVlag1<-subset(ENVi,   ENVi[,1]< "2018" & ENVi[,1]>"1984" )
ENV<-subset(ENVi,  ENVi[,1] > "1985" & ENVi[,1]< "2019" )
SPE<-3.084e-01 *z$SSB+-5.863e-06*z$SSB^2

z<-as.data.frame(cbind(z,SPE))

df0<- data.frame(  SPR=z$SP-z$SPE) 
df <- data.frame( SP = z$SP,SSB = z$SSB,SPR=df0$SPR,AMO5 = ENVlag5$Mean,AMO4 = ENVlag4$Mean, AMO3 = ENVlag3$Mean,AMO2 = ENVlag2$Mean,AMO1 = ENVlag1$Mean,AMO=  ENV$Mean)
cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$Mean, 
                method = "pearson")
res
AMO<-read.csv("AMO.csv",header = TRUE, sep = ";", encoding="UTF-8")
Mean<-apply(AMO[,c(2:13)],1, mean)
ENVi<-as.data.frame(cbind(AMO,Mean))
Database<-read.csv("Megrim3.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "meg.27.7b-k8abd"& Database[,1]<"2019")
ENVlag5<-subset(ENVi,   ENVi[,1]< "2014" & ENVi[,1]>"1978" )
ENVlag4<-subset(ENVi,   ENVi[,1]< "2015" & ENVi[,1]>"1979" )
ENVlag3<-subset(ENVi,   ENVi[,1]< "2016" & ENVi[,1]>"1980" )
ENVlag2<-subset(ENVi,   ENVi[,1]< "2017" & ENVi[,1]>"1981" )
ENVlag1<-subset(ENVi,   ENVi[,1]< "2018" & ENVi[,1]>"1982" )
ENV<-subset(ENVi,  ENVi[,1] > "1983" & ENVi[,1]< "2019" )
SPE<-6.102e-01 *z$SSB+-4.731e-06*z$SSB^2

z<-as.data.frame(cbind(z,SPE))

df0<- data.frame(  SPR=z$SP-z$SPE) 
df <- data.frame( SP = z$SP,SSB = z$SSB,SPR=df0$SPR,AMO5 = ENVlag5$Mean,AMO4 = ENVlag4$Mean, AMO3 = ENVlag3$Mean,AMO2 = ENVlag2$Mean,AMO1 = ENVlag1$Mean,AMO=  ENV$Mean)
cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05,title = "Megrim Norte")
res <- cor.test(z$SP, ENV$Mean, 
                method = "pearson")
res
# Maintenant pour tout les stocks Ensembles 
# Read data
setwd("C:/Users/zanni/Desktop/TFM/TFMYosri")
Database<-read.csv("Hakes.csv",header = TRUE, sep= ";", encoding="UTF-8")
ENVi<-read.csv("NAO.csv",header = TRUE, sep = ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "hke.27.3a46-8abd"& Database[,1]< "2019")
ENV<-subset(ENVi,   ENVi[,2]< "2019"& ENVi[,2]> "1981")
df <- data.frame(x = z$SP, y = ENV$Mean)
cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$Mean, 
                method = "pearson")
res

# Pas de Corrélation 
Database<-read.csv("Anglerfishs.csv",header = TRUE, sep= ";", encoding="UTF-8")
ENVi<-read.csv("NAO.csv",header = TRUE, sep = ";", encoding="UTF-8")

z<-subset(Database, Database[,17] == "mon.27.78abd"&Database[,1]<"2019")
ENV<-subset(ENVi,   ENVi [,2]< "2019"& ENVi[,2]> "1985")
df <- data.frame(x = z$SP, y = ENV$Mean)
cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$Mean, 
                method = "pearson")
res
#Pour l'anglerfish il y'a pas  eu une Corrélation 


Database<-read.csv("Sardines.csv",header = TRUE, sep= ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "pil.27.8abd"&Database[,1]< "2019")

ENV<-subset(ENVi ,   ENVi[,2]< "2019"& ENVi[,2]> "1999")
df <- data.frame(x = z$SP, y = ENV$Mean)
cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$Mean, 
                method = "pearson")
res
# Pas de Correlation pour la Sardine
Database<-read.csv("Megrim3s.csv",header = TRUE, sep= ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "meg.27.8c9a"& Database[,1]< "2019")
ENV<-subset(ENVi,   ENVi[,2]< "2019"& ENVi[,2]> "1985")
df <- data.frame(x = z$SP, y = ENV$Mean)
cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$Mean, 
                method = "pearson")
res
# Pas de Corrélation 

setwd("C:/Users/zanni/Desktop/TFM/TFMYosri")
Database<-read.csv("Hakes.csv",header = TRUE, sep= ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "hke.27.3a46-8abd"& Database[,1]< "2019")
ENV<-subset(AMO,   AMO[,2]< "2019"& AMO[,2]> "1981")
df <- data.frame(x = z$SP, y = ENV$mean)
cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$mean, 
                method = "pearson")
res

# Pas de Corrélation 
Database<-read.csv("Anglerfishs.csv",header = TRUE, sep= ";", encoding="UTF-8")

z<-subset(Database, Database[,17] == "mon.27.78abd"&Database[,1]<"2019")
ENV<-subset(AMO,   AMO[,2]< "2019"& AMO[,2]> "1985")
df <- data.frame(x = z$SP, y = ENV$mean)
cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$mean, 
                method = "pearson")
res
#Pour l'anglerfish il y'a eu une Corrélation 


Database<-read.csv("Sardines.csv",header = TRUE, sep= ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "pil.27.8abd"&Database[,1]< "2019")
ENV<-subset(AMO,   AMO[,2]< "2019"& AMO[,2]> "1999")
df <- data.frame(x = z$SP, y = ENV$mean)
cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$mean, 
                method = "pearson")
res
# Pas de Correlation pour la Sardine
Database<-read.csv("Megrim3s.csv",header = TRUE, sep= ";", encoding="UTF-8")
z<-subset(Database, Database[,17] == "meg.27.8c9a"& Database[,1]< "2019")
ENV<-subset(AMO,   AMO[,2]< "2019"& AMO[,2]> "1985")
df <- data.frame(x = z$SP, y = ENV$mean)
cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(df)



library(corrplot)
corrplot(round(cor(as.matrix(df),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
res <- cor.test(z$SP, ENV$mean, 
                method = "pearson")
res
data<-read.csv("Analisis de cor.csv",header = TRUE, sep= ";", encoding="UTF-8")
data
