# Lokal linearer Schätzer und Nadaraya-Watson-Schätzer 
# anhand von Daten ausgewertet und geplottet



source("function.R")


library("tidyverse")

data<-read_excel("C:/Users/Danny/Downloads/81000-0002_$F.xlsx") #Daten aus dieser Excel

#Data

bip_preisbereinigt<-c(99.9,96.74,	99.95,	103.41,	104.14,	99.2,	101.68,	103.12,	106.56,	101.59,	102.28,	104.6,	106.68,	103.87,	104.42,	105.67,	109.87,	105.7,	108.67,	109.34,	106.01	,99.23	,107.43	,111.96,	107.32	,108.31	,105.08	,113.48	,114.32,	110.86	,105.14,	112.21	,104.55)

datum<-(1:33)

auswertung<-function(n,h,kernel)
{
# Box function
box_f   <- function(u){ifelse(abs(u)<=1, 1/2, 0)}

# Gaussian function 
gauss_f <-function(u){ifelse(abs(u)<=1, dnorm(u,0,1), 0)}

# Epanechnikov function
epa_f <-function(u){ifelse(abs(u)<=1, 3/4*(1-u^2), 0)}


#Setting
n<-200

# Function to compute Kernel-weights:
w_fun <- function(x0, x, h)
{
  u     <- (x - x0)/h
  if(kernel=="box")
  {
    w_f <- box_f(u=u)
  }
  else if(kernel=="gaussian")
  {
    w_f <-gauss_f(u=u)  
  }
  else if(kernel=="epa")
  {
    w_f <-epa_f(u=u)
  }
  return(w_f)
}

y0_NW<-rep(0,n)
y0_LL<-rep(0,n)


# Compute both estimators for a grid of size n

for (i in (1:n)) 
{
  
 x0<-i/n*length(datum)
  
# Compute the local weights:
  w        <- w_fun(x0 ,datum ,h)
# Data centered around x:
  db_x0    <- data.frame("y"=bip_preisbereinigt, "x"=datum-x0)


# Computing the Nadaraya-Watson estimate (= Fitting a local constant):
NW_estim <- lm(y ~ 1, data=db_x0, weights=w)
# Save the NW estimate
y0_NW[i]    <- coef(NW_estim)


# Computing the Local-Linear estimate (= Fitting a local regression line):
LL_estim <- lm(y ~ x, data=db_x0, weights=w)
# Save the LL-estimate (= Intercept)
y0_LL[i]    <- coef(LL_estim)[1]


}
return(list(y0_NW,y0_LL))
}

#Plot mit Epanechnikov-Kern

kernel<-"epa"
ausw<-(1:n)*33/n




require(gridExtra)

h<-0.9

first<-data.frame(x=ausw,y=unlist(auswertung(n,h,kernel)[1]))
second<-data.frame(x=datum,y=bip_preisbereinigt)
thrid<-data.frame(x=ausw,y=unlist(auswertung(n,h,kernel)[2]))

plot1<-ggplot(NULL,aes(x,y))+geom_line(data = first,col="blue")+geom_point(data=second)+geom_line(data = thrid,col="red")+scale_x_continuous(name="Datum", limits=c(0, 33))+scale_y_continuous(name="BIP" , limits=c( min(bip_preisbereinigt),max(bip_preisbereinigt)))

h<-1.5


first<-data.frame(x=ausw,y=unlist(auswertung(n,h,kernel)[1]))
second<-data.frame(x=datum,y=bip_preisbereinigt)
thrid<-data.frame(x=ausw,y=unlist(auswertung(n,h,kernel)[2]))

plot2<-ggplot(NULL,aes(x,y))+geom_line(data = first,col="blue")+geom_point(data=second)+geom_line(data = thrid,col="red")+scale_x_continuous(name="Datum", limits=c(0, 33))+scale_y_continuous(name="BIP" , limits=c( min(bip_preisbereinigt),max(bip_preisbereinigt)))

h<-3

first<-data.frame(x=ausw,y=unlist(auswertung(n,h,kernel)[1]))
second<-data.frame(x=datum,y=bip_preisbereinigt)
thrid<-data.frame(x=ausw,y=unlist(auswertung(n,h,kernel)[2]))

plot3<-ggplot(NULL,aes(x,y))+geom_line(data = first,col="blue")+geom_point(data=second)+geom_line(data = thrid,col="red")+scale_x_continuous(name="Datum", limits=c(0, 33))+scale_y_continuous(name="BIP" , limits=c( min(bip_preisbereinigt),max(bip_preisbereinigt)))

grid.arrange(plot1,plot2,plot3)


#Plot mit Box-Kern

kernel<-"box"
ausw<-(1:n)*33/n




require(gridExtra)

h<-0.9

first<-data.frame(x=ausw,y=unlist(auswertung(n,h,kernel)[1]))
second<-data.frame(x=datum,y=bip_preisbereinigt)
thrid<-data.frame(x=ausw,y=unlist(auswertung(n,h,kernel)[2]))

plot1<-ggplot(NULL,aes(x,y))+geom_line(data = first,col="blue")+geom_point(data=second)+geom_line(data = thrid,col="red")+scale_x_continuous(name="Datum", limits=c(0, 33))+scale_y_continuous(name="BIP" , limits=c( min(bip_preisbereinigt),max(bip_preisbereinigt)))

h<-1.5


first<-data.frame(x=ausw,y=unlist(auswertung(n,h,kernel)[1]))
second<-data.frame(x=datum,y=bip_preisbereinigt)
thrid<-data.frame(x=ausw,y=unlist(auswertung(n,h,kernel)[2]))

plot2<-ggplot(NULL,aes(x,y))+geom_line(data = first,col="blue")+geom_point(data=second)+geom_line(data = thrid,col="red")+scale_x_continuous(name="Datum", limits=c(0, 33))+scale_y_continuous(name="BIP" , limits=c( min(bip_preisbereinigt),max(bip_preisbereinigt)))

h<-3

first<-data.frame(x=ausw,y=unlist(auswertung(n,h,kernel)[1]))
second<-data.frame(x=datum,y=bip_preisbereinigt)
thrid<-data.frame(x=ausw,y=unlist(auswertung(n,h,kernel)[2]))

plot3<-ggplot(NULL,aes(x,y))+geom_line(data = first,col="blue")+geom_point(data=second)+geom_line(data = thrid,col="red")+scale_x_continuous(name="Datum", limits=c(0, 33))+scale_y_continuous(name="BIP" , limits=c( min(bip_preisbereinigt),max(bip_preisbereinigt)))

grid.arrange(plot1,plot2,plot3)

