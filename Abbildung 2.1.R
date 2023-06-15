#Plot mit Verhältnis MSE zum lokal linearen Schätzer und Ndaraya-Watson-Schätzer 
#in Richtung des Randes



install.packages("tidyverse")
install.packages("gridExtra")
library(tidyverse)
library(gridExtra)


source("function.R")
source("VWLBA_V2.R")

alpha<-seq(0,1,1/20)

h<-0.2

plot_estimate <-rep(0,length(h))
plot_variance <-rep(0,length(h))
plot_MSE <-rep(0,length(h)) 

for (i in 1:length(alpha)) 
{
  x0 <-alpha[i]*h
  
  plot_estimate[i]<-bias_estimate(estimations(x,y_sin,kernel="epa",h,x0,degree=1),x0)/bias_estimate(estimations(x,y_sin,kernel="epa",h,x0,degree=0),x0)
  plot_variance[i]<-variance_estimate(estimations(x,y_sin,kernel="epa",h,x0,degree=1))/variance_estimate(estimations(x,y_sin,kernel="epa",h,x0,degree=0))
  plot_MSE[i]<- (bias_estimate(estimations(x,y_sin,kernel="epa",h,x0,degree=1),x0)^2+variance_estimate(estimations(x,y_sin,kernel="epa",h,x0,degree=1)))/(bias_estimate(estimations(x,y_sin,kernel="epa",h,x0,degree=0),x0)^2+variance_estimate(estimations(x,y_sin,kernel="epa",h,x0,degree=0)))
}

data_estimate<-data.frame(alpha=alpha, Bias=plot_estimate)
data_variance<-data.frame(alpha=alpha, Variance=plot_variance)
data_MSE<-data.frame(alpha=alpha, MSE=plot_MSE)

require(gridExtra)
plot1<-ggplot(data = data_estimate,aes(x=alpha,y=Bias))+geom_line()+labs(x=expression(alpha))
plot2<-ggplot(data = data_variance,aes(x=alpha,y=Variance))+geom_line()+labs(x=expression(alpha))
plot3<-ggplot(data = data_MSE,aes(x=alpha,y=MSE))+geom_line()+labs(x=expression(alpha))

grid.arrange(plot1,plot2,plot3)