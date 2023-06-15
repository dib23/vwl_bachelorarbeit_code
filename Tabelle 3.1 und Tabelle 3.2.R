source("C:/Users/Danny/OneDrive - uni-bonn.de/PC/Desktop/Uni/VWL Bachelor/VWL Bachelorarbeit/function.R")

#Daten generieren

n<-100 #Anzahl Datenpunkte
x<-seq(from=0,to=1,by=1/n) #äqudistante Punkte im Intervall von [0,1] 

#Definiere Funktionen

#lineare Funktion

y_linear<-function(x){
  random<-rnorm(n+1,0,1)
  return((3*x+2)+2*random)
} 

#Sinus-funktion

y_sin<-function(x){
  random<-rnorm(n+1,0,1)
  return(3*sin(2*pi*x)+2+3*random)
}

#Bias auf Basis des Mittelwertschätzers

bias_estimate<-function(x,x_0){(return(mean(x)-x_0))}

#Varianz mit korrigierten Varianzschätzer

variance_estimate<-function(x){(return(1/(length(x)-1)*sum((x-mean(x))^2)))}


#Auswertung #1

h<- 0.1 #Bandweite
calc<- 0.7 #Punkt der Auswertung

y_linear_calc<-3*calc+2
y_sin_calc<-3*sin(calc*2*pi)+2


#Bias für Box Kern und gleicher Bandweite für beide Funktionen

print(bias_estimate(estimations(x,y_linear,kernel="box",h,calc,degree=0),y_linear_calc))
print(bias_estimate(estimations(x,y_sin,kernel="box",h,calc,degree=0),y_sin_calc))
print(bias_estimate(estimations(x,y_linear,kernel="box",h,calc,degree=1),y_linear_calc))
print(bias_estimate(estimations(x,y_sin,kernel="box",h,calc,degree=1),y_sin_calc))

#Bias für Epanechnikov Kern und gleicher Bandweite für beide Funktionen

print(bias_estimate(estimations(x,y_linear,kernel="epa",h,calc,degree=0),y_linear_calc))
print(bias_estimate(estimations(x,y_sin,kernel="epa",h,calc,degree=0),y_sin_calc))
print(bias_estimate(estimations(x,y_linear,kernel="epa",h,calc,degree=1),y_linear_calc))
print(bias_estimate(estimations(x,y_sin,kernel="epa",h,calc,degree=1),y_sin_calc))

#Varianz für Box Kern und gleicher Bandweite für beide Funktionen

print(variance_estimate(estimations(x,y_linear,kernel="box",h,calc,degree=0)))
print(variance_estimate(estimations(x,y_sin,kernel="box",h,calc,degree=0)))
print(variance_estimate(estimations(x,y_linear,kernel="box",h,calc,degree=1)))
print(variance_estimate(estimations(x,y_sin,kernel="box",h,calc,degree=1)))

#Bias für Epanechnikov Kern und gleicher Bandweite für beide Funktionen

print(variance_estimate(estimations(x,y_linear,kernel="epa",h,calc,degree=0)))
print(variance_estimate(estimations(x,y_sin,kernel="epa",h,calc,degree=0)))
print(variance_estimate(estimations(x,y_linear,kernel="epa",h,calc,degree=1)))
print(variance_estimate(estimations(x,y_sin,kernel="epa",h,calc,degree=1)))

#Auswertung #2

h<-0.3 #Bandweite
calc<-0.4 #Punkt der Auswertung

y_linear_calc<-3*calc+2
y_sin_calc<-3*sin(calc*2*pi)+2

#Bias für Box Kern und gleicher Bandweite für beide Funktionen

print(bias_estimate(estimations(x,y_linear,kernel="box",h,calc,degree=0),y_linear_calc))
print(bias_estimate(estimations(x,y_sin,kernel="box",h,calc,degree=0),y_sin_calc))
print(bias_estimate(estimations(x,y_linear,kernel="box",h,calc,degree=1),y_linear_calc))
print(bias_estimate(estimations(x,y_sin,kernel="box",h,calc,degree=1),y_sin_calc))

#Bias für Epanechnikov Kern und gleicher Bandweite für beide Funktionen

print(bias_estimate(estimations(x,y_linear,kernel="epa",h,calc,degree=0),y_linear_calc))
print(bias_estimate(estimations(x,y_sin,kernel="epa",h,calc,degree=0),y_sin_calc))
print(bias_estimate(estimations(x,y_linear,kernel="epa",h,calc,degree=1),y_linear_calc))
print(bias_estimate(estimations(x,y_sin,kernel="epa",h,calc,degree=1),y_sin_calc))

#Varianz für Box Kern und gleicher Bandweite für beide Funktionen

print(variance_estimate(estimations(x,y_linear,kernel="box",h,calc,degree=0)))
print(variance_estimate(estimations(x,y_sin,kernel="box",h,calc,degree=0)))
print(variance_estimate(estimations(x,y_linear,kernel="box",h,calc,degree=1)))
print(variance_estimate(estimations(x,y_sin,kernel="box",h,calc,degree=1)))

#Bias für Epanechnikov Kern und gleicher Bandweite für beide Funktionen

print(variance_estimate(estimations(x,y_linear,kernel="epa",h,calc,degree=0)))
print(variance_estimate(estimations(x,y_sin,kernel="epa",h,calc,degree=0)))
print(variance_estimate(estimations(x,y_linear,kernel="epa",h,calc,degree=1)))
print(variance_estimate(estimations(x,y_sin,kernel="epa",h,calc,degree=1)))


#Auswertung #3

h<-0.1 #Bandweite
calc<-0.4 #Punkt der Auswertung

y_linear_calc<-3*calc+2
y_sin_calc<-3*sin(calc*2*pi)+2

#Bias für Box Kern und gleicher Bandweite für beide Funktionen

print(bias_estimate(estimations(x,y_linear,kernel="box",h,calc,degree=0),y_linear_calc))
print(bias_estimate(estimations(x,y_sin,kernel="box",h,calc,degree=0),y_sin_calc))
print(bias_estimate(estimations(x,y_linear,kernel="box",h,calc,degree=1),y_linear_calc))
print(bias_estimate(estimations(x,y_sin,kernel="box",h,calc,degree=1),y_sin_calc))

#Bias für Epanechnikov Kern und gleicher Bandweite für beide Funktionen

print(bias_estimate(estimations(x,y_linear,kernel="epa",h,calc,degree=0),y_linear_calc))
print(bias_estimate(estimations(x,y_sin,kernel="epa",h,calc,degree=0),y_sin_calc))
print(bias_estimate(estimations(x,y_linear,kernel="epa",h,calc,degree=1),y_linear_calc))
print(bias_estimate(estimations(x,y_sin,kernel="epa",h,calc,degree=1),y_sin_calc))

#Varianz für Box Kern und gleicher Bandweite für beide Funktionen

print(variance_estimate(estimations(x,y_linear,kernel="box",h,calc,degree=0)))
print(variance_estimate(estimations(x,y_sin,kernel="box",h,calc,degree=0)))
print(variance_estimate(estimations(x,y_linear,kernel="box",h,calc,degree=1)))
print(variance_estimate(estimations(x,y_sin,kernel="box",h,calc,degree=1)))

#Bias für Epanechnikov Kern und gleicher Bandweite für beide Funktionen

print(variance_estimate(estimations(x,y_linear,kernel="epa",h,calc,degree=0)))
print(variance_estimate(estimations(x,y_sin,kernel="epa",h,calc,degree=0)))
print(variance_estimate(estimations(x,y_linear,kernel="epa",h,calc,degree=1)))
print(variance_estimate(estimations(x,y_sin,kernel="epa",h,calc,degree=1)))
