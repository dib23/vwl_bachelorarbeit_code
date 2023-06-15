#Simulation der lokal linearen Schätzer und des Nadaray-Watson-Schätzer an einem Punkt


estimations<-function(x,y,kernel,h,x0,degree)
{

# Box function
box_f   <- function(u){ifelse(abs(u)<=1, 1/2, 0)}

# Gaussian function 
gauss_f <-function(u){ifelse(abs(u)<=1, dnorm(u,0,1), 0)}

# Epanechnikov function
epa_f <-function(u){ifelse(abs(u)<=1, 3/4*(1-u^2), 0)}


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

#Monte-Carlo-Simulation mit B Simulationen

B<-10000

y_sim<-rep(0,B)

for (j in 1:B) {


  # Compute the local weights:
  w        <- w_fun(x0 ,x ,h)
  # Data centered around x:
  db_x0    <- data.frame("y"=y(x), "x"=x-x0)
  
  if(degree==0)
  {
  # Computing the Nadaraya-Watson estimate (= Fitting a local constant):
  NW_estim <- lm(y ~ 1, data=db_x0, weights=w)
  # Save the NW estimate
  y0_NW    <- coef(NW_estim)
  #Save for every simulation
  y_sim[j]<-y0_NW
  }
  else if(degree==1)
  {
  # Computing the Local-Linear estimate (= Fitting a local regression line):
  LL_estim <- lm(y ~ x, data=db_x0, weights=w)
  # Save the LL-estimate (= Intercept)
  y0_LL    <- coef(LL_estim)[1]
  #Save for every simulation
  y_sim[j]<-y0_LL
  }

}
  
return(y_sim)

}