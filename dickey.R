library(forecast)
library(tseries)
install.packages ("moments")
library (moments)
install.packages("tidyverse")
library(tidyverse)



T = 100 #Observationer
N = 10000 #Gentagelser
tstat = c()
yt=c()
tstat1 = c()

for(j in 1:N){
y = numeric(T+50)
y[1] = 0
a = 1


for(i in 2:length(y)){
  y[i] <- a * y[i - 1] + rnorm(1)
  yt = y[51:150]
}

adf = adf.test(yt, k =0)
tstat = print(adf$statistic)
tstat1[j] = tstat 
}


adf_dens = density(tstat1)


plot(adf_dens)



