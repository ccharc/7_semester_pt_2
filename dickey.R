library(forecast)
library(tseries)
library(MultipleBubbles)



T = 200 #Observationer
N = 500 #Gentagelser
tstata = c()
yta=c()
tstat1a = c()

for(j in 1:N){
ya = numeric(T+50)
ya[1] = 0
aa = 1


for(i in 2:length(ya)){
  ya[i] <- aa * ya[i - 1] + rnorm(1)
}
yta = ya[51:150]


adfa = adf.test(yta, k =0)
tstata = print(adfa$statistic)
tstat1a[j] = tstata 
}


adf_dens = density(tstat1a)

plot(adf_dens, main = "", xlab = "")





###SADF Density
T = 900 #Observationer
N = 1000#Gentagelser
tstat = c()
yt=c()
tstat1 = c()

for(j in 1:N){
  y = numeric(T+50)
  y[1] = 0
  a = 1
  eta = 1
  theta=1
  
  
  for(i in 2:length(y)){
    y[i] <-  T^(-eta) + theta* y[i - 1] + rnorm(1)
  }
  yt = y[51:950]


r0=30
ts_l = length(yt)
rw_vecf =seq(from=900-r0, to= 0, by = -r0)
for (rw in rw_vecf ){
  r1 = 1
  r2 = ts_l- rw
  ts_yt_intf = yt[r1:r2]
  sadff1f= radf(ts_yt_intf, lag=1)
  tstat= print(sadff1f$adf)
  tstat1[j] = max(tstat)
}


}



#dens_sadf = density(tstat1)
plot(dens_sadf, col = "blue" , ylim = range(0:1), xlim=range(-5:5), main = "", xlab = "")
lines(sadf_dens, add=T, col = "red")



quantile(dens_sadf$x, probs = c(0.90, 0.95, 0.99))

quantile(sadf_dens$x, probs = c(0.90, 0.95, 0.99))
#1.1848	1.4738	2.0339

#1.1136	1.4107	1.9754

#1.397630 1.716931 1.972371 

adf_dens = density(tstat1a)
gsadff = gsadf(500,200, adflag = 1, mflag = 1, swindow0 = 20)
sadff = sadf(500,200)

plot(adf_dens, col = "blue" , ylim = range(0:1), xlim=range(-5:5), main = "", xlab = "")
lines(sadf_dens, add=T, col = "red")
lines(gsadf_dens, add=T, col ="green")
legend("topright", legend = c("ADF", "SADF", "GSADF", "Our SADF"),  text.col = c("blue","red","green", "black" ) )




