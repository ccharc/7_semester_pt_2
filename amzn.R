library(readxl)
library(vars)
library(imputeTS)
library(forecast)
library(tseries)
library(stats)
library(strucchange)
library(readr)
library(exuber)
library(shape)
library(forecast)
library(tseries)
library(MultipleBubbles)



amzn <- read_csv("AMZN-2.csv")
head(amzn)

close_amzn = amzn$Close

plot(close_amzn, type="l", ylab = "Aktiepris Amazon", xlab = "Måneder fra 01-01-2005")  

ts_amzn=  as.ts(close_amzn)  
plot(ts_amzn, type="l", ylab = "", xlab = "")


#FORWARD SADF

vecc22 = c()
ts_l = length(ts_amzn)
sadfstat11f = c()
r0 = 30
rw_vecf =seq(from=180-r0, to= 0, by = -r0)
sadfstat11f = c()
for (rw in rw_vecf ){
  r1 = 1
  r2 = ts_l- rw
  ts_amzn_intf = ts_amzn[r1:r2]
  sadff1f= radf(ts_amzn_intf, lag=1)
  sadfstatf = sadff1f$adf
  sadfstat1f = print(sadfstatf)
  sadfstat11f = c(sadfstat11f ,sadfstat1f)
  vecc22 = c(vecc22, r2)
}


fsadf = cbind(sadfstat11f, vecc22)

sadfstat11f
max(sadfstat11f)
which.max(sadfstat11f)
fsadf[which.max(sadfstat11f),]

#SADF

#sadff = sadf(500,240)
#sadf_dens = density(sadff$value)
#plot(sadf_dens)

#sadf = sadf_gsadf(ts_amzn, adflag=1, mflag = 1, IC = 1)


vecc11= c()
ts_l = length(ts_amzn)
sadfstat11 = c()
rw_vec =seq(from=r0, to=180, by = r0)
sadfstat11 = c()
for (rw in rw_vec ){
  r1 =  ts_l - rw
  r2 = ts_l
  ts_amzn_int = ts_amzn[r1:r2]
  sadff1 = radf(ts_amzn_int, lag=1)
  sadfstat = sadff1$adf
  sadfstat1 = print(sadfstat)
  sadfstat11 = c(sadfstat11 ,sadfstat1)
  vecc11 = c(vecc11, r1)
}

bsadf_bind = cbind(sadfstat11, vecc11)


sadfstat11
max(sadfstat11)
which.max(sadfstat11)
bsadf_bind[which.max(sadfstat11),]



#GSADF

#gsadff = gsadf(500,240, adflag = 1, mflag = 1, swindow0 = 30)
#gsadf_dens = density(gsadff$value)
#plot(gsadf_dens)

#gsadf = sadf_gsadf(ts_amzn, adflag=1, mflag = 1, IC = 1)

vecc1 = c()
vecc2 = c()
r1_vec = seq(from =0, to =180-r0, by = r0)
r2_vec = seq(from=180, to= r0, by = -r0)
vec = c()
gsint_vec = c()
vec1 = c()
for (j in 1:length(r1_vec)){
  for (i in 1:length(r2_vec)){
    r1 = r1_vec[j]
    r2 = r2_vec[i]
    if (r1 <r2){
      gsint = ts_amzn[r1:r2]
      gsint1 = print(gsint[1])
      gsadf = radf(gsint,lag=1)
      statgsadf = gsadf$adf
      vec = c(vec, statgsadf)
      vecc1 = c(vecc1, r1)
      vecc2 = c(vecc2, r2)
  }
  }
}

amzn_bind = cbind(vec, vecc1, vecc2)
vec
max(vec)
which.max(vec)
amzn_bind[which.max(vec),]





#Density plot


adf_dens = density(tstat1)
plot(adf_dens, col = "blue" , ylim = range(0:1), xlim=range(-5:5), main = "", xlab = "")
lines(sadf_dens, add=T, col = "red")
lines(gsadf_dens, add=T, col ="green")
legend("topright", legend = c("ADF", "SADF", "GSADF"),  text.col = c("blue","red","green" ) )


# Gross profit

amazon_profit <- read_delim("amazon_profit.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE, 
                            skip = 1)


head(amazon_profit)

gross_amzn = amazon_profit$`(Millions of US $)`

gross_amzn_ts = as.ts(gross_amzn)
plot(gross_amzn_ts, type="l", ylab = "US $", xlab = "År fra 2005")


rep_gross = rep(c(gross_amzn_ts), each = 12)

plot(rep_gross, type="l", ylab = "US $", xlab = "År fra 2005")


ts_amzn=  as.ts(close_amzn)  


ts_index=c()

for (i in 1:length(ts_amzn)-1){
  ts_index[i] = (ts_amzn[1+i]-ts_amzn[1])/ts_amzn[1]*100
}




ts_amzn_index = ts_index

plot(ts_amzn_index , type="l", ylab = "Procentvis ændring", xlab = "Måneder fra 2005")

indexnew = c(100,100+ts_index)

ts_index_g=c()

for (i in 1:length(gross_amzn_ts)-1){
  ts_index_g[i] = (gross_amzn_ts[1+i]-gross_amzn_ts[1])/gross_amzn_ts[1]*100
}


ts_index_g
indexnew_g = c(100,100+ts_index_g)

rep_gross = rep(c(indexnew_g ), each = 12)



plot(rep_gross, type="l", ylab = "Procentvis ændring", xlab = "Måneder fra 2005")
lines(indexnew, add=T, col = "red")
legend("topleft", legend = c("Bruttofortjeneste", "AMZN"),  text.col = c("black","red" ) )




