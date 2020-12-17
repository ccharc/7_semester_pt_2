library(readxl)
library(vars)
library(imputeTS)
library(forecast)
library(tseries)
library(stats)
library(strucchange)
library(readr)
library(exuber)

ctas <- read_csv("CTAS.csv")
ctas_div <- read_csv("CTAS-2.csv")

head(ctas)
tail(ctas)
head(ctas_div)


close_ctas = ctas$Close
close_ctas_div = ctas_div$Dividends

plot(close_ctas, type="l", ylab = "Aktieindeks Cintas", xlab = "Måneder fra 2010")  
plot(close_ctas_div, type="l", ylab = "Dividener Cintas", xlab = "År fra 2010")  


ts_ctas =  as.ts(close_ctas)  
ts_ctas_div =  as.ts(close_ctas_div)  

length(ts_ctas)

adf_ctas = adf.test(ts_ctas, alternative = "stationary", k=1)
adf_ctas_div = adf.test(ts_ctas_div, alternative = "stationary", k=1)

#SADF

sadff = sadf(2000,240)
sadf_dens = density(sadff$value)
plot(sadf_dens)

#sadf = sadf_gsadf(ts_amzn, adflag=1, mflag = 1, IC = 1)

match(sadf$sadf, sadf$bsadfs)

ts_l = length(ts_ctas)
sadfstat11 = c()
rw_vec =c(seq(from = 12, to = 131, by=12),131)
r0 = 30 
sadfstat11 = c()
for (rw in rw_vec ){
  r1 =  ts_l - rw
  r2 = ts_l
  ts_ctas_int = ts_ctas[r1:r2]
  sadff1 = radf(ts_ctas_int, lag=1)
  sadfstat = sadff1$adf
  sadfstat1 = print(sadfstat)
  sadfstat11 = c(sadfstat11 ,sadfstat1)
}
sadfstat11
max(sadfstat11)
which.max(sadfstat11)

#GSADF

gsadff = gsadf(500,240, adflag = 1, mflag = 1)
gsadf_dens = density(gsadff$value)
plot(gsadf_dens)

#gsadf = sadf_gsadf(ts_amzn, adflag=1, mflag = 1, IC = 1)

ts_amzn1 = ts_amzn[150:240]
plot(ts_amzn1, type = "l")


r1_vec = seq(from =0, to = 210, by = 30)
r2_vec = seq(from=30, to= 240, by = 30)
vec = c()
gsint_vec = c()
vec1 = c()
for (j in 1:length(r1_vec)){
  r1 = r1_vec[j]
  for (i in 1:length(r2_vec)){
    r2 = r2_vec[i]
    if (r1 <r2){
      gsint = ts_amzn[r1:r2]
      gsint1 = print(gsint[1])
      gsadf = radf(gsint,lag=1)
      statgsadf = gsadf$adf
      vec = c(vec, statgsadf)
    }
  }
}

plot(density(vec))
vec
max(vec)
which.max(vec)

