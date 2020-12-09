library(readxl)
library(vars)
library(imputeTS)
library(forecast)
library(tseries)
library(stats)
library(strucchange)
library(readr)


amzn <- read_csv("AMZN-2.csv")
head(amzn)

close_amzn = amzn$Close

plot(close_amzn, type="l", ylab = "Aktieindeks Amazon", xlab = "Måneder fra 2000")  

ts_amzn =  as.ts(close_amzn)  


adf = adf.test(ts_amzn, alternative = "stationary", k=1)

#SADF

sadff = sadf(2000,240)
sadf_dens = density(sadff$value)
plot(sadf_dens)

#sadf = sadf_gsadf(ts_amzn, adflag=1, mflag = 1, IC = 1)

match(sadf$sadf, sadf$bsadfs)

ts_l = length(ts_amzn)
sadfstat11 = c()
rw_vec =c(30, 60, 90, 120, 150, 180, 210) 
r0 = 30 

for (rw in rw_vec ){
  r1 = ts_l - rw
  r2 = ts_l
  ts_amzn_int = ts_amzn[r1:r2]
  sadff1 = adf.test(ts_amzn_int, alternative = "stationary", k=1)
  sadfstat = sadff1$statistic
  sadfstat1 = print(sadfstat)
  sadfstat11[rw] = sadfstat1
}

sadfstat11 = sadfstat11[!is.na(sadfstat11)]
supadf = max(sadfstat11)
window_sup = which.max(sadfstat11)




#GSADF

gsadff = gsadf(500,240, adflag = 1, mflag = 1)
gsadf_dens = density(gsadff$value)
plot(gsadf_dens)

#gsadf = sadf_gsadf(ts_amzn, adflag=1, mflag = 1, IC = 1)

ts_l = length(ts_amzn)
gsadfstat11 = c()
rw_vec =c(30, 60, 90, 120, 150, 180, 210,240) 
r0 = 30

ts_amzn_int2 = c()
gsadfstat2 = c()
statgsadf = c()


for (i in 1:length(r2_vec)){
  r11 = r2_vec[i]
  for (j in 1:length(r2_vec)){
    if (r11 != r22){
    r22 = r2_vec[j]
    ts_amzn_intt = ts_amzn[r11:r22]
    ts_amzn_int2 = print(ts_amzn_intt)
  }
  #gsadff2 = adf.test(ts_amzn_intt , k=1)
}
}
length(ts_amzn_int2)





length(gsadfstat1)

for (i in 1:length(r1_vec)){
  r11 = r1_vec[i]
  r11_vec[i] = r11 
  for (j in 1:length(r2_vec)){
    r22 = r2_vec[j]
    r22_vec[j] = r22
    ts_amzn_intt = ts_amzn[r11:r22]
    ts_amzn_int2 = print(ts_amzn_intt)
  }
  }


r1_vec = c(1, 30, 60, 90, 120, 150, 180, 210) 
r2_vec = c(30, 60, 90, 120, 150, 180, 210,240) 
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
      gsadf = adf.test(gsint,alternative= "e", k=1)
      statgsadf = gsadf$statistic
      vec = c(vec, statgsadf)
  }
  }
}


vec
max(vec)
which.max(vec)


plot(ts_amzn[1:210], type = "l")




adf.test(ts_amzn[1:210], alternative = "e", k=1)



















